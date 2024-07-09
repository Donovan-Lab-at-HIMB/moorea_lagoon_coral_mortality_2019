genus <- "Pocillopora"
size_class <- "all"
response <- "Percent_dead"
mod_version <- "Nsubmodel" #what the data is going to be called when exported
mod_version_jags <- "binom_hierarchical.jags"

library(dplyr)
library(rjags)
library(parallel)

data_expanded <- read.csv("data/final_model_inputs/data_expanded.csv")
turb_N_allYears <- read.csv("data/final_model_inputs/turb_all_years.csv")

# prep model input --------------------------------------------------------
data_expanded$response <- data_expanded[,response]

sev_mod_data <- data_expanded %>% 
  filter(Genus==genus) %>% 
  filter(!is.na(response)) %>% 
  filter(!is.na(Depth)) %>% 
  filter(!is.na(Size.class)) 

# REMOVE FRINGING REEF DATA
sev_mod_data <- sev_mod_data %>% 
  filter(Habitat_go=='Lagoon')

# subset size class
if(is.numeric(size_class)==TRUE){sev_mod_data <- sev_mod_data %>% filter(Size.class==size_class)}
sev_mod_data$Size.class <- ifelse(sev_mod_data$Size.class==1|sev_mod_data$Size.class==2,1,sev_mod_data$Size.class)

# remove 6 sites that aren't in the turbinara compiled data
sev_mod_data <- sev_mod_data %>% 
  filter(!grepl("LTER",Site))

# set up response
sev_mod_data$y <- sev_mod_data[,response]
sev_mod_data <- sev_mod_data %>% filter(y > 0)
sev_mod_data$y <- ifelse(sev_mod_data$y > 75, 1, 0)

y_p<-sev_mod_data$y
summary(y_p)


# colony level predictors
x_colony <- as.matrix(data.frame(sev_mod_data$Size.class,sev_mod_data$Depth))
for(i in 1:ncol(x_colony)) x_colony[,i] <- scale(x_colony[,i])[,1]
if(is.numeric(size_class)==TRUE){x_colony[,1] <- 1} # SET SIZE TO ONE SINCE ONLY ONE SIZE
B <- ncol(x_colony)


# point level preds
site_preds <- sev_mod_data %>% group_by(site_index) %>% summarise(cumtemp=unique(max_heatstress),habitat=unique(Habitat_go),coast=unique(Island_shore))
site_preds$cumtemp_scale <- scale(site_preds$cumtemp)[,1]
site_preds$site_index_reset <- seq(1:nrow(site_preds))
write.csv(site_preds,paste0("model_out/",mod_version,"/",response,"/sev","_",genus,"_",size_class,"Size_site_data_in.csv"))

# rest turb data index
turb_subset <- turb_N_allYears[turb_N_allYears$site_index %in% site_preds$site_index,]
turb_subset <- left_join(turb_subset, site_preds[c('site_index','site_index_reset')],by='site_index')
write.csv(turb_subset,paste0("model_out/",mod_version,"/",response,"/sev","_",genus,"_",size_class,"Size_turb_data_in.csv"))

# point
sev_mod_data <- left_join(sev_mod_data, site_preds[c('site_index','site_index_reset')],by='site_index')
point <- sev_mod_data$site_index_reset
nP <- length(unique(point))

write.csv(sev_mod_data,paste0("model_out/",mod_version,"/",response,"/sev","_",genus,"_",size_class,"Size_data_in.csv"))

# model parameters --------------------------------------------------------
# we don't know why these numbers are here, not mary's typical style. reminding ourselves what the dimensions of these should be
jd <- list(B=B, #2
           y=y_p, #[n]
           n=length(y_p), #3737
           X_colony=x_colony, #[n,B]
           point=point, #[n]
           P=length(unique(point)), #49
           #habitat=as.numeric(as.factor(as.character(site_preds$habitat))), #[n] (1 level)
           cum_heat=site_preds$cumtemp_scale, #[P]
           #K=length(unique(site_preds$habitat)), #1
           coast=as.numeric(as.factor(as.character(site_preds$coast))), #[P]
           M=3,
           prior.scale = 10,
           
           # turb sub model data inputs
           y_turb = scale(turb_subset$N)[,1],
           N = nrow(turb_subset),
           site_turb = turb_subset$site_index_reset
)


nXcol <- ncol(x_colony)

initFunc <- function(){return(list(
  beta_colony_p=rnorm(nXcol,0,1),
  sigma_point_p=runif(1,0,20),
  # sigma_habitat_p=runif(1,0,20),
  # sigma_coast_p=runif(1,0,1),
  beta_N =0,
  beta_cumheat=0,
  beta_cumheat_X_N=-1.5, #previously was commented out and  beta_cumheat_X_N=0,
  mu_turb_bar=rnorm(1,0,0.5),
  mu_turb_site=rnorm(nP,0,0.5),
  tau_turb_bar=rgamma(1,0.1,0.1),
  tau_turb=rgamma(1,0.1,0.1)
))}

n.adapt <- 1000; n.update <- 20000; n.iter <- 20000

# run model - prevalence --------------------------------------------------
# run chains in parallel
cl <- makeCluster(3) # this determines the number of chains, must be less than the number of cores on computer
# telling your computer to run this three different times on three different portions of your computer
# then paste them back together
# some thing has changed either in R or in JAGS and mary can no longer run jags models like this
# mary is getting exact same things 3 times. chains are perfectly correlated
# mary's current work around for more simple models, comment out line 112-121, 131-132, and then change number of chains to 3. posterior will be zmCore
clusterExport(cl, c('jd','n.adapt','n.update','n.iter','initFunc','nXcol','nP','mod_version_jags'))

out <- clusterEvalQ(cl,{
  library(rjags)
  tmod <- jags.model(mod_version_jags, 
                     data= jd, n.chains=1, n.adapt=n.adapt,inits = initFunc())
  update(tmod, n.iter = n.update)
  zmCore <- coda.samples(tmod, c(
    "beta_colony_p",'beta_cumheat','beta_N','beta_cumheat_X_N','beta_colony_depth',
    "pval.mean","y.new",'pval.sd','R2',
    "y_turb","y_turb_new","mu_turb_site","sigma_turb","mu_turb_bar","tau_turb","tau_turb_bar",
    'pval.cv','pval.fit','pval.min','pval.max',"mu_overall_p"
  ),n.iter=n.iter, n.thin=1)
  return(as.mcmc(zmCore))
})
zmPrp <- mcmc.list(out)
stopCluster(cl)


saveRDS(zmPrp, paste0('model_out/sev_',genus,'_',response,'_',size_class,'Size_',Sys.Date(),'_',mod_version,'.Rdata'))


# notes -------------------------------------------------------------------


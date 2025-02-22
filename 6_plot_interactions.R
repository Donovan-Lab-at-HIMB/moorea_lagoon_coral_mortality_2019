###------------------------------------------------------------------------#
# Effects of nitrogen enrichment on coral mortality depend on the intensity of heat stress
#  
# 6_plot_interactions
###------------------------------------------------------------------------#

# This script makes interaction plots

### Packages --------------------------------------------------------------#

library(rjags)
library(boot)
library(ggplot2)
library(dplyr)
library(tidyverse)

#### --------------------- Acropora Prevalence: Heat x N interaction ------------------

genus <- "Acropora"
size_class <-"all" 
response <- "Percent_dead"
mod_version <- "Nsubmodel"
mod_version_jags <- "binom_hierarchical.jags"
distgo <- "prev"
out_dir <- paste0("model_out/",mod_version,"/",response,"/")
mod_date <- "2025-02-21" # put the date you ran the model here

mod <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))

# make sure to change these for the correct size class model!
data_in <- read.csv(paste0(out_dir,distgo,"_Acropora_allSize_data_in.csv"))
site_data_in <- read.csv(paste0(out_dir,distgo,"_Acropora_allSize_site_data_in.csv"))

# run these for the all plot
#data_in <- read.csv(paste0(out_dir,distgo,"_Acropora_allSize_data_in.csv"))
#site_data_in <- read.csv(paste0(out_dir,distgo,"_Acropora_allSize_site_data_in.csv"))

# betas
grepgo <- grep('beta',colnames(mod[[1]]))
beta_post <- rbind(mod[[1]][,grepgo],mod[[2]][,grepgo],mod[[3]][,grepgo])
beta_matrix <- as.matrix(beta_post)

# habitat_intercept
grepgo <- grep('mu_overall_p',colnames(mod[[1]]))
intercept_post <- c(mod[[1]][,grepgo],mod[[2]][,grepgo],mod[[3]][,grepgo])
intercept_matrix <- as.matrix(intercept_post)

# turb mean
grepgo <- grep('mu_turb_site',colnames(mod[[1]]))
turb_post <- rbind(mod[[1]][,grepgo],mod[[2]][,grepgo],mod[[3]][,grepgo])
turb_matrix <- as.matrix(turb_post)
# write.csv(turb_matrix,paste0(out_dir,distgo,"_",genus,"_",size_class,"turb_posterior.csv"),row.names=F)
turb_means <- data.frame(site_index_reset=seq(1:ncol(turb_matrix)), turb_mean=colMeans(turb_matrix)) # note scaled
turb_means$turb_mean_s <- 0.5034365 + turb_means$turb_mean*0.1140911  # transform back to original scale

# unique cumtemp vals
cumtemp_unique <- tibble::tribble(
  ~cumtemp,     ~cumtemp_scale,
  4.94457039421061,  -0.99473896609367,
  5.90847387086241,  0.289768671410121,
  7.12,   1.90426078976857,
  5.98,  0.385085123092043,
  5.19, -0.667676961710113,
  5.13,  -0.74763357574572
)
cumtemp_unique <- arrange(cumtemp_unique, cumtemp)

# n_temp_combo <- tidyr::expand_grid(N=turb_means$turb_mean,cumtemp=cumtemp_unique$cumtemp_scale) 
### change this so its the unique combos we observe in the data
data_in <- dplyr::left_join(data_in, turb_means, by='site_index_reset')
data_in <- dplyr::left_join(data_in, site_data_in[c("site_index_reset","cumtemp_scale")], by='site_index_reset')
n_temp_combo <- data_in %>% distinct(site_index_reset,turb_mean,cumtemp_scale) %>% rename(N=turb_mean,cumtemp=cumtemp_scale)

# calculate posterior
post_out <- data.frame(n_temp_combo, mean=NA,up=NA,down=NA) # create empty dataframe to fill results

for(i in 1:nrow(post_out)){
  temp <- boot::inv.logit(intercept_post + beta_matrix[,'beta_cumheat']*post_out$cumtemp[i] + beta_matrix[,'beta_N']*post_out$N[i] + beta_matrix[,'beta_cumheat_X_N']*post_out$cumtemp[i]*post_out$N[i])
  post_out$mean[i] <- mean(temp)
  post_out$up[i] <- quantile(temp,0.90) #80% credible interval
  post_out$down[i] <- quantile(temp,0.10)
}

ggplot(data=post_out) + 
  geom_errorbar(aes(x=N,ymin=down,ymax=up)) +
  geom_point(aes(x=N,y=mean)) +
  facet_grid(cols=vars(cumtemp)) +
  ylab("Prevalence") +
  labs(title="",subtitle = 'Heat Stress')

# lump heat stress
data_in$cumtemp_scale_cat <- ifelse(data_in$cumtemp_scale < 0, median(cumtemp_unique$cumtemp_scale[cumtemp_unique$cumtemp_scale < 0]), NA)
data_in$cumtemp_scale_cat <- ifelse(data_in$cumtemp_scale > 0 & data_in$cumtemp_scale < 1.8, median(cumtemp_unique$cumtemp_scale[cumtemp_unique$cumtemp_scale > 0 & cumtemp_unique$cumtemp_scale < 1.8]), data_in$cumtemp_scale_cat)
data_in$cumtemp_scale_cat <- ifelse(data_in$cumtemp_scale > 1.8, 1.9042608, data_in$cumtemp_scale_cat)
data_in %>% group_by(cumtemp_scale_cat) %>% summarise(min(cumtemp_scale),max(cumtemp_scale)) %>% ungroup()

n_temp_combo <- data_in %>% distinct(site_index_reset,turb_mean,cumtemp_scale_cat) %>% rename(N=turb_mean,cumtemp=cumtemp_scale_cat)

# calculate posterior
post_out <- data.frame(n_temp_combo, mean=NA,up=NA,down=NA)

for(i in 1:nrow(post_out)){
  temp <- boot::inv.logit(intercept_post + beta_matrix[,'beta_cumheat']*post_out$cumtemp[i] + beta_matrix[,'beta_N']*post_out$N[i] + beta_matrix[,'beta_cumheat_X_N']*post_out$cumtemp[i]*post_out$N[i])
  post_out$mean[i] <- mean(temp)
  post_out$up[i] <- quantile(temp,0.90)
  post_out$down[i] <- quantile(temp,0.10)
}
#
# post_out <- post_out %>% mutate(cat = recode(cumtemp, "-0.7476336" = "1_low", "1.9042608" = "3_high", "0.3850851" = "2_moderate"))
post_out$turb_mean_s <- 0.5034365 + post_out$N*0.1140911  # transform back to original scale
ggplot(data=post_out) +
  geom_errorbar(aes(x=turb_mean_s,ymin=down,ymax=up)) +
  geom_point(aes(x=turb_mean_s,y=mean)) +
  facet_grid(cols=vars(cumtemp)) +
  ylab("Prevalence") +
  labs(title="",subtitle = 'Heat Stress')

# resize points to match number of corals
# raw_out <- data_in %>% group_by(site_index_reset) %>% summarise(n=length(response),raw_mean=mean(response)/100) %>% ungroup()
data_in$y <- ifelse(data_in$response > 0, 1, 0)
raw_out <- data_in %>% group_by(site_index_reset) %>% summarise(n=length(response),raw_mean=mean(y)) %>% ungroup()
post_out <- post_out %>% left_join(raw_out, by='site_index_reset')

#creating names for cumulative temp variable
cumtemp_names <- c(
  `-0.74763357574572` = "Low heat stress",
  `0.337426897251082` = "Moderate heat stress",
  `1.9042608`= "High heat stress"
)

# "#055C9D", "#189AB4", "#75E6DA", "black"

acr_prev_allSize<-ggplot(data=post_out) + 
  geom_point(aes(x=turb_mean_s,y=raw_mean, size=n), pch=21, fill="#023962", alpha=0.5, stroke=NA) +
  scale_size(name="Number of corals", breaks=c(10,30,50), labels=c("10","30","50"))+
  geom_errorbar(aes(x=turb_mean_s,ymin=down,ymax=up)) +
  geom_point(aes(x=turb_mean_s,y=mean)) +
  facet_grid(cols=vars(cumtemp), labeller = labeller(cumtemp  = as_labeller(cumtemp_names))) +
  ylab("Mortality prevalence") +
  xlab(expression(paste("Nitrogen enrichment (% N in", italic("T. ornata"), ")")))+
  scale_x_continuous(breaks=c(0.45,0.5,0.55), limits=c(0.44,0.59))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=14))+
  theme(strip.background = element_rect(color="white", fill="white", linewidth=1.5))+ #facet background
  theme(strip.text.x = element_text(size = 14))+ #facet text size
  theme(aspect.ratio = 3/2)
ggsave("figs/acr_prev_allSize_2.pdf", width=8, height=5, units="in")

#### --------------------- Pocillopora Severity: Heat x N interaction ------------------

genus <- "Pocillopora"
size_class <-"all" 
response <- "Percent_dead"
mod_version <- "Nsubmodel"
mod_version_jags <- "binom_hierarchical.jags"
distgo <- "sev"
out_dir <- paste0("model_out/",mod_version,"/",response,"/")
mod_date <- "2024-05-24"

mod <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))

data_in <- read.csv(paste0(out_dir,distgo,"_Pocillopora_allSize_data_in.csv"))
site_data_in <- read.csv(paste0(out_dir,distgo,"_Pocillopora_allSize_site_data_in.csv"))

# run these for the all plot
#data_in <- read.csv(paste0(out_dir,distgo,"_Acropora_allSize_data_in.csv"))
#site_data_in <- read.csv(paste0(out_dir,distgo,"_Acropora_allSize_site_data_in.csv"))

# betas
grepgo <- grep('beta',colnames(mod[[1]]))
beta_post <- rbind(mod[[1]][,grepgo],mod[[2]][,grepgo],mod[[3]][,grepgo])
beta_matrix <- as.matrix(beta_post)

# habitat_intercept
grepgo <- grep('mu_overall_p',colnames(mod[[1]]))
intercept_post <- c(mod[[1]][,grepgo],mod[[2]][,grepgo],mod[[3]][,grepgo])
intercept_matrix <- as.matrix(intercept_post)

# turb mean
grepgo <- grep('mu_turb_site',colnames(mod[[1]]))
turb_post <- rbind(mod[[1]][,grepgo],mod[[2]][,grepgo],mod[[3]][,grepgo])
turb_matrix <- as.matrix(turb_post)
# write.csv(turb_matrix,paste0(out_dir,distgo,"_",genus,"_",size_class,"turb_posterior.csv"),row.names=F)
turb_means <- data.frame(site_index_reset=seq(1:ncol(turb_matrix)), turb_mean=colMeans(turb_matrix)) # note scaled
turb_means$turb_mean_s <- 0.5034365 + turb_means$turb_mean*0.1140911  # transform back to original scale

# unique cumtemp vals
cumtemp_unique <- tibble::tribble(
  ~cumtemp,     ~cumtemp_scale,
  4.94457039421061,  -0.99473896609367,
  5.90847387086241,  0.289768671410121,
  7.12,   1.90426078976857,
  5.98,  0.385085123092043,
  5.19, -0.667676961710113,
  5.13,  -0.74763357574572
)
cumtemp_unique <- arrange(cumtemp_unique, cumtemp)

# n_temp_combo <- tidyr::expand_grid(N=turb_means$turb_mean,cumtemp=cumtemp_unique$cumtemp_scale) 
### change this so its the unique combos we observe in the data
data_in <- dplyr::left_join(data_in, turb_means, by='site_index_reset')
data_in <- dplyr::left_join(data_in, site_data_in[c("site_index_reset","cumtemp_scale")], by='site_index_reset')
n_temp_combo <- data_in %>% distinct(site_index_reset,turb_mean,cumtemp_scale) %>% rename(N=turb_mean,cumtemp=cumtemp_scale)

# calculate posterior
post_out <- data.frame(n_temp_combo, mean=NA,up=NA,down=NA) # create empty dataframe to fill results

for(i in 1:nrow(post_out)){
  temp <- boot::inv.logit(intercept_post + beta_matrix[,'beta_cumheat']*post_out$cumtemp[i] + beta_matrix[,'beta_N']*post_out$N[i] + beta_matrix[,'beta_cumheat_X_N']*post_out$cumtemp[i]*post_out$N[i])
  post_out$mean[i] <- mean(temp)
  post_out$up[i] <- quantile(temp,0.90) #80% credible interval
  post_out$down[i] <- quantile(temp,0.10)
}

ggplot(data=post_out) + 
  geom_errorbar(aes(x=N,ymin=down,ymax=up)) +
  geom_point(aes(x=N,y=mean)) +
  facet_grid(cols=vars(cumtemp)) +
  ylab("Severity") +
  labs(title="",subtitle = 'Heat Stress')

# lump heat stress
data_in$cumtemp_scale_cat <- ifelse(data_in$cumtemp_scale < 0, median(cumtemp_unique$cumtemp_scale[cumtemp_unique$cumtemp_scale < 0]), NA)
data_in$cumtemp_scale_cat <- ifelse(data_in$cumtemp_scale > 0 & data_in$cumtemp_scale < 1.8, median(cumtemp_unique$cumtemp_scale[cumtemp_unique$cumtemp_scale > 0 & cumtemp_unique$cumtemp_scale < 1.8]), data_in$cumtemp_scale_cat)
data_in$cumtemp_scale_cat <- ifelse(data_in$cumtemp_scale > 1.8, 1.9042608, data_in$cumtemp_scale_cat)
data_in %>% group_by(cumtemp_scale_cat) %>% summarise(min(cumtemp_scale),max(cumtemp_scale)) %>% ungroup()

n_temp_combo <- data_in %>% distinct(site_index_reset,turb_mean,cumtemp_scale_cat) %>% rename(N=turb_mean,cumtemp=cumtemp_scale_cat)

# calculate posterior
post_out <- data.frame(n_temp_combo, mean=NA,up=NA,down=NA)

for(i in 1:nrow(post_out)){
  temp <- boot::inv.logit(intercept_post + beta_matrix[,'beta_cumheat']*post_out$cumtemp[i] + beta_matrix[,'beta_N']*post_out$N[i] + beta_matrix[,'beta_cumheat_X_N']*post_out$cumtemp[i]*post_out$N[i])
  post_out$mean[i] <- mean(temp)
  post_out$up[i] <- quantile(temp,0.90)
  post_out$down[i] <- quantile(temp,0.10)
}
#
# post_out <- post_out %>% mutate(cat = recode(cumtemp, "-0.7476336" = "1_low", "1.9042608" = "3_high", "0.3850851" = "2_moderate"))
post_out$turb_mean_s <- 0.5034365 + post_out$N*0.1140911  # transform back to original scale
ggplot(data=post_out) +
  geom_errorbar(aes(x=turb_mean_s,ymin=down,ymax=up)) +
  geom_point(aes(x=turb_mean_s,y=mean)) +
  facet_grid(cols=vars(cumtemp)) +
  ylab("Prevalence") +
  labs(title="",subtitle = 'Heat Stress')

# resize points to match number of corals
# raw_out <- data_in %>% group_by(site_index_reset) %>% summarise(n=length(response),raw_mean=mean(response)/100) %>% ungroup()
#data_in$y <- ifelse(data_in$response > 75, 1, 0)
raw_out <- data_in %>% group_by(site_index_reset) %>% summarise(n=length(response),raw_mean=mean(y)) %>% ungroup()
post_out <- post_out %>% left_join(raw_out, by='site_index_reset')

#creating names for cumulative temp variable
cumtemp_names <- c(
  `-0.74763357574572` = "Low heat stress",
  `0.337426897251082` = "Moderate heat stress",
  `1.9042608`= "High heat stress"
)



poc_sev_int<-ggplot(data=post_out) + 
  geom_point(aes(x=turb_mean_s,y=(raw_mean),size=n), pch=21, fill="#005215", alpha=0.5, stroke=NA) +
  scale_size(breaks=c(10,20,30), labels=c("10","20","30"), name="Number of corals")+
  geom_errorbar(aes(x=turb_mean_s,ymin=down,ymax=up)) +
  geom_point(aes(x=turb_mean_s,y=mean)) +
  facet_grid(cols=vars(cumtemp), labeller = labeller(cumtemp  = as_labeller(cumtemp_names))) +
  ylab("Mortality severity") +
  xlab(expression(paste("Nitrogen enrichment (% N in", italic("T. ornata"), ")")))+
  scale_x_continuous(breaks=c(0.45,0.5,0.55), limits=c(0.44,0.59))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=14))+
  theme(strip.background = element_rect(color="white", fill="white", linewidth=1.5))+ #facet background
  theme(strip.text.x = element_text(size = 14))+ #facet text size
  theme(aspect.ratio = 3/2)
ggsave("figs/poc_sev_int_2.pdf", width=8, height=5, units="in")

### ------------- Figure S9: Interaction between nitrogen and heat stress for Acropora, split by size class -----------

genus <- "Acropora"
size_class <- 3
# change the number on line 287 and re-run the code below to make the plot for each size class
# those size classes are: 1, 3, 4
response <- "Percent_dead"
mod_version <- "Nsubmodel"
mod_version_jags <- "binom_hierarchical.jags"
distgo <- "prev"
out_dir <- paste0("model_out/",mod_version,"/",response,"/")
mod_date <- "2024-05-24"

mod <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))

# make sure to change these for the correct size class model .csv file! those file names are:
# "_Acropora_1Size_data_in.csv", "_Acropora_3Size_data_in.csv", "_Acropora_4Size_data_in.csv"
# remember that size classes 1 and 2 from the data file get combined to be called size class 1
data_in <- read.csv(paste0(out_dir,distgo,"_Acropora_3Size_data_in.csv"))
site_data_in <- read.csv(paste0(out_dir,distgo,"_Acropora_3Size_site_data_in.csv"))

# betas
grepgo <- grep('beta',colnames(mod[[1]]))
beta_post <- rbind(mod[[1]][,grepgo],mod[[2]][,grepgo],mod[[3]][,grepgo])
beta_matrix <- as.matrix(beta_post)

# habitat_intercept
grepgo <- grep('mu_overall_p',colnames(mod[[1]]))
intercept_post <- c(mod[[1]][,grepgo],mod[[2]][,grepgo],mod[[3]][,grepgo])
intercept_matrix <- as.matrix(intercept_post)

# turb mean
grepgo <- grep('mu_turb_site',colnames(mod[[1]]))
turb_post <- rbind(mod[[1]][,grepgo],mod[[2]][,grepgo],mod[[3]][,grepgo])
turb_matrix <- as.matrix(turb_post)
# write.csv(turb_matrix,paste0(out_dir,distgo,"_",genus,"_",size_class,"turb_posterior.csv"),row.names=F)
turb_means <- data.frame(site_index_reset=seq(1:ncol(turb_matrix)), turb_mean=colMeans(turb_matrix)) # note scaled
turb_means$turb_mean_s <- 0.5034365 + turb_means$turb_mean*0.1140911  # transform back to original scale

# unique cumtemp vals
cumtemp_unique <- tibble::tribble(
  ~cumtemp,     ~cumtemp_scale,
  4.94457039421061,  -0.99473896609367,
  5.90847387086241,  0.289768671410121,
  7.12,   1.90426078976857,
  5.98,  0.385085123092043,
  5.19, -0.667676961710113,
  5.13,  -0.74763357574572
)
cumtemp_unique <- arrange(cumtemp_unique, cumtemp)

# n_temp_combo <- tidyr::expand_grid(N=turb_means$turb_mean,cumtemp=cumtemp_unique$cumtemp_scale) 
### change this so its the unique combos we observe in the data
data_in <- dplyr::left_join(data_in, turb_means, by='site_index_reset')
data_in <- dplyr::left_join(data_in, site_data_in[c("site_index_reset","cumtemp_scale")], by='site_index_reset')
n_temp_combo <- data_in %>% distinct(site_index_reset,turb_mean,cumtemp_scale) %>% rename(N=turb_mean,cumtemp=cumtemp_scale)

# calculate posterior
post_out <- data.frame(n_temp_combo, mean=NA,up=NA,down=NA) # create empty dataframe to fill results

for(i in 1:nrow(post_out)){
  temp <- boot::inv.logit(intercept_post + beta_matrix[,'beta_cumheat']*post_out$cumtemp[i] + beta_matrix[,'beta_N']*post_out$N[i] + beta_matrix[,'beta_cumheat_X_N']*post_out$cumtemp[i]*post_out$N[i])
  post_out$mean[i] <- mean(temp)
  post_out$up[i] <- quantile(temp,0.90) #80% credible interval
  post_out$down[i] <- quantile(temp,0.10)
}

ggplot(data=post_out) + 
  geom_errorbar(aes(x=N,ymin=down,ymax=up)) +
  geom_point(aes(x=N,y=mean)) +
  facet_grid(cols=vars(cumtemp)) +
  ylab("Prevalence") +
  labs(title="",subtitle = 'Heat Stress')

# lump heat stress
data_in$cumtemp_scale_cat <- ifelse(data_in$cumtemp_scale < 0, median(cumtemp_unique$cumtemp_scale[cumtemp_unique$cumtemp_scale < 0]), NA)
data_in$cumtemp_scale_cat <- ifelse(data_in$cumtemp_scale > 0 & data_in$cumtemp_scale < 1.8, median(cumtemp_unique$cumtemp_scale[cumtemp_unique$cumtemp_scale > 0 & cumtemp_unique$cumtemp_scale < 1.8]), data_in$cumtemp_scale_cat)
data_in$cumtemp_scale_cat <- ifelse(data_in$cumtemp_scale > 1.8, 1.9042608, data_in$cumtemp_scale_cat)
data_in %>% group_by(cumtemp_scale_cat) %>% summarise(min(cumtemp_scale),max(cumtemp_scale)) %>% ungroup()

n_temp_combo <- data_in %>% distinct(site_index_reset,turb_mean,cumtemp_scale_cat) %>% rename(N=turb_mean,cumtemp=cumtemp_scale_cat)

# calculate posterior
post_out <- data.frame(n_temp_combo, mean=NA,up=NA,down=NA)

for(i in 1:nrow(post_out)){
  temp <- boot::inv.logit(intercept_post + beta_matrix[,'beta_cumheat']*post_out$cumtemp[i] + beta_matrix[,'beta_N']*post_out$N[i] + beta_matrix[,'beta_cumheat_X_N']*post_out$cumtemp[i]*post_out$N[i])
  post_out$mean[i] <- mean(temp)
  post_out$up[i] <- quantile(temp,0.90)
  post_out$down[i] <- quantile(temp,0.10)
}
#
# post_out <- post_out %>% mutate(cat = recode(cumtemp, "-0.7476336" = "1_low", "1.9042608" = "3_high", "0.3850851" = "2_moderate"))
post_out$turb_mean_s <- 0.5034365 + post_out$N*0.1140911  # transform back to original scale
ggplot(data=post_out) +
  geom_errorbar(aes(x=turb_mean_s,ymin=down,ymax=up)) +
  geom_point(aes(x=turb_mean_s,y=mean)) +
  facet_grid(cols=vars(cumtemp)) +
  ylab("Prevalence") +
  labs(title="",subtitle = 'Heat Stress')

# resize points to match number of corals
# raw_out <- data_in %>% group_by(site_index_reset) %>% summarise(n=length(response),raw_mean=mean(response)/100) %>% ungroup()
data_in$y <- ifelse(data_in$response > 0, 1, 0)
raw_out <- data_in %>% group_by(site_index_reset) %>% summarise(n=length(response),raw_mean=mean(y)) %>% ungroup()
post_out <- post_out %>% left_join(raw_out, by='site_index_reset')

#creating names for cumulative temp variable
cumtemp_names <- c(
  `-0.74763357574572` = "Low heat stress",
  `0.337426897251082` = "Moderate heat stress",
  `1.9042608`= "High heat stress"
)


acr_prev_1size<-ggplot(data=post_out) +
  geom_point(aes(x=turb_mean_s,y=raw_mean,size=n), pch=21, fill="#75E6DA", alpha=0.7, stroke=NA) +
  scale_size(breaks=c(10,20,30), labels=c("10","20","30"), name="Number of corals")+
  geom_errorbar(aes(x=turb_mean_s,ymin=down,ymax=up)) +
  geom_point(aes(x=turb_mean_s,y=mean)) +
  facet_grid(cols=vars(cumtemp), labeller = labeller(cumtemp  = as_labeller(cumtemp_names))) +
  ylab("Mortality prevalence") +
  xlab(expression(paste("Nitrogen enrichment (% N in", italic("T. ornata"), ")")))+
  scale_x_continuous(breaks=c(0.45,0.5,0.55), limits=c(0.44,0.59))+
  #labs(title="Heat Stress", size=16)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14),
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=14))+
  theme(strip.background = element_rect(color="white", fill="white", linewidth=1.5))+ #facet background
  theme(strip.text.x = element_text(size = 14))+ #facet text size
  theme(aspect.ratio = 3/2)


acr_prev_3size<-ggplot(data=post_out) +
  geom_point(aes(x=turb_mean_s,y=raw_mean,size=n), pch=21, fill="#189AB4", alpha=0.7, stroke=NA) +
  scale_size(breaks=c(5,15,25), labels=c("5","15","25"), name="Number of corals")+
  geom_errorbar(aes(x=turb_mean_s,ymin=down,ymax=up)) +
  geom_point(aes(x=turb_mean_s,y=mean)) +
  facet_grid(cols=vars(cumtemp), labeller = labeller(cumtemp  = as_labeller(cumtemp_names))) +
  ylab("Mortality prevalence") +
  xlab(expression(paste("Nitrogen enrichment (% N in", italic("T. ornata"), ")")))+
  scale_x_continuous(breaks=c(0.45,0.5,0.55), limits=c(0.44,0.59))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14),
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=14))+
  theme(strip.background = element_rect(color="white", fill="white", linewidth=1.5))+ #facet background
  theme(strip.text.x = element_text(size = 14))+ #facet text size
  theme(aspect.ratio = 3/2)

acr_prev_4size<-ggplot(data=post_out) +
  geom_point(aes(x=turb_mean_s,y=raw_mean,size=n), pch=21, fill="#055C9D", alpha=0.7, stroke=NA) +
  scale_size(breaks=c(4,8,12), labels=c("4","8","12"), name="Number of corals")+
  geom_errorbar(aes(x=turb_mean_s,ymin=down,ymax=up)) +
  geom_point(aes(x=turb_mean_s,y=mean)) +
  facet_grid(cols=vars(cumtemp), labeller = labeller(cumtemp  = as_labeller(cumtemp_names))) +
  ylab("Mortality prevalence") +
  xlab(expression(paste("Nitrogen enrichment (% N in", italic("T. ornata"), ")")))+
  scale_x_continuous(breaks=c(0.45,0.5,0.55), limits=c(0.44,0.59))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14),
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=14))+
  theme(strip.background = element_rect(color="white", fill="white", linewidth=1.5))+ #facet background
  theme(strip.text.x = element_text(size = 14))+ #facet text size
  theme(aspect.ratio = 3/2)


acr_int_3sizes<-cowplot::plot_grid(acr_prev_1size, acr_prev_3size, acr_prev_4size, labels=c('A','B','C'), ncol=1)
ggsave("figs/acr_int_3sizes.pdf", width=10, height=15, units="in")


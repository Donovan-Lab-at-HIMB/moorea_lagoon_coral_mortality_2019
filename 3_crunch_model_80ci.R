library(rjags)
# For each size class separately
for(i in c(1,3,4)){
  for(k in c("Pocillopora", "Acropora")){
    for(j in c("Percent_dead")){
      for(z in c("prev")){
      genus <- k
      size_class <- i
      response <- j
      distgo <- z
      mod_version <- "Nsubmodel"
      mod_version_jags <- paste0(z,"_hierarchical.jags")
      out_dir <- paste0("model_out/",mod_version,"/",response,"/")
      mod_date <- "2024-12-31"
      
      mod <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))
      mod_sum <- summary(mod)
      
      beta_quantiles <- mod_sum$quantiles[c('beta_cumheat','beta_N','beta_cumheat_X_N', 'beta_colony_depth'),]
      write.csv(cbind(rownames(beta_quantiles),beta_quantiles), 
                paste0(out_dir,distgo,"_",genus,"_",size_class,"Size_beta_quantiles.csv"),row.names=F)
     
      mod_check <- c('pval.mean','R2')
      mod_check <- mod_sum$statistics[mod_check,]
      write.csv(cbind(rownames(mod_check),mod_check), 
                paste0(out_dir,distgo,"_",genus,"_",size_class,"Size_mod_check.csv"),row.names=F)
      
      grepgo <- grep('mu_turb_site', colnames(mod[[1]]))
      turb_site_out <- summary(mcmc.list(mod[[1]][,grepgo],mod[[2]][,grepgo],mod[[3]][,grepgo]))[[2]]
      write.csv(cbind(rownames(turb_site_out),turb_site_out), 
                paste0(out_dir,distgo,"_",genus,"_",size_class,"Size_turb_site.csv"),row.names=F)
  
      grepgo <- grep('beta', colnames(mod[[1]]))
      gel_check <- gelman.diag(mcmc.list(
        mod[[1]][, grepgo],mod[[2]][, grepgo],mod[[3]][, grepgo]),multivariate = F)[[1]][,1]
      gel_check #will spit out to the console. or can do write.csv
      write.csv(cbind(colnames(gel_check),gel_check), 
                paste0(out_dir,distgo,"_",genus,"_",size_class,"Size_gel_check.csv"),row.names=T)
      
      grepgo <- grep('beta',colnames(zmPrp[[1]]))
      beta_80 <- data.frame(
        beta = rep(NA,length(grepgo)),
        beta_up80 = rep(NA,length(grepgo)),
        beta_down80 = rep(NA,length(grepgo)),
        beta_up95 = rep(NA,length(grepgo)),
        beta_down95 = rep(NA,length(grepgo))
      )
      for(q in 1:length(grepgo)){  ### just need a new letter here
        beta_80$beta[q] <- quantile(c(mod[[1]][,grepgo[q]],
                                      mod[[2]][,grepgo[q]],
                                      mod[[3]][,grepgo[q]]),0.5)
        beta_80$beta_up80[q] <- quantile(c(mod[[1]][,grepgo[q]],
                                           mod[[2]][,grepgo[q]],
                                           mod[[3]][,grepgo[q]]),0.90)
        beta_80$beta_down80[q] <- quantile(c(mod[[1]][,grepgo[q]],
                                             mod[[2]][,grepgo[q]],
                                             mod[[3]][,grepgo[q]]),0.10)
        beta_80$beta_up95[q] <- quantile(c(mod[[1]][,grepgo[q]],
                                           mod[[2]][,grepgo[q]],
                                           mod[[3]][,grepgo[q]]),0.975)
        beta_80$beta_down95[q] <- quantile(c(mod[[1]][,grepgo[q]],
                                             mod[[2]][,grepgo[q]], 
                                             mod[[3]][,grepgo[q]]),0.025)
      }
      beta_80$factor<-as.factor(grep('beta',colnames(zmPrp[[1]])))
      beta_80$factor_name<-as.factor(c("beta_N", "beta_colony_depth", "beta_cumheat", "beta_cumheat_X_N"))
      write.csv(beta_80, paste0(out_dir,distgo,"_",genus,"_",size_class,"Size_beta80_quantiles.csv"),row.names=T)
}
}
}
}


# for the model with all of the data
for(i in c("all")){
  for(k in c("Pocillopora", "Acropora")){
    for(j in c("Percent_dead")){
      for(z in c("prev","sev")){
        genus <- k
        size_class <- i
        response <- j
        distgo <- z
        mod_version <- "Nsubmodel"
        mod_version_jags <- paste0(z,"_hierarchical.jags")
        out_dir <- paste0("model_out/",mod_version,"/",response,"/")
        mod_date <- "2024-12-31"
        
        mod <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))
        mod_sum <- summary(mod)
        
        beta_quantiles <- mod_sum$quantiles[c('beta_cumheat','beta_N','beta_cumheat_X_N', 'beta_colony_depth'),]
        write.csv(cbind(rownames(beta_quantiles),beta_quantiles), 
                  paste0(out_dir,distgo,"_",genus,"_",size_class,"Size_beta_quantiles.csv"),row.names=F)
        
        mod_check <- c('pval.mean','R2')
        mod_check <- mod_sum$statistics[mod_check,]
        write.csv(cbind(rownames(mod_check),mod_check), 
                  paste0(out_dir,distgo,"_",genus,"_",size_class,"Size_mod_check.csv"),row.names=F)
        
        grepgo <- grep('mu_turb_site', colnames(mod[[1]]))
        turb_site_out <- summary(mcmc.list(mod[[1]][,grepgo],mod[[2]][,grepgo],mod[[3]][,grepgo]))[[2]]
        write.csv(cbind(rownames(turb_site_out),turb_site_out), 
                  paste0(out_dir,distgo,"_",genus,"_",size_class,"Size_turb_site.csv"),row.names=F)
        
        grepgo <- grep('beta', colnames(mod[[1]]))
        gel_check <- gelman.diag(mcmc.list(
          mod[[1]][, grepgo],mod[[2]][, grepgo],mod[[3]][, grepgo]),multivariate = F)[[1]][,1]
        gel_check #will spit out to the console. or can do write.csv
        write.csv(cbind(colnames(gel_check),gel_check), 
                  paste0(out_dir,distgo,"_",genus,"_",size_class,"Size_gel_check.csv"),row.names=T)
        
        grepgo <- grep('beta',colnames(zmPrp[[1]]))
        beta_80 <- data.frame(
          beta = rep(NA,length(grepgo)),
          beta_up80 = rep(NA,length(grepgo)),
          beta_down80 = rep(NA,length(grepgo)),
          beta_up95 = rep(NA,length(grepgo)),
          beta_down95 = rep(NA,length(grepgo))
        )
        for(q in 1:length(grepgo)){  ### just need a new letter here
          beta_80$beta[q] <- quantile(c(mod[[1]][,grepgo[q]],
                                        mod[[2]][,grepgo[q]],
                                        mod[[3]][,grepgo[q]]),0.5)
          beta_80$beta_up80[q] <- quantile(c(mod[[1]][,grepgo[q]],
                                             mod[[2]][,grepgo[q]],
                                             mod[[3]][,grepgo[q]]),0.90)
          beta_80$beta_down80[q] <- quantile(c(mod[[1]][,grepgo[q]],
                                               mod[[2]][,grepgo[q]],
                                               mod[[3]][,grepgo[q]]),0.10)
          beta_80$beta_up95[q] <- quantile(c(mod[[1]][,grepgo[q]],
                                             mod[[2]][,grepgo[q]],
                                             mod[[3]][,grepgo[q]]),0.975)
          beta_80$beta_down95[q] <- quantile(c(mod[[1]][,grepgo[q]],
                                               mod[[2]][,grepgo[q]], 
                                               mod[[3]][,grepgo[q]]),0.025)
        }
        beta_80$factor<-as.factor(grep('beta',colnames(zmPrp[[1]])))
        beta_80$factor_name<-as.factor(c("beta_N", "beta_colony_depth", "beta_cumheat", "beta_cumheat_X_N"))
        write.csv(beta_80, paste0(out_dir,distgo,"_",genus,"_",size_class,"Size_beta80_quantiles.csv"),row.names=F)
      }
    }
  }
}







# ctrl + shift + c to comment out multiple lines
#acropora all 
genus<-"Pocillopora"
size_class<-4
response<-"Percent_dead"
distgo<-"prev"
        mod_version <- "Nsubmodel"
        mod_version_jags <- "_hierarchical.jags"
        out_dir <- paste0("model_out/",mod_version,"/",response,"/")
        mod_date <- "2024-12-31"
        mod <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))
        grepgo <- grep('beta', colnames(mod[[1]]))
       gel_check <- gelman.diag(mcmc.list(mod[[1]][, grepgo],mod[[2]][, grepgo],mod[[3]][, grepgo]),multivariate = F)[[1]][,1]
      gel_check #will spit out to the console. or can do write.csv
#beta_N     beta_cumheat beta_cumheat_X_N 
#1.101796         1.006177         1.088186 

#grepgo
#colnames(mod[[1]])[1:10]      

#plot for beta_N       
plot(mcmc.list(mod[[1]][, grepgo[1]],mod[[2]][, grepgo[1]],mod[[3]][, grepgo[1]]))
#the mod[] tells it which chain we want (there are 3 and we want all 3), the grepgo[] specifies which beta we want

#plot for beta_cumheat   
plot(mcmc.list(mod[[1]][, grepgo[2]],mod[[2]][, grepgo[2]],mod[[3]][, grepgo[2]]))

#plot for beta_cumheat_X_N  
plot(mcmc.list(mod[[1]][, grepgo[3]],mod[[2]][, grepgo[3]],mod[[3]][, grepgo[3]]))

# Pocillopora all
genus<-"Pocillopora"
size_class<-4
response<-"Percent_dead"
distgo<-"binom"
mod_version <- "Nsubmodel"
mod_version_jags <- "_hierarchical.jags"
out_dir <- paste0("model_out/",mod_version,"/",response,"/")
mod_date <- "2024-12-31"
mod <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))
grepgo <- grep('beta', colnames(mod[[1]]))
gel_check <- gelman.diag(mcmc.list(
  mod[[1]][, grepgo],mod[[2]][, grepgo],mod[[3]][, grepgo]),multivariate = F)[[1]][,1]
gel_check #will spit out to the console. or can do write.csv
#beta_N     beta_cumheat beta_cumheat_X_N 
#1.003789         1.000136         1.000624 
plot(mcmc.list(mod[[1]][, grepgo[1]],mod[[2]][, grepgo[1]],mod[[3]][, grepgo[1]]))

# first 10 column names, see the three betas there. that can tell you how to grab things with grepgo

### ------ troubleshooting beta80
genus<-"Pocillopora"
size_class<-1
response<-"Percent_dead"
distgo<-"prev"
mod_version <- "Nsubmodel"
mod_version_jags <- "_hierarchical.jags"
out_dir <- paste0("model_out/",mod_version,"/",response,"/")
mod_date <- "2024-12-31"
mod <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))
grepgo <- grep('beta', colnames(mod[[1]]))
beta_80 <- data.frame(
  beta = rep(NA,length(grepgo)),
  beta_up80 = rep(NA,length(grepgo)),
  beta_down80 = rep(NA,length(grepgo)),
  beta_up95 = rep(NA,length(grepgo)),
  beta_down95 = rep(NA,length(grepgo))
)
### just need a new letter here
beta_80$beta <- quantile(c(zmPrp[[1]][,grepgo],zmPrp[[2]][,grepgo],zmPrp[[3]][,grepgo]),0.5)
  # beta_80$beta_up80 <- quantile(c(zmPrp[[1]][,grepgo[f]],zmPrp[[2]][,grepgo[f]],zmPrp[[3]][,grepgo[f]]),0.90)
  # beta_80$beta_down80 <- quantile(c(zmPrp[[1]][,grepgo[f]],zmPrp[[2]][,grepgo[f]],zmPrp[[3]][,grepgo[f]]),0.10)
  # beta_80$beta_up95 <- quantile(c(zmPrp[[1]][,grepgo[f]],zmPrp[[2]][,grepgo[f]],zmPrp[[3]][,grepgo[f]]),0.975)
  # beta_80$beta_down95 <- quantile(c(zmPrp[[1]][,grepgo[f]],zmPrp[[2]][,grepgo[f]], zmPrp[[3]][,grepgo[f]]),0.025)


#-0.731423285 -0.751034379 -0.748348808 -0.689241494 -0.695663640 -0.739367099 -0.617385803
# -----------------------------------------------------------------------------#
# Effects of nitrogen enrichment on coral mortality depend on the intensity of heat stress
#  
# 4_mod_checks
# -----------------------------------------------------------------------------#

# This script executes model checks for all models
# note to self, occasionally have some errors with fonts when running this script. 
# works to quit r and rerun.

# Packages --------------------------------------------------------------------

library(bayesplot)
library(dplyr)
library(tidyr)
library(ggplot2)

# Prevalence All Sizes  --------------------------------------------------------  

for(k in c("Pocillopora", "Acropora")){
  genus <- k
  size_class <- "all"
  response <- "Percent_dead"
  distgo <- "prev"
  mod_version <- "Nsubmodel"
  mod_version_jags <- "_hierarchical.jags"
  out_dir <- paste0("model_out/",mod_version,"/",response,"/")
  mod_date <- "2024-12-31"
  
  # getting the model 
  
  zmPrp <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))
  grepgo <- grep('y.new', colnames(zmPrp[[1]]))
  yrep <- rbind(zmPrp[[1]][,grepgo],zmPrp[[2]][,grepgo],zmPrp[[3]][,grepgo])
  dim(yrep)
  
  ## getting the data
  data_expanded <- read.csv("data/final_model_inputs/mortality_data.csv")
  data_expanded$response<-data_expanded[,response]
  
  prev_mod_data <- data_expanded %>% 
    filter(Genus==genus) %>% 
    filter(!is.na(response)) %>% 
    filter(!is.na(Depth)) %>% 
    filter(!is.na(Size.class)) 
  
  # subset size class
  if(is.numeric(size_class)==TRUE){prev_mod_data <- prev_mod_data %>% filter(Size.class==size_class)}
  prev_mod_data$Size.class <- ifelse(prev_mod_data$Size.class==1|prev_mod_data$Size.class==2,1,prev_mod_data$Size.class)
  
  # remove 6 sites that aren't in the turbinara compiled data
  prev_mod_data <- prev_mod_data %>% 
    filter(!grepl("LTER",Site))
  
  y <- prev_mod_data$response
  y_p <- ifelse(y > 0, 1, 0)
  y<-y_p
  summary(y)
  
  plotName <- paste0(distgo,"_",genus,"_",size_class,"Size_pp_dens")
  assign(plotName, pp_check(y, yrep[sample(1:60000,length(y)),],ppc_dens_overlay), envir = .GlobalEnv )
  #plot(prev_Pocillopora_allSize_pp_dens)
  
  plotName <- paste0(distgo,"_",genus,"_",size_class,"Size_pp_hist")
  assign(plotName, ppc_stat(y, yrep[sample(1:60000,length(y)),], stat = "mean"), envir = .GlobalEnv )
  #plot(sev_Pocillopora_allSize_pp_hist)
  
}

prev_poc_acr_all_hist<-cowplot::plot_grid(prev_Pocillopora_allSize_pp_hist, prev_Acropora_allSize_pp_hist, align = "VH") # both work
ggsave("figs/pp_checks/prev_poc_acr_all_hist.pdf", width=6, height=3, units="in")
prev_poc_acr_all_dens<-cowplot::plot_grid(prev_Pocillopora_allSize_pp_dens, prev_Acropora_allSize_pp_dens, align = "VH")
ggsave("figs/pp_checks/prev_poc_acr_all_dens.pdf", width=6, height=3, units="in")


# Prevalence 1Size  ------------------------------------------------------------

for(k in c("Pocillopora", "Acropora")){
  genus <- k
  size_class <- 1
  response <- "Percent_dead"
  distgo <- "prev"
  mod_version <- "Nsubmodel"
  mod_version_jags <- "_hierarchical.jags"
  out_dir <- paste0("model_out/",mod_version,"/",response,"/")
  mod_date <- "2024-12-31"
  
  # getting the model 
  
  zmPrp <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))
  grepgo <- grep('y.new', colnames(zmPrp[[1]]))
  yrep <- rbind(zmPrp[[1]][,grepgo],zmPrp[[2]][,grepgo],zmPrp[[3]][,grepgo])
  dim(yrep)
  
  ## getting the data
  data_expanded <- read.csv("data/final_model_inputs/mortality_data.csv")
  data_expanded$response<-data_expanded[,response]
  
  prev_mod_data <- data_expanded %>% 
    filter(Genus==genus) %>% 
    filter(!is.na(response)) %>% 
    filter(!is.na(Depth)) %>% 
    filter(!is.na(Size.class)) 
  
  # subset size class
  if(is.numeric(size_class)==TRUE){prev_mod_data <- prev_mod_data %>% filter(Size.class==size_class)}
  prev_mod_data$Size.class <- ifelse(prev_mod_data$Size.class==1|prev_mod_data$Size.class==2,1,prev_mod_data$Size.class)
  
  # remove 6 sites that aren't in the turbinara compiled data
  prev_mod_data <- prev_mod_data %>% 
    filter(!grepl("LTER",Site))
  
  y <- prev_mod_data$response
  y_p <- ifelse(y > 0, 1, 0)
  y<-y_p
  summary(y)
  
  plotName <- paste0(distgo,"_",genus,"_",size_class,"Size_pp_dens")
  assign(plotName, pp_check(y, yrep[sample(1:60000,length(y)),],ppc_dens_overlay), envir = .GlobalEnv )
  plot(prev_Pocillopora_1Size_pp_dens)
  
  plotName <- paste0(distgo,"_",genus,"_",size_class,"Size_pp_hist")
  assign(plotName, ppc_stat(y, yrep[sample(1:60000,length(y)),], stat = "mean"), envir = .GlobalEnv )
  
}

plot(prev_Pocillopora_1Size_pp_hist)
plot(prev_Pocillopora_1Size_pp_dens)
plot(prev_Acropora_1Size_pp_hist)
plot(prev_Acropora_1Size_pp_dens)

# Prevalence 3Size  ------------------------------------------------------------ 

for(k in c("Pocillopora", "Acropora")){
    genus <- k
    size_class <- 3
    response <- "Percent_dead"
    distgo <- "prev"
    mod_version <- "Nsubmodel"
    mod_version_jags <- "_hierarchical.jags"
    out_dir <- paste0("model_out/",mod_version,"/",response,"/")
    mod_date <- "2024-12-31"
    
    # getting the model 
    
    zmPrp <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))
    grepgo <- grep('y.new', colnames(zmPrp[[1]]))
    yrep <- rbind(zmPrp[[1]][,grepgo],zmPrp[[2]][,grepgo],zmPrp[[3]][,grepgo])
    dim(yrep)
    
    ## getting the data
    data_expanded <- read.csv("data/final_model_inputs/mortality_data.csv")
    data_expanded$response<-data_expanded[,response]
    
    prev_mod_data <- data_expanded %>% 
      filter(Genus==genus) %>% 
      filter(!is.na(response)) %>% 
      filter(!is.na(Depth)) %>% 
      filter(!is.na(Size.class)) 
    
    # subset size class
    if(is.numeric(size_class)==TRUE){prev_mod_data <- prev_mod_data %>% filter(Size.class==size_class)}
    prev_mod_data$Size.class <- ifelse(prev_mod_data$Size.class==1|prev_mod_data$Size.class==2,1,prev_mod_data$Size.class)
    
    # remove 6 sites that aren't in the turbinara compiled data
    prev_mod_data <- prev_mod_data %>% 
      filter(!grepl("LTER",Site))
    
    y <- prev_mod_data$response
    y_p <- ifelse(y > 0, 1, 0)
    y<-y_p
    summary(y)
    
    plotName <- paste0(distgo,"_",genus,"_",size_class,"Size_pp_dens")
    assign(plotName, pp_check(y, yrep[sample(1:60000,length(y)),],ppc_dens_overlay), envir = .GlobalEnv )
    #plot(prev_Pocillopora_3Size_pp_dens)
    
    plotName <- paste0(distgo,"_",genus,"_",size_class,"Size_pp_hist")
    assign(plotName, ppc_stat(y, yrep[sample(1:60000,length(y)),], stat = "mean"), envir = .GlobalEnv )
    #plot(prev_Pocillopora_3Size_pp_hist)
    
  }

plot(prev_Pocillopora_3Size_pp_hist)
plot(prev_Pocillopora_3Size_pp_dens)
plot(prev_Acropora_3Size_pp_hist)
plot(prev_Acropora_3Size_pp_dens)

# Prevalence 4Size  ------------------------------------------------------------ 

for(k in c("Pocillopora", "Acropora")){
  genus <- k
  size_class <- 4
  response <- "Percent_dead"
  distgo <- "prev"
  mod_version <- "Nsubmodel"
  mod_version_jags <- "_hierarchical.jags"
  out_dir <- paste0("model_out/",mod_version,"/",response,"/")
  mod_date <- "2024-12-31"
  
  # getting the model 
  
  zmPrp <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))
  grepgo <- grep('y.new', colnames(zmPrp[[1]]))
  yrep <- rbind(zmPrp[[1]][,grepgo],zmPrp[[2]][,grepgo],zmPrp[[3]][,grepgo])
  dim(yrep)
  
  ## getting the data
  data_expanded <- read.csv("data/final_model_inputs/mortality_data.csv")
  data_expanded$response<-data_expanded[,response]
  
  prev_mod_data <- data_expanded %>% 
    filter(Genus==genus) %>% 
    filter(!is.na(response)) %>% 
    filter(!is.na(Depth)) %>% 
    filter(!is.na(Size.class)) 
  
  # subset size class
  if(is.numeric(size_class)==TRUE){prev_mod_data <- prev_mod_data %>% filter(Size.class==size_class)}
  prev_mod_data$Size.class <- ifelse(prev_mod_data$Size.class==1|prev_mod_data$Size.class==2,1,prev_mod_data$Size.class)
  
  # remove 6 sites that aren't in the turbinara compiled data
  prev_mod_data <- prev_mod_data %>% 
    filter(!grepl("LTER",Site))
  
  y <- prev_mod_data$response
  y_p <- ifelse(y > 0, 1, 0)
  y<-y_p
  summary(y)
  
  plotName <- paste0(distgo,"_",genus,"_",size_class,"Size_pp_dens")
  assign(plotName, pp_check(y, yrep[sample(1:60000,length(y)),],ppc_dens_overlay), envir = .GlobalEnv )
  #plot(prev_Pocillopora_3Size_pp_dens)
  
  plotName <- paste0(distgo,"_",genus,"_",size_class,"Size_pp_hist")
  assign(plotName, ppc_stat(y, yrep[sample(1:60000,length(y)),], stat = "mean"), envir = .GlobalEnv )
  #plot(prev_Pocillopora_3Size_pp_hist)
  
}

plot(prev_Pocillopora_4Size_pp_hist)
plot(prev_Pocillopora_4Size_pp_dens)
plot(prev_Acropora_4Size_pp_hist)
plot(prev_Acropora_4Size_pp_dens)

# Severity All Size ------------------------------------------------------------ 

for(k in c("Pocillopora", "Acropora")){
  genus <- k
  size_class <- "all"
  response <- "Percent_dead"
  distgo <- "sev"
  mod_version <- "Nsubmodel"
  mod_version_jags <- "_hierarchical.jags"
  out_dir <- paste0("model_out/",mod_version,"/",response,"/")
  mod_date <- "2024-12-31"
  
  # getting the model 
  
  zmPrp <- readRDS(paste0('model_out/',distgo,'_',genus,'_',response,'_',size_class,'Size_',mod_date,'_',mod_version,'.Rdata'))
  grepgo <- grep('y.new', colnames(zmPrp[[1]]))
  yrep <- rbind(zmPrp[[1]][,grepgo],zmPrp[[2]][,grepgo],zmPrp[[3]][,grepgo])
  dim(yrep)
  
  ## getting the data
  data_expanded <- read.csv("data/final_model_inputs/mortality_data.csv")
  
  data_expanded$response <- data_expanded[,response]
  
  sev_mod_data <- data_expanded %>% 
    filter(Genus==genus) %>% 
    filter(!is.na(response)) %>% 
    filter(!is.na(Depth)) %>% 
    filter(!is.na(Size.class)) 
  
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
  y<-y_p
  summary(y)
  
  plotName <- paste0(distgo,"_",genus,"_",size_class,"Size_pp_dens")
  assign(plotName, pp_check(y, yrep[sample(1:60000,length(y)),],ppc_dens_overlay), envir = .GlobalEnv )
  plot(sev_Pocillopora_allSize_pp_dens)
  
  plotName <- paste0(distgo,"_",genus,"_",size_class,"Size_pp_hist")
  assign(plotName, ppc_stat(y, yrep[sample(1:60000,length(y)),], stat = "mean"), envir = .GlobalEnv )
  #plot(sev_Pocillopora_allSize_pp_hist)

}

plot(sev_Pocillopora_allSize_pp_hist)
plot(sev_Pocillopora_allSize_pp_hist)
plot(sev_Acropora_allSize_pp_hist)
plot(sev_Acropora_allSize_pp_hist)

# Combining Plots --------------------------------------------------------------

# Prevalence

prev_poc_acr_all134_hist<-cowplot::plot_grid(prev_Pocillopora_allSize_pp_hist, prev_Acropora_allSize_pp_hist,
                                             prev_Pocillopora_1Size_pp_hist, prev_Acropora_1Size_pp_hist, 
                                             prev_Pocillopora_3Size_pp_hist, prev_Acropora_3Size_pp_hist, 
                                             prev_Pocillopora_4Size_pp_hist, prev_Acropora_4Size_pp_hist, 
                                             ncol=2, align = "VH", scale=0.9)
ggsave("figs/pp_checks/prev_poc_acr_all134_hist.pdf", width=9, height=9, units="in")

prev_poc_acr_all134_dens<-cowplot::plot_grid(prev_Pocillopora_allSize_pp_dens, prev_Acropora_allSize_pp_dens,
                                             prev_Pocillopora_1Size_pp_dens, prev_Acropora_1Size_pp_dens, 
                                             prev_Pocillopora_3Size_pp_dens, prev_Acropora_3Size_pp_dens, 
                                             prev_Pocillopora_4Size_pp_dens, prev_Acropora_4Size_pp_dens, 
                                             ncol=2, align = "VH", scale=0.9)
ggsave("figs/pp_checks/prev_poc_acr_all134_dens.pdf", width=9, height=9, units="in")

# Severity

sev_poc_acr_all_hist<-cowplot::plot_grid(sev_Pocillopora_allSize_pp_hist, sev_Acropora_allSize_pp_hist, align = "VH")
ggsave("figs/pp_checks/sev_poc_acr_all_hist.pdf", width=6, height=3, units="in", scale=0.9)
# both work
sev_poc_acr_all_dens<-cowplot::plot_grid(sev_Pocillopora_allSize_pp_dens, sev_Acropora_allSize_pp_dens, align = "VH")
ggsave("figs/pp_checks/sev_poc_acr_all_dens.pdf", width=6, height=3, units="in", scale=0.9)






# -----------------------------------------------------------------------------#
# Nitrogen enrichment determines coral mortality during a marine heatwave
#  
# 0_download_data
# -----------------------------------------------------------------------------#

# this script downloads data from EDI data repository
# creates necessary folders to store data and model results 
# creates summarized site-level data file for maps

# Packages --------------------------------------------------------------------#

library(EDIutils)
library(tidyverse)

# Download data ----------------------------------------------------------------#

# create data folder 
dir.create("data")
dir.create("data/final_model_inputs")

# EDI data packageID is: "knb-lter-mcr.2014.1"
read_data_package(packageId="knb-lter-mcr.2014.1")

# read data package, download package zip to folder
read_data_package_archive(packageId="knb-lter-mcr.2014.1", path = "data/final_model_inputs/")

# unzip the file
unzip("data/final_model_inputs/knb-lter-mcr.2014.1.zip", exdir ="data/final_model_inputs/")


# create folders for storing figs and model outputs ----------------------------#

# create folders for figures 
dir.create("figs")
dir.create("figs/pp_checks")

# create folders for figures 
dir.create("model_out")
dir.create("model_out/Nsubmodel")
dir.create("model_out/Nsubmodel/Percent_dead")

# create a folder for files to make map
dir.create("data/moorea_map")

# summarizes and exports site data for map --------------------------------------

data_expanded<-read.csv("data/final_model_inputs/mortality_data.csv", stringsAsFactors = TRUE)
turb_N_allYears<-read.csv("data/final_model_inputs/turb_all_years.csv", stringsAsFactors = TRUE)

# Site, heat stress, nitrogen
temp <- turb_N_allYears %>% group_by(Site,site_index) %>% summarise(meanN=mean(N,na.rm = T)) %>% ungroup()

# mortality prevalence
prev <- data_expanded %>% group_by(Genus,Site) %>% summarise(meanPrev=mean(Percent_dead)) %>% ungroup() %>% tidyr::pivot_wider(names_from=Genus,values_from=meanPrev,names_prefix="prev_")

# number of corals to calculate mortality prevalence
prev_n <- data_expanded %>% group_by(Genus,Site) %>% summarise(n_obs=length(Percent_dead)) %>% ungroup() %>% tidyr::pivot_wider(names_from=Genus,values_from=n_obs,names_prefix="n_obs_")

# average mortality severity for Pocillopora
poc_sev_avg <- data_expanded %>% filter(Genus=="Pocillopora" & Percent_dead > 0) %>% group_by(Site) %>% summarise(sev_avg_Pocillopora=mean(Percent_dead),n_obs_sev_Pocillopora=length(Percent_dead))

# proportion of corals that are severely dead (>75%) for Pocillopora
poc_sev_prop <- data_expanded %>% filter(Genus=="Pocillopora" & Percent_dead > 0) %>% mutate(Sev_dead=ifelse(Percent_dead>75,1,0)) %>% group_by(Site) %>% summarise(sev_prop_Pocillopora=mean(Sev_dead))

# average mortality severity for Acropora
acr_sev_avg <- data_expanded %>% filter(Genus=="Acropora" & Percent_dead > 0) %>% group_by(Site) %>% summarise(sev_avg_Acropora=mean(Percent_dead),n_obs_sev_Acropora=length(Percent_dead))

# proportion of corals that are severely dead (>75%) for Acropora
acr_sev_prop <- data_expanded %>% filter(Genus=="Acropora" & Percent_dead > 0) %>% mutate(Sev_dead=ifelse(Percent_dead>75,1,0)) %>% group_by(Site) %>% summarise(sev_prop_Acropora=mean(Sev_dead))

map_fig_data <- data_expanded %>% 
  filter(!grepl("LTER",Site)) %>%  
  distinct(Site,site_index,Latitude.2019,Longitude.2019,max_heatstress) %>% 
  left_join(temp,by=c('site_index','Site')) %>% 
  left_join(prev, by="Site") %>% 
  left_join(prev_n, by="Site") %>% 
  left_join(poc_sev_avg,by="Site") %>% 
  left_join(poc_sev_prop,by="Site") %>%
  left_join(acr_sev_avg,by="Site") %>% 
  left_join(acr_sev_prop,by="Site") 

write.csv(map_fig_data, "data/moorea_map/site_data_for_map.csv", row.names=F)


#   -----------------------------------------------------------------------
# Moorea 2019 Lagoon coral bleaching and mortality surveys
#   -----------------------------------------------------------------------

# Set up data for modeling

library(dplyr)
library(tidyr)

# bleaching data ----------------------------------------------------------
data <- read.csv('data/Lagoon_bleaching_updated.csv')

# subset for only those species we define as "common"
data <- subset(data, Common==1)

# expand to one line per colony
data <- data %>% filter(!is.na(Count)) # 2 lines without count data?
data_expanded <- data %>% uncount(Count) 

# merge with site location data
site_locations <- read.csv("data/site_check.csv")
data_expanded <- data_expanded %>% left_join(site_locations[,c('Site','Latitude.2019','Longitude.2019','LTER_site_go','Habitat_go')],by=c('Site'))

# add other site data
site_other_data <- read.csv('data/Site_locations.csv')
data_expanded <- left_join(data_expanded, site_other_data %>% select(Name,Island_shore), by=c('Site'='Name'))

# heat stress data --------------------------------------------------------
# calculated in 'temperature_fill.R'
# heatstress is the maximum cumulative heatstress calculated with 12 wk cumulative hotspots over 29 degrees
heatstress <- read.csv('cumulative_heatstress_2019_bysite_filled.csv')
heatstress <- heatstress %>% mutate(LTER_site_go = stringr::str_replace(Site,'0','_')) %>% select(-Site) %>% 
  mutate(Habitat_go=ifelse(habitat=='backreef','Lagoon','Fringe'))

# combine with data
data_expanded <- left_join(data_expanded, heatstress, by=c('LTER_site_go','Habitat_go'))
summary(data_expanded)


# turb nutrients ----------------------------------------------------------
turb_N_allYears <- read.csv('data/turb_N_allYears_sub.csv') # figure out which file this was made in and check?

# need the same site index in both the bleaching and turb data
bleaching_site_index <- data_expanded %>% filter(Habitat_go=='Lagoon') %>% distinct(Site) 
bleaching_site_index$site_index <- as.numeric(as.factor(bleaching_site_index$Site))
data_expanded <- left_join(data_expanded,bleaching_site_index,by='Site')

## check that all sites are there - 6 sites not in combined turbinara database, the LTER sites, going to exclude from analysis for now
turb_N_allYears_sub <- turb_N_allYears[turb_N_allYears$Site %in% bleaching_site_index$Site,]
length(unique(turb_N_allYears_sub$Site))
temp <- left_join(bleaching_site_index, data.frame(Site=as.character(unique(turb_N_allYears_sub$Site)),Site_turb=unique(turb_N_allYears_sub$Site)),by='Site')

turb_N_allYears$Site <- as.character(turb_N_allYears$Site)
turb_N_allYears <- turb_N_allYears %>% select(-site_index) # left over from something else?
turb_N_allYears <- left_join(turb_N_allYears, bleaching_site_index, by='Site')


# export ------------------------------------------------------------------

write.csv(data_expanded, "data/final_model_inputs/data_expanded.csv",row.names=F)
write.csv(turb_N_allYears, "data/final_model_inputs/turb_all_years.csv",row.names=F)


# data for map ------------------------------------------------------------

data_expanded<-read.csv("data/final_model_inputs/data_expanded.csv", stringsAsFactors = TRUE)
turb_N_allYears<-read.csv("data/final_model_inputs/turb_all_years.csv", stringsAsFactors = TRUE)

# Site, heat stress, N, raw avg prev, raw avg sev
temp <- turb_N_allYears %>% group_by(Site,site_index) %>% summarise(meanN=mean(N,na.rm = T)) %>% ungroup()
temp$Site<-as.factor(temp$Site) # KES added to make below code run

prev_avg <- data_expanded %>% group_by(Genus,Site) %>% summarise(meanPrev=mean(Percent_dead)) %>% ungroup() %>% tidyr::pivot_wider(names_from=Genus,values_from=meanPrev,names_prefix="prev_")

prev_n <- data_expanded %>% group_by(Genus,Site) %>% summarise(n_obs=length(Percent_dead)) %>% ungroup() %>% tidyr::pivot_wider(names_from=Genus,values_from=n_obs,names_prefix="n_obs_")

#poc_sev_avg <- data_expanded %>% filter(Genus=="Pocillopora" & Percent_dead > 0) %>% group_by(Site) %>% summarise(meanSevPoc=mean(Percent_dead),n_dead_Poc=length(Percent_dead))
#calculating the proportion of corals that are severely dead (>75%)
poc_sev_avg <- data_expanded %>% filter(Genus=="Pocillopora" & Percent_dead > 0) %>% mutate(Sev_dead=ifelse(Percent_dead>75,1,0)) %>% group_by(Site) %>% summarise(meanSevPoc=mean(Sev_dead),n_dead_Poc=length(Percent_dead))

#acr_sev_avg <- data_expanded %>% filter(Genus=="Acropora" & Percent_dead > 0) %>% group_by(Site) %>% summarise(meanSevAcr=mean(Percent_dead),n_dead_Acr=length(Percent_dead))
acr_sev_avg <- data_expanded %>% filter(Genus=="Acropora" & Percent_dead > 0) %>% mutate(Sev_dead=ifelse(Percent_dead>75,1,0)) %>% group_by(Site) %>% summarise(meanSevAcr=mean(Sev_dead),n_dead_Poc=length(Percent_dead))

map_fig_data <- data_expanded %>% 
  filter(!grepl("LTER",Site)) %>%  filter(Habitat_go=='Lagoon') %>% 
  distinct(Site,site_index,Latitude.2019,Longitude.2019,max_heatstress) %>% 
  left_join(temp,by=c('site_index','Site')) %>% 
  left_join(prev_avg, by="Site") %>% 
  left_join(prev_n, by="Site") %>% 
  left_join(poc_sev_avg,by="Site") %>% 
  left_join(acr_sev_avg,by="Site")

write.csv(map_fig_data, "data/final_model_inputs/site_data_for_map.csv", row.names=F)

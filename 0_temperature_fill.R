
#   -----------------------------------------------------------------------
# Fill in missing temperatures using linear models
# Plots figure 1a,b
#   -----------------------------------------------------------------------

# Some loggers were missing in 2019, but we have timeseries going back to 2005 and patterns among sites are highly correlated (>0.97). Using existing data we impute the missing data. 

library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(rjags)
library(ggplot2)
library(RcppRoll)
library(ggpubr)
library(scales)
library(RcppRoll)

# data --------------------------------------------------------------------

# data from Moorea Coral Reef LTER core time series, bottom mounted termistors:
# http://mcrlter.msi.ucsb.edu/cgi-bin/showDataset.cgi?docid=knb-lter-mcr.1035 # accessed October 28, 2020
# Leichter, J, K. Seydel and C. Gotschalk of Moorea Coral Reef LTER. 2018. MCR LTER: Coral Reef: Benthic Water Temperature, ongoing since 2005. knb-lter-mcr.1035.11

temperature <- read.csv(file.path(getwd(),'data/water_temp',paste('MCR_LTER','00','_BottomMountThermistors_20190926.csv',sep='')))
for(i in c('01','02','03','04','05','06')){
  temperature <- rbind(temperature,read.csv(file.path(getwd(),'data/water_temp',paste('MCR_LTER',i,'_BottomMountThermistors_20191023.csv',sep=''))))
 }

 head(temperature)
# saveRDS(temperature,file='data/water_temp/temperature_combined_2019.Rdata')
# temperature <- readRDS(file='data/water_temp/temperature_combined_2019.Rdata')
 temperature$time_use <- ymd_hms(temperature$time_local)
 temperature$day <- format(temperature$time_use, '%Y-%m-%d')

# median temperature by day (and filter out forereef)
 temperature.day <- temperature %>% group_by(site,reef_type_code,sensor_type,sensor_depth_m,day) %>% filter(reef_type_code=='BAK' | reef_type_code=='FRI') %>% summarise(temp_c = median(temperature_c)) %>% ungroup()
 temperature.day$day <- ymd(temperature.day$day)
 saveRDS(temperature.day, file='data/water_temp/temperature_day_2019.Rdata')

#temperature.day <- readRDS('data/water_temp/temperature_day_2019.Rdata')
temperature.day$day <- as.Date(temperature.day$day, '%Y-%m-%d')
temperature.day$monthday <- format(temperature.day$day, '%m-%d')
temperature.day$month <- month(temperature.day$day)
str(temperature.day)

# subset for months and habitats of interest
temp_sub <- temperature.day %>% filter(month ==12 | month >= 1 & month < 9)
temp_sub <- temperature.day %>% filter(month >= 3 & month < 6)
temp_sub <- temperature.day %>% filter(reef_type_code=='BAK' | reef_type_code=='FRI')
str(temp_sub)
temp_sub$site_hab <- paste(temp_sub$site,temp_sub$reef_type_code,temp_sub$sensor_type,temp_sub$sensor_depth_m,sep='_')

# create a continuous sequence of dates
alldays <- data.frame(alldays = seq(ymd(min(temp_sub$day)), ymd(max(temp_sub$day)), by = "days")); alldays$day <- alldays$alldays
alldays <- temp_sub %>% dplyr::select('day','site_hab') %>% expand(day, site_hab)
temp_sub <- left_join(alldays,temp_sub,by=c('day','site_hab'))
head(temp_sub)
summary(temp_sub)
temp_sub %>% group_by(site_hab) %>% summarise(n=length(day > 0)) %>% ungroup()

# add year and month cols
temp_sub$month <- month(temp_sub$day)
temp_sub$year <- year(temp_sub$day)

# how many days have no data at any of the sites
temp_sub %>% 
  group_by(day,site_hab) %>% summarise(n_NA=sum(is.na(temp_c))) %>% 
  pivot_wider(names_from = site_hab,values_from=n_NA) %>% 
  mutate(n_site_na = sum(c_across((LTER01_BAK_sb39_1:LTER06_FRI_hobo_4))), year=year(day)) %>% 
  dplyr::select(year,day,n_site_na) %>% filter(n_site_na>=18)

temp_sub %>% 
  group_by(day,site_hab) %>% summarise(n_NA=sum(is.na(temp_c))) %>% 
  pivot_wider(names_from = site_hab,values_from=n_NA) %>% 
  mutate(n_site_na = sum(c_across((LTER01_BAK_sb39_1:LTER06_FRI_hobo_4))), year=year(day)) %>% 
  dplyr::select(year,day,n_site_na) %>% filter(year==2019) %>% View()
# %>% group_by(year) %>% summarise(n=length(n_site_na > 0))


# plot raw data -----------------------------------------------------------
temp_sub %>% 
  filter(day > '2018-12-01') %>%
  filter(reef_type_code == 'BAK') %>%
  # distinct(date,Site,cum_heat) %>% 
  ggplot() + 
  geom_point(aes(x=day,y=temp_c,color=site))  + 
  facet_wrap(~site) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.justification = c(0, 1),
    # legend.position = c(0, 1),
    axis.ticks = element_line(colour = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size=12), axis.title = element_text(size=12)
  )  +
  xlab('Date') +
  ylab('Cumulative Heat stress 2019') +
  ggtitle('Back')


temp_sub %>% 
  filter(day > '2018-12-01') %>%
  filter(reef_type_code == 'FRI' & sensor_depth_m == 1) %>%
  # distinct(date,Site,cum_heat) %>% 
  ggplot() + 
  geom_point(aes(x=day,y=temp_c,color=site))  + 
  facet_wrap(~site) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.justification = c(0, 1),
    # legend.position = c(0, 1),
    axis.ticks = element_line(colour = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size=12), axis.title = element_text(size=12)
  )  +
  xlab('Date') +
  ylab('Cumulative Heat stress 2019') +
  ggtitle('Fringing, depth=1m')




# predict missing ---------------------------------------------------------

############### backreef
temp_bak_long <- temp_sub %>% filter(reef_type_code=='BAK') %>% dplyr::select(site,day,temp_c) 
temp_bak_long$month <- month(temp_bak_long$day)
temp_bak_long$temp_c_warm <- ifelse(temp_bak_long$month >= 3 & temp_bak_long$month < 6,temp_bak_long$temp_c,NA)
str(temp_bak_long)

temp_bak_wide <- temp_bak_long %>% dplyr::select(day,site,temp_c) %>% pivot_wider(names_from=site,values_from = temp_c)
temp_bak_wide <- temp_bak_wide[c('day','LTER01','LTER02','LTER03','LTER04','LTER05','LTER06')] # makes sure they are in order
summary(temp_bak_wide)

temp_bak_wide_warm <- temp_bak_long %>% filter(month >= 3 & month < 6) %>% dplyr::select(day,site,temp_c) %>% pivot_wider(names_from=site,values_from = temp_c)
temp_bak_wide_warm <- temp_bak_wide_warm[c('day','LTER01','LTER02','LTER03','LTER04','LTER05','LTER06')] # makes sure they are in order
summary(temp_bak_wide_warm)

temp_bak_wide_warm$year <- year(temp_bak_wide_warm$day)
temp <- temp_bak_wide_warm %>% filter(year==2019)
summary(temp) # LTER02, LTER03 LTER04, and LTER05 have complete data in 2019

# temp <- temp_bak_wide %>% select(day,LTER01,LTER03,LTER04) %>% filter(!is.na(LTER01))
# # lm01_3_4 <- lm(LTER01 ~ LTER03+LTER04, data=temp_bak_wide)
# # lm01_2_3_4 <- lm(LTER01 ~ LTER02+LTER03+LTER04, data=temp_bak_wide)
# lm01_2_3_4_5 <- lm(LTER01 ~ LTER02+LTER03+LTER04+LTER05, data=temp_bak_wide)
# # temp <- temp_bak_wide %>% select(day,LTER01,LTER03,LTER04)
# # temp_bak_wide$pred01_3_4 <- predict(lm01_3_4,newdata = temp_bak_wide)
# # temp_bak_wide$pred01_2_3_4 <- predict(lm01_2_3_4,newdata = temp_bak_wide)
# temp_bak_wide$pred01_2_3_4_5 <- predict(lm01_2_3_4_5,newdata = temp_bak_wide)

# temp <- temp_bak_wide %>% select(day,LTER02,LTER03,LTER04) %>% filter(!is.na(LTER02))
# lm02 <- lm(LTER02 ~ LTER03+LTER04, data=temp)
# lm02_3_4 <- lm(LTER02 ~ LTER03+LTER04, data=temp_bak_wide)
# lm02_3_4_5 <- lm(LTER02 ~ LTER03+LTER04+LTER05, data=temp_bak_wide)
# lm02_3_4_5_6 <- lm(LTER02 ~ LTER03+LTER04+LTER05+LTER06, data=temp_bak_wide)
# temp <- temp_bak_wide %>% select(day,LTER02,LTER03,LTER04)
# temp_bak_wide$pred02_3_4 <- predict(lm02_3_4,newdata = temp_bak_wide)
# temp_bak_wide$pred02_3_4_5 <- predict(lm02_3_4_5,newdata = temp_bak_wide)
# temp_bak_wide$pred02_3_4_5_6 <- predict(lm02_3_4_5_6,newdata = temp_bak_wide)

# temp <- temp_bak_wide %>% select(day,LTER05,LTER03,LTER04) %>% filter(!is.na(LTER05))
# lm05 <- lm(LTER05 ~ LTER03+LTER04, data=temp)
# temp <- temp_bak_wide %>% select(day,LTER05,LTER03,LTER04)
# temp_bak_wide$pred05 <- predict(lm05,newdata = temp)
# 
# temp <- temp_bak_wide %>% select(day,LTER06,LTER03,LTER04) %>% filter(!is.na(LTER06))
# lm06 <- lm(LTER06 ~ LTER03+LTER04, data=temp)
# temp <- temp_bak_wide %>% select(day,LTER06,LTER03,LTER04)
# temp_bak_wide$pred06 <- predict(lm06,newdata = temp)

lm01_2_3_4_5 <- lm(LTER01 ~ LTER02+LTER03+LTER04+LTER05, data=temp_bak_wide_warm)
lm01_2_3_4 <- lm(LTER01 ~ LTER02+LTER03+LTER04, data=temp_bak_wide_warm)
lm01_2_3_5 <- lm(LTER01 ~ LTER02+LTER03+LTER05, data=temp_bak_wide_warm)
lm01_2_4_5 <- lm(LTER01 ~ LTER02+LTER04+LTER05, data=temp_bak_wide_warm)
lm01_3_4_5 <- lm(LTER01 ~ LTER03+LTER04+LTER05, data=temp_bak_wide_warm)
lm01_2 <- lm(LTER01 ~ LTER02, data=temp_bak_wide_warm)
lm01_3_4 <- lm(LTER01 ~ LTER03 + LTER04, data=temp_bak_wide_warm)
temp_bak_wide$pred01_2_3_4_5 <- predict(lm01_2_3_4_5,newdata = temp_bak_wide)
temp_bak_wide$pred01_2_3_4 <- predict(lm01_2_3_4,newdata = temp_bak_wide)
temp_bak_wide$pred01_2_3_5 <- predict(lm01_2_3_5,newdata = temp_bak_wide)
temp_bak_wide$pred01_2_4_5 <- predict(lm01_2_4_5,newdata = temp_bak_wide)
temp_bak_wide$pred01_3_4_5 <- predict(lm01_3_4_5,newdata = temp_bak_wide)
temp_bak_wide$pred01_2 <- predict(lm01_2,newdata = temp_bak_wide)
temp_bak_wide$pred01_3_4 <- predict(lm01_3_4,newdata = temp_bak_wide)

lm02_2_3_4_5 <- lm(LTER02 ~ LTER03+LTER04+LTER05, data=temp_bak_wide)
lm02_3_4 <- lm(LTER02 ~ LTER03+LTER04, data=temp_bak_wide)
temp_bak_wide$pred02_2_3_4_5 <- predict(lm02_2_3_4_5,newdata = temp_bak_wide)
temp_bak_wide$pred02_3_4 <- predict(lm02_3_4,newdata = temp_bak_wide)

lm03_2_3_4_5 <- lm(LTER03 ~ LTER01+LTER02+LTER04+LTER05, data=temp_bak_wide)
lm03_1_2 <- lm(LTER03 ~ LTER01+LTER02, data=temp_bak_wide)
lm03_2_4_5 <- lm(LTER03 ~ LTER02+LTER04+LTER05, data=temp_bak_wide)
lm03_2 <- lm(LTER03 ~ LTER02, data=temp_bak_wide)
temp_bak_wide$pred03_2_3_4_5 <- predict(lm03_2_3_4_5,newdata = temp_bak_wide)
temp_bak_wide$pred03_1_2 <- predict(lm03_1_2,newdata = temp_bak_wide)
temp_bak_wide$pred03_2_4_5 <- predict(lm03_2_4_5,newdata = temp_bak_wide)
temp_bak_wide$pred03_2 <- predict(lm03_2,newdata = temp_bak_wide)

lm04_2_3_4_5 <- lm(LTER04 ~ LTER01+LTER02+LTER03+LTER05, data=temp_bak_wide)
lm04_1_2 <- lm(LTER04 ~ LTER01+LTER02, data=temp_bak_wide)
lm04_2 <- lm(LTER04 ~ LTER02, data=temp_bak_wide)
temp_bak_wide$pred04_2_3_4_5 <- predict(lm04_2_3_4_5,newdata = temp_bak_wide)
temp_bak_wide$pred04_1_2 <- predict(lm04_1_2,newdata = temp_bak_wide)
temp_bak_wide$pred04_2 <- predict(lm04_2,newdata = temp_bak_wide)

lm05_2_3_4_5 <- lm(LTER05 ~ LTER01+LTER02+LTER03+LTER04+LTER06, data=temp_bak_wide)
lm05_2 <- lm(LTER05 ~ LTER02, data=temp_bak_wide)
lm05_1_2 <- lm(LTER05 ~ LTER01+LTER02, data=temp_bak_wide)
lm05_2_3_4_6 <- lm(LTER05 ~ LTER02+LTER03+LTER04+LTER06, data=temp_bak_wide)
lm05_3_4 <- lm(LTER05 ~ LTER03+LTER04, data=temp_bak_wide)
temp_bak_wide$pred05_2_3_4_5 <- predict(lm05_2_3_4_5,newdata = temp_bak_wide)
temp_bak_wide$pred05_2 <- predict(lm05_2,newdata = temp_bak_wide)
temp_bak_wide$pred05_1_2 <- predict(lm05_1_2,newdata = temp_bak_wide)
temp_bak_wide$pred05_2_3_4_6 <- predict(lm05_2_3_4_6,newdata = temp_bak_wide)
temp_bak_wide$pred05_3_4 <- predict(lm05_3_4,newdata = temp_bak_wide)

lm06_1_2_3_4_5 <- lm(LTER06 ~ LTER01+LTER02+LTER03+LTER04+LTER05, data=temp_bak_wide)
lm06_2 <- lm(LTER06 ~ LTER02, data=temp_bak_wide)
lm06_1_2 <- lm(LTER06 ~ LTER01+LTER02, data=temp_bak_wide)
lm06_1_2_3_4 <- lm(LTER06 ~ LTER01+LTER02+LTER03+LTER04, data=temp_bak_wide)
lm06_2_4_5 <- lm(LTER06 ~ LTER02+LTER04+LTER05, data=temp_bak_wide)
lm06_3_4_5 <- lm(LTER06 ~ LTER03+LTER04+LTER05, data=temp_bak_wide)
lm06_3_4 <- lm(LTER06 ~ LTER03+LTER04, data=temp_bak_wide)
lm06_2_3_4_5 <- lm(LTER06 ~ LTER02+LTER03+LTER04+LTER05, data=temp_bak_wide)
temp_bak_wide$pred06_1_2_3_4_5 <- predict(lm06_1_2_3_4_5,newdata = temp_bak_wide)
temp_bak_wide$pred06_2 <- predict(lm06_2,newdata = temp_bak_wide)
temp_bak_wide$pred06_1_2 <- predict(lm06_1_2,newdata = temp_bak_wide)
temp_bak_wide$pred06_1_2_3_4 <- predict(lm06_1_2_3_4,newdata = temp_bak_wide)
temp_bak_wide$pred06_2_4_5 <- predict(lm06_2_4_5,newdata = temp_bak_wide)
temp_bak_wide$pred06_3_4_5 <- predict(lm06_3_4_5,newdata = temp_bak_wide)
temp_bak_wide$pred06_3_4 <- predict(lm06_3_4,newdata = temp_bak_wide)
temp_bak_wide$pred06_2_3_4_5 <- predict(lm06_2_3_4_5,newdata = temp_bak_wide)

temp_bak_wide$fill01 <- ifelse(is.na(temp_bak_wide$LTER01),temp_bak_wide$pred01_2_3_4_5,temp_bak_wide$LTER01)
temp_bak_wide$fill01 <- ifelse(is.na(temp_bak_wide$fill01),temp_bak_wide$pred01_2_3_4,temp_bak_wide$fill01)
temp_bak_wide$fill01 <- ifelse(is.na(temp_bak_wide$fill01),temp_bak_wide$pred01_2_3_5,temp_bak_wide$fill01)
temp_bak_wide$fill01 <- ifelse(is.na(temp_bak_wide$fill01),temp_bak_wide$pred01_2_4_5,temp_bak_wide$fill01)
temp_bak_wide$fill01 <- ifelse(is.na(temp_bak_wide$fill01),temp_bak_wide$pred01_3_4_5,temp_bak_wide$fill01)
temp_bak_wide$fill01 <- ifelse(is.na(temp_bak_wide$fill01),temp_bak_wide$pred01_3_4,temp_bak_wide$fill01)
temp_bak_wide$fill01 <- ifelse(is.na(temp_bak_wide$fill01),temp_bak_wide$pred01_2,temp_bak_wide$fill01)

temp_bak_wide$fill02 <- ifelse(is.na(temp_bak_wide$LTER02),temp_bak_wide$pred02_2_3_4_5,temp_bak_wide$LTER02)
temp_bak_wide$fill02 <- ifelse(is.na(temp_bak_wide$fill02),temp_bak_wide$pred02_3_4,temp_bak_wide$fill02)

temp_bak_wide$fill03 <- ifelse(is.na(temp_bak_wide$LTER03),temp_bak_wide$pred03_2_3_4_5,temp_bak_wide$LTER03)
temp_bak_wide$fill03 <- ifelse(is.na(temp_bak_wide$fill03),temp_bak_wide$pred03_2_4_5,temp_bak_wide$fill03)
temp_bak_wide$fill03 <- ifelse(is.na(temp_bak_wide$fill03),temp_bak_wide$pred03_1_2,temp_bak_wide$fill03)
temp_bak_wide$fill03 <- ifelse(is.na(temp_bak_wide$fill03),temp_bak_wide$pred03_2,temp_bak_wide$fill03)

temp_bak_wide$fill04 <- ifelse(is.na(temp_bak_wide$LTER04),temp_bak_wide$pred04_2_3_4_5,temp_bak_wide$LTER04)
temp_bak_wide$fill04 <- ifelse(is.na(temp_bak_wide$fill04),temp_bak_wide$pred04_1_2,temp_bak_wide$fill04)
temp_bak_wide$fill04 <- ifelse(is.na(temp_bak_wide$fill04),temp_bak_wide$pred04_2,temp_bak_wide$fill04)

temp_bak_wide$fill05 <- ifelse(is.na(temp_bak_wide$LTER05),temp_bak_wide$pred05_2_3_4_5,temp_bak_wide$LTER05)
temp_bak_wide$fill05 <- ifelse(is.na(temp_bak_wide$fill05),temp_bak_wide$pred05_2_3_4_6,temp_bak_wide$fill05)
temp_bak_wide$fill05 <- ifelse(is.na(temp_bak_wide$fill05),temp_bak_wide$pred05_1_2,temp_bak_wide$fill05)
temp_bak_wide$fill05 <- ifelse(is.na(temp_bak_wide$fill05),temp_bak_wide$pred05_3_4,temp_bak_wide$fill05)
temp_bak_wide$fill05 <- ifelse(is.na(temp_bak_wide$fill05),temp_bak_wide$pred05_2,temp_bak_wide$fill05)

temp_bak_wide$fill06 <- ifelse(is.na(temp_bak_wide$LTER06),temp_bak_wide$pred06_1_2_3_4_5,temp_bak_wide$LTER06)
temp_bak_wide$fill06 <- ifelse(is.na(temp_bak_wide$fill06),temp_bak_wide$pred06_1_2_3_4,temp_bak_wide$fill06)
temp_bak_wide$fill06 <- ifelse(is.na(temp_bak_wide$fill06),temp_bak_wide$pred06_2_3_4_5,temp_bak_wide$fill06)
temp_bak_wide$fill06 <- ifelse(is.na(temp_bak_wide$fill06),temp_bak_wide$pred06_2_4_5,temp_bak_wide$fill06)
temp_bak_wide$fill06 <- ifelse(is.na(temp_bak_wide$fill06),temp_bak_wide$pred06_3_4_5,temp_bak_wide$fill06)
temp_bak_wide$fill06 <- ifelse(is.na(temp_bak_wide$fill06),temp_bak_wide$pred06_1_2,temp_bak_wide$fill06)
temp_bak_wide$fill06 <- ifelse(is.na(temp_bak_wide$fill06),temp_bak_wide$pred06_3_4,temp_bak_wide$fill06)
temp_bak_wide$fill06 <- ifelse(is.na(temp_bak_wide$fill06),temp_bak_wide$pred06_2,temp_bak_wide$fill06)

summary(temp_bak_wide)

temp_bak_wide$flag01 <- ifelse(is.na(temp_bak_wide$LTER01),1,0)
temp_bak_wide$flag02 <- ifelse(is.na(temp_bak_wide$LTER02),1,0)
temp_bak_wide$flag03 <- ifelse(is.na(temp_bak_wide$LTER03),1,0)
temp_bak_wide$flag04 <- ifelse(is.na(temp_bak_wide$LTER04),1,0)
temp_bak_wide$flag05 <- ifelse(is.na(temp_bak_wide$LTER05),1,0)
temp_bak_wide$flag06 <- ifelse(is.na(temp_bak_wide$LTER06),1,0)


temp <- temp_bak_wide %>% 
  filter(day > '2018-12-01') %>%
  mutate(year = year(day)) %>% 
  dplyr::select(-year) %>% 
  pivot_longer(cols = LTER01:flag06, names_to='var', values_to='value') %>% 
  separate(var, c('type','site'), 4)

site_names <- c(
  `01` = "LTER 1",
  `02` = "LTER 2",
  `03` = "LTER 3",
  `04` = "LTER 4",
  `05` = "LTER 5",
  `06` = "LTER 6"
)

temp_known_predicted_time_plot<-ggplot() + 
  geom_point(data = temp %>% filter(type=='fill' | type=='flag') %>% pivot_wider(names_from='type',values_from='value'), 
             aes(x=day,y=fill,color=as.factor(flag)), pch=16, alpha=0.7)  + 
  scale_color_manual(labels = c("known", "predicted"), values = c("blue", "red")) +
  facet_wrap(~site, labeller = as_labeller(site_names)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    #legend.justification = c(0, 1),
    # legend.position = c(0, 1),
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "black", size = 0.5),
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size=12), axis.title = element_text(size=12)
  )  +
  xlab('Date') +
  ylab(expression(paste('Temperature (',~degree,'C)',sep='')))
ggsave("figs/temp_known_predicted_time_plot.pdf", width=8, height=8, units="in")


# calc heat stress --------------------------------------------------------

# caculate weekly means
temp_bak_weekly <- temp_bak_wide %>% 
  dplyr::select(day,starts_with('fill')) %>% 
  pivot_longer(cols=starts_with('fill'),names_to='site',values_to='temperature') %>% 
  mutate(month=month(day),year=year(day),week=week(day)) %>% 
  group_by(site,year,week) %>% 
  summarise(temp_c=median(temperature,na.rm=T)) %>% 
  ungroup()

# calculate accumulated heat stress
mma_ref <- 29
temp_bak_weekly$hotspot <- temp_bak_weekly$temp_c - mma_ref
temp_bak_weekly$hotspot[temp_bak_weekly$hotspot < 0] <- 0

temp_bak_cumheat <- temp_bak_weekly %>%
  arrange(site, year, week) %>%
  group_by(site) %>%
  mutate(cum_heat = roll_sum(hotspot, 12, align = "right", fill = NA))

temp_bak_cumheat$date <- as.Date(paste(temp_bak_cumheat$year, temp_bak_cumheat$week, 1, sep="-"), "%Y-%U-%w")
temp_bak_cumheat$Site <- stringr::str_remove(temp_bak_cumheat$site,'_fill')

temp_bak_cumheat %>% 
  filter(date > '2018-12-01') %>% 
  distinct(date,Site,cum_heat) %>% 
  ggplot() + 
  geom_line(aes(x=date,y=cum_heat,color=Site),lwd=2)  + 
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.justification = c(0, 1),
    # legend.position = c(0, 1),
    axis.ticks = element_line(colour = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size=12), axis.title = element_text(size=12)
  )  +
  xlab('Date') +
  ylab('Cumulative Heat stress 2019')


# predict old heat stress events? -----------------------------------------

## LTER02 data were missing in 2006-7

# predicting for one backreef site using data from 4 other backreef sites with data in 2007
temp2007 <- temp_bak_wide %>% 
  filter(day > '2006-12-01' & day < '2007-07-01')

# pulling fil data for lter01
ggplot(data=temp2007, aes(x=LTER01,y=pred01_3_4_5)) + geom_point() + geom_abline(a=0,b=1) + geom_smooth(method='lm')
temp2007$fill01 <- temp2007$pred01_3_4_5
# ideally we would predict by 3_4_5_6 but we didn't set up this script to predict that way 

# pulling fil data for lter01
temp2007$fill06 <- temp2007$pred06_3_4_5
ggplot(data=temp2007, aes(x=LTER06,y=pred06_3_4_5)) + geom_point() + geom_abline(a=0,b=1) + geom_smooth(method='lm')
# until here

# calculate heat stress from the actual and predicted and check the congruence 

## FILL data --- 
# caculate weekly median
weekly_2007_fill <- temp2007 %>% 
  dplyr::select(day,starts_with('fill')) %>% 
  pivot_longer(cols=starts_with('fill'),names_to='site',values_to='temperature') %>% 
  mutate(month=month(day),year=year(day),week=week(day)) %>% 
  group_by(site,year,week) %>% 
  summarise(temp_c=median(temperature,na.rm=T)) %>% 
  ungroup()

# calculate accumulated heat stress
mma_ref <- 29
weekly_2007_fill$hotspot <- weekly_2007_fill$temp_c - mma_ref
weekly_2007_fill$hotspot[weekly_2007_fill$hotspot < 0] <- 0

cumheat2007_fill <- weekly_2007_fill %>%
  arrange(site, year, week) %>%
  group_by(site) %>%
  mutate(cum_heat_fill = roll_sum(hotspot, 12, align = "right", fill = NA))
cumheat2007_fill$site <-stringr::str_replace(cumheat2007_fill$site,'fill','LTER')

# Real data --- 
weekly_2007_real <- temp_bak_wide %>% 
  filter(day > '2006-12-01' & day < '2007-07-01') %>% 
  dplyr::select(day,starts_with('LTER')) %>% 
  pivot_longer(cols=starts_with('LTER'),names_to='site',values_to='temperature') %>% 
  mutate(month=month(day),year=year(day),week=week(day)) %>% 
  group_by(site,year,week) %>% 
  summarise(temp_c=median(temperature,na.rm=T)) %>% 
  ungroup()

# calculate accumulated heat stress
mma_ref <- 29
weekly_2007_real$hotspot <- weekly_2007_real$temp_c - mma_ref
weekly_2007_real$hotspot[weekly_2007_real$hotspot < 0] <- 0

cumheat2007_real <- weekly_2007_real %>%
  arrange(site, year, week) %>%
  group_by(site) %>%
  mutate(cum_heat_real = roll_sum(hotspot, 12, align = "right", fill = NA))


# combine real and predicted weekly temperature
weekly_2007_fill$site_lter<-ifelse(weekly_2007_fill$site=="fill01", "LTER01", 
                                   ifelse(weekly_2007_fill$site=="fill02", "LTER02",
                                          ifelse(weekly_2007_fill$site=="fill03", "LTER03",
                                                 ifelse(weekly_2007_fill$site=="fill04", "LTER04",
                                                        ifelse(weekly_2007_fill$site=="fill05", "LTER05",
                                                               ifelse(weekly_2007_fill$site=="fill06", "LTER06", NA))))))

weekly2007 <- left_join(weekly_2007_real[c('site','year','week','temp_c')], weekly_2007_fill[c('site_lter','year','week','temp_c')],by=c('site'='site_lter','year'='year','week'='week'))
weekly2007_LTER1 <- weekly2007 %>% rename(temp_c_real=temp_c.x, temp_c_fill=temp_c.y) %>% filter(site=="LTER01")



kp_temp_lter1_plot<-ggplot(data=weekly2007_LTER1, aes(x=temp_c_real,y=temp_c_fill)) + 
  geom_point() + geom_abline(intercept=0,slope=1) + geom_smooth(method='lm')+
  #stat_cor(method = "pearson", label.x = 27.65, label.y = 29.58, p.accuracy = 0.0001, r.accuracy = 0.001)+
  theme_bw()+
  xlab(expression(paste('Temperature known (',~degree,'C)',sep='')))+
  ylab(expression(paste('Temperature predicted (',~degree,'C)',sep='')))+
  scale_x_continuous(limits = c(27.3, 29.8), breaks=seq(27.5,30.0, by=0.5))+
  scale_y_continuous(limits = c(27.3, 29.8), breaks=seq(27.5,30.0, by=0.5))+
  ggtitle("Weekly median temperature at LTER 1")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks = element_line(color="black"),
        axis.title = element_text(size=14),
        axis.text.x=element_text(color="black", size=14),
        axis.text.y=element_text(color="black", size=14),
        aspect.ratio=1)
#ggsave("figs/kp_temp_plot.png", width=4, height=4, units="in")
#linear model relationship between real data and filled data
temp_c_lm<-lm(temp_c_fill~temp_c_real, data=weekly2007_LTER1)
# testing if the slope is different from 1
linearHypothesis(temp_c_lm, c("(Intercept) = 0", "temp_c_real=1"), test="F")
linearHypothesis(temp_c_lm, hypothesis.matrix = c(0, 1), rhs=1)

cor.test(weekly2007_LTER1$temp_c_real, weekly2007_LTER1$temp_c_fill, method="pearson") # they are highly correlated


# combine real and predicted cum heat
cumheat2007 <- left_join(cumheat2007_real[c('site','year','week','cum_heat_real')],cumheat2007_fill[c('site','year','week','cum_heat_fill')], by=c('site','year','week'))
cumheat2007_LTER1<- cumheat2007 %>% filter(site=="LTER01")

ggplot(data=cumheat2007_LTER1, aes(x=cum_heat_real,y=cum_heat_fill)) + 
  geom_point() + geom_abline(a=0,b=1) + geom_smooth(method='lm')

#testing correlation
cor.test(cumheat2007_LTER1$cum_heat_real, cumheat2007_LTER1$cum_heat_fill, method="pearson") # they are highly correlated

# but does the slope of the line differ from 1? 
cum_heat_lm<-lm(cum_heat_fill~cum_heat_real, data=cumheat2007_LTER1)
linearHypothesis(cum_heat_lm, c("(Intercept) = 0", "cum_heat_real=1"))
linearHypothesis(cum_heat_lm, c("(Intercept) = 0", "cum_heat_real = 1"))


kp_cumheat_plot<-ggplot(data=cumheat2007, aes(x=cum_heat_real,y=cum_heat_fill)) + 
  geom_point() + geom_abline(intercept=0,slope=1) + geom_smooth(method='lm')+
  theme_bw()+
  stat_cor(method = "pearson", label.x = 0.8, label.y = 3.9, p.accuracy = 0.0001, r.accuracy = 0.001)+
  xlab('Cumulative heat stress known')+
  ylab('Cumulative heat stress predicted')+
  ggtitle("Weekly cumulative heat stress")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks = element_line(color="black"),
        axis.title = element_text(size=14),
        axis.text.x=element_text(color="black", size=14),
        axis.text.y=element_text(color="black", size=14))

corplots<-cowplot::plot_grid(kp_temp_plot, kp_cumheat_plot, align = "vh", nrow=1, labels = c("(a)", "(b)"), label_fontface = "italic")
ggsave("figs/corplots.pdf", width=8, height=4, units="in")

##------------ LTER 6 ------------
# this needs to go up top
temp2007$fill06 <- temp2007$pred06_3_4_5
ggplot(data=temp2007, aes(x=LTER06,y=pred06_3_4_5)) + geom_point() + geom_abline(a=0,b=1) + geom_smooth(method='lm')
# until here

# now this down here stays
weekly2007_LTER6 <- weekly2007 %>% rename(temp_c_real=temp_c.x, temp_c_fill=temp_c.y) %>% filter(site=="LTER06")

kp_temp_lter6_plot<-ggplot(data=weekly2007_LTER6, aes(x=temp_c_real,y=temp_c_fill)) + 
  geom_point() + geom_abline(intercept=0,slope=1) + geom_smooth(method='lm')+
  #stat_cor(method = "pearson", label.x = 27.65, label.y = 29.58, p.accuracy = 0.0001, r.accuracy = 0.001)+
  theme_bw()+
  xlab(expression(paste('Temperature known (',~degree,'C)',sep='')))+
  ylab(expression(paste('Temperature predicted (',~degree,'C)',sep='')))+
  scale_x_continuous(limits = c(27.3, 29.8), breaks=seq(27.5,30.0, by=0.5))+
  scale_y_continuous(limits = c(27.3, 29.8), breaks=seq(27.5,30.0, by=0.5))+
  ggtitle("Weekly median temperature at LTER 6")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks = element_line(color="black"),
        axis.title = element_text(size=14),
        axis.text.x=element_text(color="black", size=14),
        axis.text.y=element_text(color="black", size=14),
        aspect.ratio=1)
#linear model relationship between real data and filled data
temp_c_lter6_lm<-lm(temp_c_fill~temp_c_real, data=weekly2007_LTER6)
# testing if the slope is different from 1
linearHypothesis(temp_c_lter6_lm, c("(Intercept) = 0", "temp_c_real=1"), test="F")
linearHypothesis(temp_c_lter6_lm, hypothesis.matrix = c(0, 1), rhs=1)

cor.test(weekly2007_LTER6$temp_c_real, weekly2007_LTER6$temp_c_fill, method="pearson") # they are highly correlated


real_predicted_temps_2007<-cowplot::plot_grid(kp_temp_lter1_plot, kp_temp_lter6_plot, align = "vh", nrow=1, labels = c("(a)", "(b)"), label_fontface = "italic")
ggsave("figs/real_predicted_temps_2007.pdf", width=8, height=4, units="in")



#-------------------- create dataframe to export ----------------------------------------------

bak_out <- temp_bak_cumheat %>% 
  group_by(site) %>% 
  filter(date > '2018-12-01') %>% 
  summarise('max_heatstress'=max(cum_heat,na.rm=T)) %>% 
  mutate(Site=stringr::str_replace(site,'fill','LTER'), habitat='backreef')
bak_out

out <- rbind(bak_out[c('Site','habitat','max_heatstress')])

# export max heatstress values
write.csv(out, 'data/water_temp/cumulative_heatstress_2019_bysite_filled.csv', row.names = F)

# export data by day
write.csv(temp_bak_wide %>% select(day,starts_with('LTER'),starts_with('fill')),"data/water_temp/backreef_filled_temperature.csv",row.names=F)


#-------------------- plot figures ----------------------------------------------

# use temp_bak_long dataframe. give it a new name
thermTemp<-temp_bak_long

#changing "day" to date because we need a numaric variable for day
names(thermTemp)[names(thermTemp) == 'day'] <- 'date'

thermTemp$date_use<-thermTemp$date
thermTemp$date_use<-as.Date(thermTemp$date_use,'%Y-%m-%d')

# format time and date
thermTemp$day_mo_yr <- format(thermTemp$date_use, '%Y-%m-%d')
thermTemp$day <- format(thermTemp$date_use, '%d')
thermTemp$month <- format(thermTemp$date_use, '%m')
thermTemp$year <- format(thermTemp$date_use, '%y') 

thermTemp$day<-as.factor(thermTemp$day)
thermTemp$month<-as.factor(thermTemp$month)
thermTemp$year<-as.factor(thermTemp$year)
thermTemp$date_use<-as.Date(thermTemp$date_use, "%m/%d/%y")

# creating a new df with only data up to July 31 2018. this will become the mean line and SD
thermTemp_toAug2018<-subset(thermTemp, date_use<"2018-08-01")
thermTemp_mean<-ddply(thermTemp_toAug2018, .(day, month), summarize,
                      mean_daily_temp=mean(temp_c),
                      sd=sd(temp_c))
#creating a factor column for this year
thermTemp_mean$timeframe<-as.factor("mean")
#creating a new factor column just for plotting purposes. August-December we will call 2018, Jan-July we will call 2019
#this is just to make them plot in the correct order on this plot 
thermTemp_mean$year<-as.factor(ifelse(thermTemp_mean$month=="08"|thermTemp_mean$month=="09"|
                                        thermTemp_mean$month=="10"|thermTemp_mean$month=="11"|thermTemp_mean$month=="12", "2018", "2019"))

thermTemp_mean$date<- as.Date(with(thermTemp_mean, paste(month, day, year,sep="-")), format="%m-%d-%Y")
thermTemp_mean<-thermTemp_mean[, c("day","month","year","date","mean_daily_temp","sd","timeframe")]

# subsetting data from August 2018 to July 2019. this will become the line for the high thermal stress year
thermTemp_2019<-subset(thermTemp, date_use>="2018-08-01" & date_use<="2019-07-31")

thermTemp_2019_mean<-ddply(thermTemp_2019, .(day, month, year, date), summarize,
                                 mean_daily_temp=mean(temp_c),
                                 sd=sd(temp_c))

#creating a factor column for this year
thermTemp_2019_mean$timeframe<-as.factor("2018to2019")

thermTemp_2019_mean$year<-as.factor(ifelse(thermTemp_2019_mean$month=="08"|thermTemp_2019_mean$month=="09"|
                                             thermTemp_2019_mean$month=="10"|thermTemp_2019_mean$month=="11"|thermTemp_2019_mean$month=="12", "2018", "2019"))

thermTemp_mean_and_2019<-rbind(thermTemp_2019_mean, thermTemp_mean)

# temps2019 <-
#   tidyr::separate(
#     data = temperature.day.2019.mean,
#     col = monthday,
#     sep = "-",
#     into=c("month", "day"),
#     remove = TRUE
#   )
# 
# temps2019<-temps2019[,c(1,3,4,2,5,6,7)]
# 
# longterm.temps<-thermTemp_mean[,c(7,1,2,3,4,5,6)]
# 
# names(longterm.temps)[names(longterm.temps)=="mean_daily_temp"]<-"mean_temp_c"
# 
# temps<-rbind(temps2019, longterm.temps)

temp_patterns<-ggplot(thermTemp_mean_and_2019, aes(x=date, y=mean_daily_temp, fill=timeframe))+
  geom_hline(yintercept=29, linetype=2, color="#8B8B8B")+
  geom_ribbon(aes(ymin=mean_daily_temp - sd, ymax=mean_daily_temp + sd), alpha=.2)+
  geom_line(aes(y=mean_daily_temp, color=timeframe))+
  scale_color_manual(values=c("#FA3405","#05CBFA"), labels=c("August 2018 - July 2019", "mean through July 2018"))+
  scale_fill_manual(values=c("#FA3405","#05CBFA"), labels=c("August 2018 - July 2019", "mean through July 2018"))+
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))+
  labs(x="", y=expression("Temperature " ( degree*C)))+
  theme_classic()+
  theme(axis.text.x = element_text(colour="black", angle=-45, size=14, hjust=0), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title=element_text(size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.25,0.92), legend.title = element_blank(), legend.text=element_text(size=10))+ 
  theme(aspect.ratio = 3/4)


# simple stats for temperature and heat stress ----------------------------------------------

temp_bak_cumheat_sub<-temp_bak_cumheat
# binary column, 1 for days >29C, 0 for days <29
temp_bak_cumheat_sub$over29<-ifelse(temp_bak_cumheat_sub$temp_c > 29, 1, 0)
temp_bak_cumheat_sub$date <- as.Date(temp_bak_cumheat_sub$date, '%Y-%m-%d')
temp_bak_cumheat_sub$site<-as.factor(temp_bak_cumheat_sub$site)
temp_bak_cumheat_sub$site<-factor(temp_bak_cumheat_sub$site)
#subsetting to include only the exact first and last day above 29. temp measured to .000
temp_bak_cumheat_sub <-  filter(temp_bak_cumheat_sub, date >= '2018-07-31' & date <= '2019-08-01') 
temp_bak_cumheat_sub$week<-as.factor(temp_bak_cumheat_sub$week)
temp_bak_cumheat_sub$week<-factor(temp_bak_cumheat_sub$week)

temp_bak_cumheat_sub_sum<-temp_bak_cumheat_sub %>% group_by(week) %>% 
  summarise(temp_c_mean = mean(temp_c), over_29=sum(over29), cum_heat_mean=mean(cum_heat)) %>% ungroup()


temp_sum_day <- temp_bak_cumheat_sub %>% group_by(site) %>% summarise(temp_c_mean = mean(temp_c), over_29=sum(over29)) %>% ungroup()

sum(temp_bak_cumheat_sub$over29) #number of days >29 during the heatwave: 115
length(temp_bak_cumheat_sub$over29) #total number of days in this window: 139
temp_bak_cumheat_sub_longest<-filter(temp_bak_cumheat_sub, day >= '2019-02-28' & day <= '2019-05-01')
length(lter.day.sub.longest$over29) #consecutive days over 29: 63


heat_stress<-ggplot(temp_bak_cumheat_sub) + 
  geom_line(aes(x=date,y=cum_heat,color=Site),lwd=1)  + 
  theme_classic() +
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))+
  scale_color_manual(labels = c("LTER 1", "LTER 2", "LTER 3", "LTER 4", "LTER 5", "LTER 6"), 
                     values=c( "#A5E300", "#BF047E", "#F26389", "#6B6B9D","#06BFAD","#15266B"))+
  xlab('') +
  ylab('Cumulative Heat Stress Weeks')+
  theme_classic()+
  theme(axis.text.x = element_text(colour="black", angle=-45, size=14, hjust=0), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title=element_text(size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.15,0.73), legend.text=element_text(size=10), legend.title=element_blank())+ 
  theme(aspect.ratio = 3/4)


temp_figure<-cowplot::plot_grid(temp_patterns, heat_stress, nrow = 2, align="vh", labels=c("(a)", "(b)"))
ggsave("figs/temp_figure.pdf", width=5.5, height=8, units="in")

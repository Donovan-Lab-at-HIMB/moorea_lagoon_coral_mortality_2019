###------------------------------------------------------------------------#
# Effects of nitrogen enrichment on coral mortality depend on the intensity of heat stress
#  
# 6_plot_interactions
###------------------------------------------------------------------------#

# This script plots maps of mortality prevalence, mortality severity, and nitrogen enrichment

### Packages --------------------------------------------------------------#

library(terra)
library(sf)
library(tidyverse)
library(cowplot)
library(marmap)
library(raster)
library(rgdal)
library(ggspatial)
library(ggpubr)

### prepping data --------------------------------------------------------------#

### Import Data
moo=read_sf("data/moorea_map/moorea_outline.shp")

plot(moo)

moo #"Geometry type: MULTIPOLYGON",and dimensions on an XY coord system

#load field site data
fieldSites_df= read_csv("data/moorea_map/site_data_for_map.csv")

fieldSites_df

fieldSites_df<-fieldSites_df %>% 
  rename(
    y = Latitude.2019,
    x = Longitude.2019
  )

fieldSites_sf =  fieldSites_df %>%
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
fieldSites_sf

#load lter site data for heat stress map
lterSites_df= read_csv("data/moorea_map/LTER_Backreef_Sites_LatLon.csv")

lterSites_df

lterSites_df<-lterSites_df %>% 
  rename(
    y = lat,
    x = long
  )

lterSites_sf =  lterSites_df %>%
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
lterSites_sf


lterSites_sf$Site_type<-as.factor("temp_measured")
fieldSites_sf$Site_type<-as.factor("survey_site")
tempPlot_sf<-rbind(lterSites_sf, fieldSites_sf) #combining for plotting
tempPlot_sf$max_heatstress_fac<-as.factor(tempPlot_sf$max_heatstress)
tempPlot_sf

#### Import data, raster ----

hill = rast("data/moorea_map/hillshade_gruen.tif")
#hill = rast("data/moorea_map/hillshade.tif")
hill
plot(hill)

# hillsides
hill_df=as.data.frame(hill, xy=T)
head(hill_df)
## keep in mind that when we plot hill_df it's going to be in the coordinate ref system it came from
## if you want to plot vector data w/raster data you need to make sure they are in the same coordinate ref system
mycrs=st_crs(hill)

moo_longlat = moo %>% st_transform(crs=mycrs)
moo_longlat
#now we can plot these things together

# bathymetry---------------------------------------------------------------------


bathy<-read.table("data/moorea_map/bathy_LIDAR_Riegl_820_05m.xyz")
reg<-griddify(bathy, nlon =1000, nlat = 1000)

# transform raster for ggplot
bath.p  <-  rasterToPoints(reg)
bath_df <-  data.frame(bath.p)
colnames(bath_df) = c("x", "y", "alt")

bath_sf =  bath_df %>%
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
bath_sf=subset(bath_sf, alt>0 & alt<3)

ggplot() +
  geom_point(data = bath_df, aes(x,y),color = grey(0.9), size = 0.01, show.legend=FALSE)+
  geom_sf(data = fieldSites_sf) +
  geom_sf(data = moo)+
  theme_bw()

# ---------------------------------------------------------------------

### Pocillopora Mortality Prevalence ------------------------------------------
fieldSites_sf<-fieldSites_sf%>%arrange(desc(n_obs_Pocillopora))

poc_prevalence<-ggplot() +
  geom_point(data = bath_df, aes(x,y),color = grey(0.9), size = 0.01, show.legend=FALSE)+ #bathy layer
  geom_sf(data=moo_longlat)+
  geom_sf(data = moo)+
  geom_raster(data=hill_df, aes(x=x, y=y,fill=hillshade))+ #hillshade layer
  scale_fill_gradientn(colours=c("#454545", "#e9e9e9"))+
  guides(fill = "none")+
  geom_sf(data=moo_longlat, fill="transparent", lwd=0.1,col="#696969")+ #adding outline
  geom_sf(data = fieldSites_sf, #site points colored by severity, sized by number of corals
          aes(col = prev_Pocillopora,
              size=n_obs_Pocillopora))+
  scale_color_gradientn(colours=c("#14EB85", "#C7387C", "#EB147A"), limits = c(0,100), labels = c("0%","25%","50%","75%","100%"), guide="none")+
  scale_size(breaks=c(50,100,150))+
  labs(color = "Prevalence", size="Corals\nsurveyed")+
  #ggtitle("Pocillopora mortality prevalence")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.ticks = element_blank())+  
  theme(legend.title = element_text(size=9), legend.text = element_text(size=9))+
  theme(legend.direction = "vertical", legend.box = "vertical",legend.justification="bottom")+
  theme(legend.position=c(0.22,0))+
  theme(strip.background = element_rect(colour="white"), 
        legend.background = element_rect(color = NA, fill = NA))
  #theme(legend.position = "none") #comment this out and rerun to grab the legend

# legend_poc_prevalence <- get_legend(poc_prevalence)
# as_ggplot(legend_poc_prevalence)
# ggsave("figs/legend_poc_prevalence.pdf", width=2, height=2, units="in")

### Acropora Mortality Prevalence ----------------------------------------------

fieldSites_sf<-fieldSites_sf%>%arrange(desc(n_obs_Acropora))

acr_prevalence<-ggplot() +
  geom_point(data = bath_df, aes(x,y),color = grey(0.9), size = 0.01, show.legend=FALSE)+ #bathy layer
  geom_sf(data=moo_longlat)+
  geom_sf(data = moo)+
  geom_raster(data=hill_df, aes(x=x, y=y,fill=hillshade))+ #hillshade layer
  scale_fill_gradientn(colours=c("#454545", "#e9e9e9"))+
  guides(fill = "none")+
  geom_sf(data=moo_longlat, fill="transparent", lwd=0.1,col="#696969")+ #adding outline
  geom_sf(data = fieldSites_sf, #site points colored by severity, sized by number of corals
          aes(col = prev_Acropora,
              size=n_obs_Acropora))+
  scale_color_gradientn(colours=c("#14EB85", "#C7387C", "#EB147A"), limits = c(0,100), labels = c("0%","25%","50%","75%","100%"))+
  scale_size(breaks=c(15,30,45))+
  labs(color = "Prevalence", size="Corals\nsurveyed")+
  #ggtitle("Acropora mortality prevalence")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.ticks = element_blank())+  
  theme(legend.title = element_text(size=9), legend.text = element_text(size=9))+
  theme(legend.direction = "vertical", legend.box = "vertical",legend.justification="bottom")+
  #theme(legend.position=c(0.22,0))+
  theme(strip.background = element_rect(colour="white"), 
        legend.background = element_rect(color = NA, fill = NA))
  #theme(legend.position = "none") #comment this out and rerun to grab the legend

# legend_acr_prevalence <- get_legend(acr_prevalence)
# as_ggplot(legend_acr_prevalence)
# ggsave("figs/legend_acr_prevalence.pdf", width=2, height=2, units="in")



### Pocillopora Mortality Severity ----------------------------------------------

# old red blue mort severity palette: "#28D1D7", "#D72E28"

# Pocillopora
fieldSites_sf<-fieldSites_sf%>%arrange(desc(n_dead_Poc))

poc_severity<-ggplot() +
  geom_point(data = bath_df, aes(x,y),color = grey(0.9), size = 0.01, show.legend=FALSE)+ #bathy layer
  geom_sf(data=moo_longlat)+
  geom_sf(data = moo)+
  geom_raster(data=hill_df, aes(x=x, y=y,fill=hillshade))+ #hillshade layer
  scale_fill_gradientn(colours=c("#454545", "#e9e9e9"))+
  guides(fill = "none")+
  geom_sf(data=moo_longlat, fill="transparent", lwd=0.1,col="#696969")+ #adding outline
  geom_sf(data = fieldSites_sf, #site points colored by severity, sized by number of corals
          aes(col = meanSevPoc,
              size=n_dead_Poc))+
  scale_color_gradientn(colours=c("#AFF50A", "#500AF5"), limits = c(0,100), labels = c("0%","25%","50%","75%","100%"), guide="none")+
  labs(color = "Severity", size="Corals\nsurveyed      ")+
  #ggtitle("Pocillopora mortality severity")+  
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.ticks = element_blank())+  
  theme(legend.title = element_text(size=9), legend.text = element_text(size=9))+
  theme(legend.direction = "vertical", legend.box = "vertical",legend.justification="bottom")+
  theme(legend.position=c(0.22,0))+
  theme(strip.background = element_rect(colour="white"), 
        legend.background = element_rect(color = NA, fill = NA))
  #theme(legend.position = "right") #comment this out and rerun to grab the legend

# legend_poc_severity <- get_legend(poc_severity)
# as_ggplot(legend_poc_severity)
# ggsave("figs/legend_poc_severity.pdf", width=2, height=2, units="in")

### Acropora Mortality Severity ------------------------------------------------

fieldSites_sf<-fieldSites_sf%>%arrange(desc(n_dead_Acr))

acr_severity<-ggplot() +
  geom_point(data = bath_df, aes(x,y),color = grey(0.9), size = 0.01, show.legend=FALSE)+ #bathy layer
  geom_sf(data=moo_longlat)+
  geom_sf(data = moo)+
  geom_raster(data=hill_df, aes(x=x, y=y,fill=hillshade))+ #hillshade layer
  scale_fill_gradientn(colours=c("#454545", "#e9e9e9"))+
  guides(fill = "none")+
  geom_sf(data=moo_longlat, fill="transparent", lwd=0.1,col="#696969")+ #adding outline
  geom_sf(data = fieldSites_sf, #site points colored by severity, sized by number of corals
          aes(col = meanSevAcr,
              size=n_dead_Acr))+
  scale_color_gradientn(colours=c("#AFF50A", "#500AF5"), limits = c(0,100), labels = c("0%","25%","50%","75%","100%"), guide="none")+
  labs(color = "Severity", size="Corals\nsurveyed")+
  #ggtitle("Acropora mortality severity")+
  theme(plot.title = element_text(hjust = 0.5))+
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(0.4, "in"), width = unit(0.4, "in"), 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.25, "in"))+ #north arrow
  annotation_scale(location = "br", pad_x = unit(0.1, "cm"),pad_y = unit(0.1, "cm")) + #scale bar
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.ticks = element_blank())+  
  theme(legend.title = element_text(size=9), legend.text = element_text(size=9))+
  theme(legend.direction = "vertical", legend.box = "vertical",legend.justification="bottom")+
  theme(legend.position=c(0.22,0))+
  theme(strip.background = element_rect(colour="white"), 
        legend.background = element_rect(color = NA, fill = NA))
  #theme(legend.position = "none") #comment this out and rerun to grab the legend
#ggsave("figs/acropora_severity.pdf", width=6.5, height=4.5, units="in")
# returning warning about removing 5 rows b/c there were 5 sites that had no acroproa mortality


# legend_acr_severity <- get_legend(acr_severity)
# as_ggplot(legend_acr_severity)
# ggsave("figs/legend_acr_severity.pdf", width=2, height=2, units="in")

### 4 panel plot ---------------------------------------------------------------

map_fig_2<-cowplot::plot_grid(poc_prevalence, poc_severity, acr_prevalence, acr_severity,
                            scale = 0.9,nrow = 2, align = "vh", labels = c("(a)", "(b)", "(c)", "(d)"))
#ggsave("figs/map_fig.pdf", width=12, height=8.75, units="in")
ggsave("figs/map_fig_2.tiff", width=12, height=10, units="in", compression = "lzw")


map_fig_3<-cowplot::plot_grid(poc_prevalence, acr_prevalence, poc_severity,  acr_severity,
                              scale = 0.9,nrow = 2, align = "vh", labels = c("(a)", "(b)", "(c)", "(d)"))
ggsave("figs/map_fig_3.tiff", width=12, height=10, units="in", compression = "lzw")



### grab legend ----------------------------------------------------------------

severity_legend<-ggplot() +
  geom_sf(data = fieldSites_sf, #site points colored by severity, sized by number of corals
          aes(col = meanSevAcr,
              size=n_dead_Acr))+
  scale_color_gradientn(colours=c("#AFF50A", "#500AF5"), limits = c(0,100), labels = c("0%","25%","50%","75%","100%"))+
  labs(color = "Severity", size="Corals\nsurveyed")+
  #ggtitle("Acropora mortality severity")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.ticks = element_blank())+  
  theme(legend.title = element_text(size=9), legend.text = element_text(size=9))+
  theme(legend.direction = "vertical", legend.box = "horizontal",legend.justification="bottom")+
  theme(legend.position=c(0.22,0))+
  theme(strip.background = element_rect(colour="white"), 
        legend.background = element_rect(color = NA, fill = NA))

legend_acr_severity <- get_legend(severity_legend)
legend_acr_severity<-ggpubr::as_ggplot(legend_acr_severity)
ggsave("figs/legend_acr_severity.pdf", width=2, height=2, units="in")

prevalence_legend<-ggplot() +
  geom_sf(data = fieldSites_sf, #site points colored by severity, sized by number of corals
          aes(col = meanSevAcr,
              size=n_dead_Acr))+
  scale_color_gradientn(colours=c("#14EB85", "#C7387C", "#EB147A"), limits = c(0,100), labels = c("0%","25%","50%","75%","100%"))+
  labs(color = "Severity", size="Corals\nsurveyed")+
  #ggtitle("Acropora mortality severity")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.ticks = element_blank())+  
  theme(legend.title = element_text(size=9), legend.text = element_text(size=9))+
  theme(legend.direction = "vertical", legend.box = "horizontal",legend.justification="bottom")+
  theme(legend.position=c(0.22,0))+
  theme(strip.background = element_rect(colour="white"), 
        legend.background = element_rect(color = NA, fill = NA))

legend_acr_prevalence <- get_legend(prevalence_legend)
legend_acr_prevalence<-ggpubr::as_ggplot(legend_acr_prevalence)
ggsave("figs/legend_acr_prevalence.pdf", width=2, height=2, units="in")


### map of nitrogen enrichment ----------------------------------------------------------------

fieldSites_sf<-fieldSites_sf%>% arrange(meanN)

nitrogen_fig<-ggplot() +
  geom_point(data = bath_df, aes(x,y),color = grey(0.9), size = 0.01, show.legend=FALSE)+ #bathy layer
  geom_sf(data=moo_longlat)+
  geom_sf(data = moo)+
  geom_raster(data=hill_df, aes(x=x, y=y,fill=hillshade))+
  scale_fill_gradientn(colours=c("#454545", "#e9e9e9"))+ #hillshade fill
  guides(fill = "none", size="none")+
  geom_sf(data = fieldSites_sf,
          aes(col = meanN,size=2))+
  geom_point(data=lterSites_df, aes(x,y), size=6, pch=0)+
  #scale_color_gradientn(colours=c("#BEEE62", "#386641"))+ #point color
  scale_color_gradientn(colours=c("#bf0fff", "#cbff49"))+ #point color
  labs(color = "% Nitrogen")+
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(0.4, "in"), width = unit(0.4, "in"), 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.25, "in"))+ #north arrow
  annotation_scale(location = "br", pad_x = unit(0.1, "cm"),pad_y = unit(0.1, "cm")) + #scale bar
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.ticks = element_blank())+
  theme(legend.title = element_text(size=9), legend.text = element_text(size=9))+
  theme(strip.background = element_rect(colour="white"),legend.position=c(.09,.2))
ggsave("figs/nitrogen_fig.pdf", width=6.5, height=4.5, units="in")

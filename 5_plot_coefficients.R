###------------------------------------------------------------------------#
# Effects of nitrogen enrichment on coral mortality depend on the intensity of heat stress
#  
# 5_plot_coefficients
###------------------------------------------------------------------------#

# This script plots model coefficients

### Packages --------------------------------------------------------------#

library(ggplot2)
library(cowplot)

###  Pocillopora Mortality Prevalence -------------------------------------

#inputting results from the model
prev_poc_allsize<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Pocillopora_allSize_beta80_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_poc_1Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Pocillopora_1Size_beta80_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_poc_3Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Pocillopora_3Size_beta80_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_poc_4Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Pocillopora_4Size_beta80_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)

prev_poc_allsize$Size<-as.factor("All")
prev_poc_1Size$Size<-as.factor("Size1and2")
prev_poc_3Size$Size<-as.factor("Size3")
prev_poc_4Size$Size<-as.factor("Size4")

prev_poc<-rbind(prev_poc_allsize, prev_poc_1Size, prev_poc_3Size, prev_poc_4Size)


prev_poc<-subset(prev_poc, factor_name!="beta_colony_depth")
prev_poc$factor_name<-factor(prev_poc$factor_name, levels=c("beta_cumheat_X_N","beta_cumheat","beta_N"))
prev_poc$Size<-factor(prev_poc$Size, levels=c("Size4","Size3","Size1and2","All"))

levels(prev_poc$Size)
levels(prev_poc$factor)

# plot coefficients for pocillopora mortality prevalence
prevalence_poc<-ggplot()+
  geom_vline(xintercept=0, linetype="dashed", color="darkgray")+
  geom_errorbar(data=prev_poc, aes(x=beta, y=factor_name, color=Size, xmin=beta_down95, xmax=beta_up95, width = 0), position = position_dodge(0.75))+
  geom_errorbar(data=prev_poc, aes(x=beta,y=factor_name,xmin=beta_down80, xmax=beta_up80, width = 0, color=Size),position = position_dodge(0.75), size=1.75)+
  geom_point(data=prev_poc, aes(x=beta, y=factor_name, fill=Size),position = position_dodge(0.75), size=3, shape=21, color="black")+
  ggtitle(expression(paste(italic("Pocillopora"), " mortality prevalence")))+
  xlab("Effect")+
  ylab("")+
  scale_y_discrete(labels = c("Heat stress x \nNitrogen enrichment", "Heat stress", "Nitrogen enrichment"))+
  scale_color_manual(values = c("#6AFC90", "#47C367", "#238B3E", "#005215"), guide = guide_legend(reverse = TRUE),
                     labels = c("30+ cm", "20-29 cm","5-19 cm","All"))+
  scale_fill_manual(values = c("#6AFC90", "#47C367", "#238B3E", "#005215"), guide = guide_legend(reverse = TRUE),
                     labels = c("30+ cm", "20-29 cm","5-19 cm","All"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=14))+
  theme(plot.title = element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=12))



### Acropora Mortality Prevalence -------------------------------------------------------

#inputting results from the model
prev_acr_allsize<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Acropora_allSize_beta80_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_acr_1Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Acropora_1Size_beta80_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_acr_3Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Acropora_3Size_beta80_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_acr_4Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Acropora_4Size_beta80_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)

prev_acr_allsize$Size<-as.factor("All")
prev_acr_1Size$Size<-as.factor("Size1and2")
prev_acr_3Size$Size<-as.factor("Size3")
prev_acr_4Size$Size<-as.factor("Size4")

prev_acr<-rbind(prev_acr_allsize, prev_acr_1Size, prev_acr_3Size, prev_acr_4Size)

prev_acr<-subset(prev_acr, factor_name!="beta_colony_depth")
prev_acr$factor_name<-factor(prev_acr$factor_name, levels=c("beta_cumheat_X_N","beta_cumheat","beta_N"))
prev_acr$Size<-factor(prev_acr$Size, levels=c("Size4","Size3","Size1and2","All"))

# old blue gradient w/black: "#055C9D", "#189AB4", "#75E6DA", "black"
# "#75E6DA", "#50B8C6", "#2A8AB1", "#055C9D"

# plot coefficients for Acropora mortality prevalence
prevalence_acr<-ggplot()+
  geom_vline(xintercept=0, linetype="dashed", color="darkgray")+
  geom_errorbar(data=prev_acr, aes(x=beta, y=factor_name, color=Size, xmin=beta_down95, xmax=beta_up95, width = 0), position = position_dodge(0.75))+
  geom_errorbar(data=prev_acr, aes(x=beta,y=factor_name,xmin=beta_down80, xmax=beta_up80, width = 0, color=Size),position = position_dodge(0.75), size=1.75)+
  geom_point(data=prev_acr, aes(x=beta, y=factor_name, fill=Size),position = position_dodge(0.75), size=3, shape=21, color="black")+
  ggtitle(expression(paste(italic("Acropora"), " mortality prevalence")))+
  xlab("Effect")+
  ylab("")+
  scale_y_discrete(labels = c("", "", ""))+
  scale_color_manual(values = c("#00D4FF", "#01A0CB", "#016D96", "#023962"), guide = guide_legend(reverse = TRUE),
                     labels = c("60+ cm", "30-59 cm","5-29 cm","All"))+
  scale_fill_manual(values = c("#00D4FF", "#01A0CB", "#016D96", "#023962"), guide = guide_legend(reverse = TRUE),
                     labels = c("60+ cm", "30-59 cm","5-29 cm","All"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(plot.title = element_text(size=16))+
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=12))


###  Pocillopora Mortality Severity --------------------------------------------

#inputting results from the model
sev_poc_allsize<-read.csv("./model_out/Nsubmodel/Percent_dead/sev_Pocillopora_allSize_beta80_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)

sev_poc_allsize$Size<-as.factor("All")

sev_poc<-sev_poc_allsize

sev_poc<-subset(sev_poc, factor_name!="beta_colony_depth")
sev_poc$factor_name<-factor(sev_poc$factor_name, levels=c("beta_cumheat_X_N","beta_cumheat","beta_N"))
sev_poc$Size<-factor(sev_poc$Size, levels=c("All"))

# plot coefficients for Pocillopora mortality severity
severity_poc<-ggplot(sev_poc, aes(x=beta, y=factor_name, color=Size))+
  geom_vline(xintercept=0, linetype="dashed", color="darkgray")+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=beta_down95, xmax=beta_up95, width = 0))+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=beta_down80, xmax=beta_up80, width = 0), size=1.75)+
  geom_point(aes(fill=Size), position = position_dodge(0.75), size=3, shape=21, color="black")+
  #ggtitle(expression(italic("Pocillopora")))+
  ggtitle(expression(paste(italic("Pocillopora"), " mortality severity")))+
  xlab("Effect")+
  ylab("")+
  scale_y_discrete(labels = c("Heat stress x \nNitrogen enrichment", "Heat stress", "Nitrogen enrichment"))+
  scale_color_manual(values = c("#005215"), guide = guide_legend(reverse = TRUE),
                     labels = c("All"))+
  scale_fill_manual(values = c("#005215"), guide = guide_legend(reverse = TRUE),
                     labels = c("All"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=14))+
  theme(plot.title = element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=12))



###  Acropora Mortality Severity -----------------------------------------------

#inputting results from the model
sev_acr_allsize<-read.csv("./model_out/Nsubmodel/Percent_dead/sev_Acropora_allSize_beta80_quantiles.csv", header=TRUE, stringsAsFactors = TRUE) # beta_Acropora_all doesn't exist! why?

sev_acr_allsize$Size<-as.factor("All")

sev_acr<-sev_acr_allsize


sev_acr<-subset(sev_acr, factor_name!="beta_colony_depth")
sev_acr$factor_name<-factor(sev_acr$factor_name, levels=c("beta_cumheat_X_N","beta_cumheat","beta_N"))
sev_acr$Size<-factor(sev_acr$Size, levels=c("All"))

# plot coefficients for Acropora mortality severity
severity_acr<-ggplot(sev_acr, aes(x=beta, y=factor_name, color=Size))+
  geom_vline(xintercept=0, linetype="dashed", color="darkgray")+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=beta_down95, xmax=beta_up95, width = 0),lineend="round")+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=beta_down80, xmax=beta_up80, width = 0), size=1.75, lineend="round")+
  geom_point(aes(fill=Size), position = position_dodge(0.75), size=3, shape=21, color="black")+
  #ggtitle(expression(italic("Acropora")))+
  ggtitle(expression(paste(italic("Acropora"), " mortality severity")))+
  xlab("Effect")+
  ylab("")+
  scale_y_discrete(labels = c("", "", ""))+
  scale_color_manual(values = c("#023962"), guide = guide_legend(reverse = TRUE),
                     labels = c("All"))+
  scale_fill_manual(values = c("#023962"), guide = guide_legend(reverse = TRUE),
                     labels = c("All"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(plot.title = element_text(size=16))+
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  theme(legend.text=element_text(size=12))


## Plotting together------------------------------------------------------------


poc_plots<-cowplot::plot_grid(prevalence_poc, severity_poc, ncol=1, align="vh",rel_heights=c(1 ,0.5), labels=c('(a)', '(c)'))
acr_plots<-cowplot::plot_grid(prevalence_acr, severity_acr, ncol=1, align="vh",rel_heights=c(1 ,0.5), labels=c('(b)', '(d)'))

poc_acr_prev_sev_plot<-cowplot::plot_grid(poc_plots, acr_plots, rel_widths=c(1,0.7), align='h')
ggsave("figs/poc_acr_prev_sev_plot.pdf", width=12, height=10, units="in")


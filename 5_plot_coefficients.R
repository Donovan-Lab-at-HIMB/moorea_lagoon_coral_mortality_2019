library(ggplot2)
library(cowplot)

##----------------------------- Pocillopora Prevalence -----------------------------

#inputting results from the model
prev_poc_allsize<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Pocillopora_allSize_beta_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_poc_1Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Pocillopora_1Size_beta_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_poc_3Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Pocillopora_3Size_beta_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_poc_4Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Pocillopora_4Size_beta_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)

prev_poc_allsize$Size<-as.factor("All")
prev_poc_1Size$Size<-as.factor("Size1and2")
prev_poc_3Size$Size<-as.factor("Size3")
prev_poc_4Size$Size<-as.factor("Size4")

prev_poc<-rbind(prev_poc_allsize, prev_poc_1Size, prev_poc_3Size, prev_poc_4Size)

prev_poc <- prev_poc %>% 
  rename("factor"="X",
         "quant2.5"= "X2.5.",
         "quant25"="X25.",
         "quant50"="X50.",
         "quant75"= "X75.",
         "quant97.5"="X97.5.")


prev_poc<-subset(prev_poc, factor!="beta_colony_depth")
prev_poc$factor<-factor(prev_poc$factor, levels=c("beta_cumheat_X_N","beta_cumheat","beta_N"))
prev_poc$Size<-factor(prev_poc$Size, levels=c("Size4","Size3","Size1and2","All"))

prevalence_poc<-ggplot(subset(prev_poc, Size=="All"), aes(x=quant50, y=factor, color=Size))+
  geom_vline(xintercept=0, linetype="dashed", color="darkgray")+
  geom_point(position = position_dodge(0.75), size=2)+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=quant2.5, xmax=quant97.5, width = 0))+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=quant25, xmax=quant75, width = 0), size=1)+
  ggtitle(expression(italic("Pocillopora")))+
  xlab("Effect")+
  ylab("")+
  scale_y_discrete(labels = c("Heat x N", "Heat", "N"))+
  #scale_color_manual(values = c("#B8190F", "#FE5F55", "#FF9086", "black"), guide = guide_legend(reverse = TRUE),
  #                   labels = c("30+ cm", "20-29 cm","5-19 cm","All"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title.x=element_text(size=16), axis.title.y=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=12))

levels(prev_poc$Size)
levels(prev_poc$factor)
##----------------------------- Acropora Prevalence -----------------------------

#inputting results from the model
prev_acr_allsize<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Acropora_allSize_beta_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_acr_1Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Acropora_1Size_beta_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_acr_3Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Acropora_3Size_beta_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)
prev_acr_4Size<-read.csv("./model_out/Nsubmodel/Percent_dead/prev_Acropora_4Size_beta_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)

prev_acr_allsize$Size<-as.factor("All")
prev_acr_1Size$Size<-as.factor("Size1and2")
prev_acr_3Size$Size<-as.factor("Size3")
prev_acr_4Size$Size<-as.factor("Size4")

prev_acr<-rbind(prev_acr_allsize, prev_acr_1Size, prev_acr_3Size, prev_acr_4Size)

prev_acr <- prev_acr %>% 
  rename("factor"="X",
         "quant2.5"= "X2.5.",
         "quant25"="X25.",
         "quant50"="X50.",
         "quant75"= "X75.",
         "quant97.5"="X97.5.")

prev_acr<-subset(prev_acr, factor!="beta_colony_depth")
prev_acr$factor<-factor(prev_acr$factor, levels=c("beta_cumheat_X_N","beta_cumheat","beta_N"))
prev_acr$Size<-factor(prev_acr$Size, levels=c("Size4","Size3","Size1and2","All"))

prevalence_acr<-ggplot(prev_acr, aes(x=quant50, y=factor, color=Size))+
  geom_vline(xintercept=0, linetype="dashed", color="darkgray")+
  geom_point(position = position_dodge(0.75), size=2)+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=quant2.5, xmax=quant97.5, width = 0))+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=quant25, xmax=quant75, width = 0), size=1)+
  ggtitle(expression(italic("Acropora")))+
  xlab("Effect")+
  ylab("")+
  scale_y_discrete(labels = c("Heat x N", "Heat", "N"))+
  scale_color_manual(values = c("#055C9D", "#189AB4", "#75E6DA", "black"), guide = guide_legend(reverse = TRUE),
                     labels = c("60+ cm", "30-59 cm","5-29 cm","All"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title.x=element_text(size=16), axis.title.y=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=12))


##----------------------------- Pocillopora Severity -----------------------------

#inputting results from the model
sev_poc_allsize<-read.csv("./model_out/Nsubmodel/Percent_dead/sev_Pocillopora_allSize_beta_quantiles.csv", header=TRUE, stringsAsFactors = TRUE)

sev_poc_allsize$Size<-as.factor("All")

sev_poc<-sev_poc_allsize

sev_poc <- sev_poc %>% 
  rename("factor"="X",
         "quant2.5"= "X2.5.",
         "quant25"="X25.",
         "quant50"="X50.",
         "quant75"= "X75.",
         "quant97.5"="X97.5.")

sev_poc<-subset(sev_poc, factor!="beta_colony_depth")
sev_poc$factor<-factor(sev_poc$factor, levels=c("beta_cumheat_X_N","beta_cumheat","beta_N"))
sev_poc$Size<-factor(sev_poc$Size, levels=c("All"))

severity_poc<-ggplot(sev_poc, aes(x=quant50, y=factor, color=Size))+
  geom_vline(xintercept=0, linetype="dashed", color="darkgray")+
  geom_point(position = position_dodge(0.75), size=2)+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=quant2.5, xmax=quant97.5, width = 0))+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=quant25, xmax=quant75, width = 0), size=1)+
  ggtitle(expression(italic("Pocillopora")))+
  xlab("Effect")+
  ylab("")+
  scale_y_discrete(labels = c("Heat x N", "Heat", "N"))+
  scale_color_manual(values = c("black"), guide = guide_legend(reverse = TRUE),
                     labels = c("All"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title.x=element_text(size=16), axis.title.y=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=12))



##----------------------------- Acropora Severity -----------------------------

#inputting results from the model
sev_acr_allsize<-read.csv("./model_out/Nsubmodel/Percent_dead/sev_Acropora_allSize_beta_quantiles.csv", header=TRUE, stringsAsFactors = TRUE) # beta_Acropora_all doesn't exist! why?

sev_acr_allsize$Size<-as.factor("All")

sev_acr<-sev_acr_allsize

sev_acr <- sev_acr %>% 
  rename("factor"="X",
         "quant2.5"= "X2.5.",
         "quant25"="X25.",
         "quant50"="X50.",
         "quant75"= "X75.",
         "quant97.5"="X97.5.")

sev_acr<-subset(sev_acr, factor!="beta_colony_depth")
sev_acr$factor<-factor(sev_acr$factor, levels=c("beta_cumheat_X_N","beta_cumheat","beta_N"))
beta_acr$Size<-factor(beta_acr$Size, levels=c("All"))

severity_acr<-ggplot(sev_acr, aes(x=quant50, y=factor, color=Size))+
  geom_vline(xintercept=0, linetype="dashed", color="darkgray")+
  geom_point(position = position_dodge(0.75), size=2)+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=quant2.5, xmax=quant97.5, width = 0))+
  geom_errorbar(position = position_dodge(0.75),aes(xmin=quant25, xmax=quant75, width = 0), size=1)+
  ggtitle(expression(italic("Acropora")))+
  xlab("Effect")+
  ylab("")+
  scale_y_discrete(labels = c("Heat x N", "Heat", "N"))+
  scale_color_manual(values = c("black"), guide = guide_legend(reverse = TRUE),
                     labels = c("All"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(colour="black", size=14), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title.x=element_text(size=16), axis.title.y=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks = element_line(color="black"))+
  theme(legend.text=element_text(size=12))


##----------------------------- Plots -----------------------------

#Prevalence

prev_title <- ggdraw() + draw_label("Mortality Prevalence", fontface='bold')
prev_plots<-cowplot::plot_grid(prevalence_poc, prevalence_acr, ncol=2, align="VH", labels=c('A', 'B'))
#prevalence_plot<-cowplot::plot_grid(prev_title, prev_plots, ncol=1, rel_heights=c(0.1, 1))
#ggsave("figs/mortality_prevalence_plot.pdf", width=12, height=6, units="in")

#Severity

sev_title <- ggdraw() + draw_label("Mortality Severity", fontface='bold')
sev_plots<-cowplot::plot_grid(severity_poc, severity_acr, ncol=2, align="VH", labels=c('C', 'D'))
#severity_plot<-cowplot::plot_grid(sev_title, sev_plots, ncol=1, rel_heights=c(0.1, 1))
#ggsave("figs/mortality_severity_plot.pdf", width=12, height=6, units="in")

## together as one plot

poc_acr_prev_sev_plot<-cowplot::plot_grid(prev_title, prev_plots, ncol=1, sev_title, sev_plots, rel_heights=c(0.1, 1, 0.1, 0.5))
ggsave("figs/poc_acr_prev_sev_plot.pdf", width=12, height=9, units="in")

#cowplot::plot_grid(prevalence_poc, prevalence_acr, severity_poc, severity_acr, ncol=2, align="VH", labels=c('A', 'B', 'C', 'D'))

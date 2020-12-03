### Load packages####
library(readr)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(tidyverse)
library(brms)
library(loo)
library(rstanarm)
library(StanHeaders)
library(Rcpp)
library(rstan)
library(parallel)
library(bayesplot)
library(ggplot2)
library(rcartocolor)
library(ggthemes)
library(ggmcmc)
### define theme ggplot
th = theme_classic()+
  theme(
    panel.grid.major = element_blank(),
    aspect.ratio = 0.75,
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size = 0.75),
    text = element_text(family = "sans"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.key.size=unit(0.75,'lines'),
    legend.key.height=unit(1,"lines"),
    legend.text=element_text(size=12),
    strip.text = element_text(size = 12),
    strip.background = element_rect(colour=NA, fill=NA))
colors = color_scheme_set("mix-blue-red")


#### Reload models with the readRDS() function####
fit0= readRDS("fit0.rds")
fitP1= readRDS("fitP1.rds")
fitP2= readRDS("fitP2.rds")
fitP3= readRDS("fitP3.rds")


fitP1tranformed <- ggs(fitP1)  # the ggs function transforms the brms output into a longformat tibble, 
                                #that we can use to make different types of plots.
fitP1tranformed$Parameter <- factor(fitP1tranformed$Parameter,
              levels = c("b_a_Intercept", "b_m_Intercept","sigma"),
              labels = c("a: attack rate", "m: mutual interference", "Residual standard deviation"))
fitP1tranformed= fitP1tranformed %>% drop_na()
####   Trace Plot P1 ######
p1 = ggplot(filter(fitP1tranformed, Parameter %in% 
      c("a: attack rate", "m: mutual interference", "Residual standard deviation")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 500, linetype="longdash")+
  facet_wrap(Parameter ~ . ,
             scale  = 'free_y',
             strip.position="top") +
               th +
  scale_color_manual(values =c( "#B97C7C", "#03396c", "#d1e1ec" )) +
 # scale_color_grey(start = 0.3, end = 0.5,)+
 #scale_color_brewer(palette = color) +
 #scale_colour_wsj("colors6")+
  #scale_color_grey() +
 # scale_colour_viridis_d()+
  labs(title = "Trace plots P1: linear", 
       col   = "Chains")
p1
### Before run this part of code, load R script "F_modify_facet_scales"
p1_modify <- p1 +
  facet_wrap_custom(Parameter ~ . , scales = "free_y", scale_overrides = list (
    scale_override(1, scale_y_continuous(limits  = c(0, 4), breaks = seq(0, 4, 0.75))),
    
    scale_override(2, scale_y_continuous(limits  = c(-2, 1), breaks = seq(-2, 1, 0.5))),
    
    scale_override(3, scale_y_continuous(limits  = c(0.1,0.5), breaks = seq(0.1, 0.5, 0.075)))
  ))
p1_modify

##### Saving P1 trace plot #####
#Set new windo size and replot whatever plot you just made. 
dev.new(width = 10, height = 7.5, unit="in", noRStudioGD = T)
last_plot() 
#Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "FigureS3.png",
       plot=p1_modify, 
       device="png",
       path ="outputs_results/supplementary_materials/",
       width = dev.size()[1],
       height = dev.size()[2],
       dpi = 600)

dev.off()

##### Trace plots P2######
fitP2tranformed <- ggs(fitP2)  # the ggs function transforms the brms output into a longformat tibble, 
#that we can use to make different types of plots.
fitP2tranformed$Parameter <- factor(fitP2tranformed$Parameter,
                                    levels = c("b_a_Intercept", "b_h_Intercept" ,"b_m_Intercept","sigma"),
                                    labels = c("a: attack rate","h: handling time", "m: mutual interference", "Residual standard deviation"))
fitP2tranformed= fitP2tranformed %>% drop_na()
#  Trace Plot P2 #
p2= ggplot(filter(fitP2tranformed, Parameter %in% 
                    c("a: attack rate","h: handling time",
                      "m: mutual interference", "Residual standard deviation")),
           aes(x   = Iteration,
               y   = value, 
               col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 500, linetype="longdash")+
  facet_wrap(Parameter ~ . ,
             scale  = 'free_y',
             strip.position="top") +
  th +
  scale_color_manual(values =c( "#B97C7C", "#03396c", "#d1e1ec" )) +
  #scale_color_brewer(palette = "Spectral")+
  #scale_colour_wsj("colors6")+
  #scale_color_grey() +
  #scale_colour_viridis_d()+
  labs(title = "Trace plots P2: hyperbolic", 
       col   = "Chains")
p2

p2_modify <- p2 +
  facet_wrap_custom(Parameter ~ . , scales = "free_y", scale_overrides = list (
    scale_override(1, scale_y_continuous(limits  = c(0, 0.75), breaks = seq(0, 0.75, 0.15))),
    
    scale_override(2, scale_y_continuous(limits  = c(0, 75), breaks = seq(0, 75, 15))),
    
    scale_override(3, scale_y_continuous(limits  = c(-0.5, 2.5), breaks = seq(-0.5, 2.5, 0.5))),
    
    scale_override(4, scale_y_continuous(limits  = c(0.0015, 0.015), breaks = seq(0.0015, 0.015, 0.0025)))
  ))
p2_modify
##### Saving P2 trace plot #####
#Set new windo size and replot whatever plot you just made. 
dev.new(width = 10, height = 7.5, unit="in", noRStudioGD = T)
last_plot() 
#Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "P2_trace_plots.png",
       plot=p2_modify, 
       device="png",
       path ="outputs_results/supplementary_materials/",
       width = dev.size()[1],
       height = dev.size()[2],
       dpi = 600)

dev.off()

##### P3 trace plots  ######
fitP3tranformed <- ggs(fitP3)  # the ggs function transforms the brms output into a longformat tibble, 
#that we can use to make different types of plots.
fitP3tranformed$Parameter <- factor(fitP3tranformed$Parameter,
                                    levels = c("b_a_Intercept", "b_h_Intercept" ,"b_m_Intercept","sigma"),
                                    labels = c("a: attack rate","h: handling time", "m: mutual interference", "Residual standard deviation"))
fitP3tranformed= fitP3tranformed %>% drop_na()
#  Trace Plot P3 ##
p3 = ggplot(filter(fitP3tranformed, Parameter %in% 
                    c("a: attack rate","h: handling time",
                      "m: mutual interference", "Residual standard deviation")),
           aes(x   = Iteration,
               y   = value, 
               col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 500, linetype="longdash")+
  facet_wrap(Parameter ~ . ,
             scale  = 'free_y',
             strip.position="top") +
  th +
  scale_color_manual(values =c( "#B97C7C", "#03396c", "#d1e1ec" )) +
  #scale_color_brewer(palette = "Spectral")+
  #scale_colour_wsj("colors6")+
  #scale_color_grey() +
  #scale_colour_viridis_d()+
  labs(title = "Trace plots P3: sigmoidal", 
       col   = "Chains")
p3


p3_modify <- p3 +
  facet_wrap_custom(Parameter ~ . , scales = "free_y", scale_overrides = list (
    scale_override(1, scale_y_continuous(limits  = c(0, 0.75), breaks = seq(0, 0.75, 0.15))),
    
    scale_override(2, scale_y_continuous(limits  = c(0, 75), breaks = seq(0, 75, 15))),
    
    scale_override(3, scale_y_continuous(limits  = c(-0.5, 2.5), breaks = seq(-0.5, 2.5, 0.5))),
    
    scale_override(4, scale_y_continuous(limits  = c(0.0015, 0.015), breaks = seq(0.0015, 0.015, 0.0025)))
  ))
p3_modify
##### Saving P3 trace plot #####
#Set new windo size and replot whatever plot you just made. 
dev.new(width = 10, height = 7.5, unit="in", noRStudioGD = T)
last_plot() 
#Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "P3_trace_plots.png",
       plot=p3_modify, 
       device="png",
       path ="outputs_results/supplementary_materials/",
       width = dev.size()[1],
       height = dev.size()[2],
       dpi = 600)

dev.off()




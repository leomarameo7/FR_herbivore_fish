#### Load packages#######
library(brms)
library(tidyverse)
library(bayesplot)
library(brmstools)
library(tidybayes)
library(dplyr)
library(reshape2)
library(sjPlot)
##### set personal theme for plot ####

personal_theme = theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    aspect.ratio = 0.75,
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size = 0.75),
    text = element_text(family = "sans"),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    axis.text.x = element_text(colour = "black", size = 14),
    axis.text.y  = element_text(colour = "black", size = 14),
    strip.text = element_text(size = 16),
    legend.key.size = unit(1,'lines'),
    legend.key.height = unit(1,"lines"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size=16),
    legend.position = "none")

##### Load data #####
data_fit_seq <- as_tibble(read_csv("data/raw/data_raw.csv"))
# predictors as factors 
data_fit_seq$P <- as.factor(round(data_fit_seq$P, digits = 2))
data_fit_seq$N <- round(data_fit_seq$N, digits = 2)
levels(data_fit_seq$P)

model_seq <- brm(formula =  C ~ Sequence  + (Sequence |P),
                 family = hurdle_gamma(),
                 data    = data_fit_seq,
                 seed    = 123,
                 cores = getOption("mc.cores",1),
                 chains = 3,
                 warmup = 5000,
                 iter = 7000,
                 control = list(adapt_delta = 0.99, max_treedepth = 15),
                 file= "outputs_results/supplementary_materials/linear_model_sequence")

summary(model_seq)
get_variables(model_seq)

### plot estimates ###
p_seq <- mcmc_intervals(model_seq, pars = c("r_P[0.08,Sequence]" ,"r_P[0.11,Sequence]","r_P[0.13,Sequence]","r_P[0.15,Sequence]"),
                    point_est = "median", prob = 0.5, prob_outer = 0.95) +
  labs(title ="Sequence order effect size",
       x = "Regression slope estimate",
      # y = expression(Fish~density~" "~"("~ind.~m^{-2}~")")
       ) +
  scale_y_discrete(name = expression(Fish~density~" "~"("~ind.~m^{-2}~")"), 
                   labels=c("0.08","0.11","0.13","0.15"))+
  scale_x_continuous(breaks = seq(-0.3, 0.3, 0.05), 
                     labels = scales::number_format(accuracy = 0.01, decimal.mark = '.'))+
 # scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.15))+
  personal_theme
  
p_seq


ggsave(filename = "Fig_S_sequence.pdf",
       plot = p_seq, 
       width = 15,
       height = 10,
       path = "outputs_results/supplementary_materials/",
       dpi = 600)

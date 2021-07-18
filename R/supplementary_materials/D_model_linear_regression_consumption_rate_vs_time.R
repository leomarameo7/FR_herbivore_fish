#### Load packages#######
library(brms)
library(tidyverse)
library(brmstools)
library(tidybayes)
library(dplyr)
library(reshape2)
library(sjPlot)
##### Load data #####
data_fit <- as_tibble(read_csv("data/raw/data_raw.csv"))

# predictors as factors 
data_fit$P <- round(data_fit$P, digits = 2)
data_fit$N <- round(data_fit$N, digits = 2)

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
# For each combination of algae and fish densities. Retrived from  https://tem11010.github.io/regression_brms/
# Note:  it will take 5-10 minutes to fit 
model_time <- brm(formula =  C ~ Minute + ( Minute |ID_experiment) + ar(p = 1),
            
              data    = data_fit,
              seed    = 123,
              cores = getOption("mc.cores",1),
              chains = 3,
              warmup = 500,
              iter = 5000,
              control = list(adapt_delta = 0.99, max_treedepth = 15),
             file= "outputs_results/supplementary_materials/linear_model_time")

summary(model_time)


### plot estimates ####

get_variables(model_time)

p_min <- mcmc_intervals(model_time, pars = c("r_ID_experiment[1,Minute]", "r_ID_experiment[2,Minute]","r_ID_experiment[3,Minute]" , "r_ID_experiment[4,Minute]","r_ID_experiment[5,Minute]","r_ID_experiment[6,Minute]","r_ID_experiment[7,Minute]", "r_ID_experiment[8,Minute]","r_ID_experiment[9,Minute]"  , "r_ID_experiment[10,Minute]","r_ID_experiment[11,Minute]" ,"r_ID_experiment[12,Minute]","r_ID_experiment[13,Minute]" , "r_ID_experiment[14,Minute]" ,"r_ID_experiment[15,Minute]" , "r_ID_experiment[16,Minute]","r_ID_experiment[17,Minute]" , "r_ID_experiment[18,Minute]","r_ID_experiment[19,Minute]", "r_ID_experiment[20,Minute]"),
                        point_est = "median", prob = 0.5, prob_outer = 0.95) +
  labs(title ="Minute effect size",
       x = "Regression slope estimate") +
  scale_y_discrete(name = NULL, 
                   labels=c("Trial 1" , "Trial 2" , "Trial 3" , "Trial 4" , "Trial 5" , "Trial 6" , "Trial 7" , "Trial 8" , "Trial 9", "Trial 10" ,"Trial 11", "Trial 12", "Trial 13" ,"Trial 14" ,"Trial 15", "Trial 16" ,"Trial 17" ,"Trial 18", "Trial 19", "Trial 20"))+
  scale_x_continuous(breaks = seq(-0.03, 0.03, 0.005),
                     labels = scales::number_format(accuracy = 0.001, decimal.mark = '.'))+
  personal_theme

p_min


ggsave(filename = "Fig_S_slopes_minute.pdf",
       plot = p_min, 
       width = 15,
       height = 10,
       path = "outputs_results/supplementary_materials/",
       dpi = 600)



#### do not run ###
### plot regression lines ####

ce <- conditional_effects(model_time, effects = "Minute:ID_experiment",
                          #conditions = data_fit,
                          re_formula = NULL,points = TRUE )
str(ce[[1]])  # See what variables are available

to_string <- as_labeller(c(`1` = "Trial 1", `2` = "Trial 2",`3` = "Trial 3",
                           `4` = "Trial 4", `5` = "Trial 5",`6` = "Trial 6",
                           `7` = "Trial 7", `8` = "Trial 8",`9` = "Trial 9",
                           `10` = "Trial 10", `11` = "Trial 11",`12` = "Trial 12",
                           `13` = "Trial 13", `14` = "Trial 14",`15` = "Trial 15",
                           `16` = "Trial 16", `17` = "Trial 17",`18` = "Trial 18",
                           `19` = "Trial 19", `20` = "Trial 20"))


p <- plot(ce)[[1]] +
  facet_wrap("ID_experiment", scales = "free",labeller = to_string)+
  personal_theme +
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15)) +
  labs(x = "Time (minutes)", y = expression(Per~capita~consumption~rate~"    "~g~"·"~min^{-1}~"·"~ind^{-1}))

p


ggsave(filename = "Fig_S1.pdf",
       plot = p, 
       width = 15,
       height = 10,
       path = "outputs_results/supplementary_materials/",
       dpi = 600)








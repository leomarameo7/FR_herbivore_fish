#### Load packages#######
library(brms)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(reshape2)
library(sjPlot)
library(modelr)
##### Load data #####
data_fit <- as_tibble(read_csv("data/raw/data_raw.csv"))
# predictors as factors 
data_fit$P <- round(data_fit$P, digits = 2)
data_fit$N <- round(data_fit$N, digits = 2)
##### Load model #####
m <- readRDS("outputs_results/supplementary_materials/linear_model_sequence_minute.rds")

##### set personal theme for plot ####

personal_theme = theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    aspect.ratio = 0.75,
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size = 0.75),
    text = element_text(family = "sans"),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    axis.text.x = element_text(colour = "black", size = 14),
    axis.text.y  = element_text(colour = "black", size = 14),
    strip.text = element_text(size = 16),
    strip.background = element_blank(),
    legend.key.size = unit(1,'lines'),
    legend.key.height = unit(1,"lines"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size=16),
    legend.position = "none")

###Plot ###

ce <- conditional_effects(m, effects = "Sequence:ID_experiment",
                          re_formula = NULL)
str(ce[[1]])  # See what variables are available

p <- plot(ce)[[1]] +
  facet_wrap("ID_experiment")+
  personal_theme
p




#### Load packages#######
library(rstan) 
library(brms)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(reshape2)
# needed for data manipulation and plotting
##### Load data #####
data <- as_tibble(read_csv("data/processed/data_cleaned.csv"))

data = data %>% 
  mutate(trial = rep(c(1:24),each = 15)) %>% 
  select("Day", "trial", "Minute"       ,                  "Algae_density"       ,          
         ,"fish_number"         ,           "Fish_density"    ,               "biomass_fish"     ,             
          "Fish_biomass_density"    ,       "Total_bites_per_minute"  ,       "fish_eating_minute"   ,         
          "per_capita_bites"          ,     "total_bites_per_trial"   ,       "total_amount_resource_consumed",
          "Sequence"              ,         "initial_weight"             ,    "control_before"         ,       
          "control_after"             ,     "weight_algae_after_trial"   ,    "avgtime"    ,                   
         "factor_loss"       ,             "consumption_rate")

# For each combination of algae and fish densities. Retrived from  https://tem11010.github.io/regression_brms/
# Note:  it will take 5-10 minutes to fit 
model <- brm(formula =  consumption_rate ~ Minute + ( Minute |Algae_density:Fish_density) + ar(p = 1),
            
              data    = data,
              seed    = 123,
              cores = getOption("mc.cores",1),
              chains = 3,
              warmup = 500,
              iter = 5000,
              control = list(adapt_delta = 0.99, max_treedepth = 15),
             file= "outputs_results/supplementary_materials/linear_model")

summary(model)

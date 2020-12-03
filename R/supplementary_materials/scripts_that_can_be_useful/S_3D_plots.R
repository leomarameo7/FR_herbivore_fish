### Load packages####
library(ggplot2)
library(scatterplot3d)
library(tidyverse)
library(readr)
### Read raw data ####
data=read.csv("data/raw/data.csv",header=T)

#### Estimate algae weight loss by natural causes ####
data <- data %>%   
  mutate(factor_loss = control_after/control_before)
### Area of the natural pool in the Rocas Atoll where the trials were made: 594.96 m²
### Standarizing for the area where experiments were made ####
data <- data %>%   
  mutate(fish_density = fish_number/594.96)
#### Standarize resource density ####
data <- data %>% 
  mutate(resource_density = initial_weight/594.96)
#### Consumption rate (grams/time*individual) ####
data <- data %>% 
  mutate(consumption_rate = 
           (initial_weight-weight_algae_after_trial)*factor_loss/(avgtime*number_fish_eating))

####Plot 3D ####
# Remove columns that are not important
data1 <- data[, !(colnames(data) %in% c("initial_weight", "control_before" ,        
                                          "control_after","weight_algae_after_trial",
                                          "fish_number","biomass_fish",       
                                          "avgtime","factor_loss"))]
#plot
with(data1, 
     scatterplot3d(resource_density,
                   fish_density, 
                   consumption_rate, 
                   xlab = expression(Resource~density~" "~g/m^{2}),
                   ylab = expression(Fish~density~" "~g/m^{2}),
                   zlab = expression(Consumption~rate~" "~g/min~"·"~ind),
                   pch = 16, color="black",
                   angle = 66,
                   type="h",
                   box=F,
                   grid = T))









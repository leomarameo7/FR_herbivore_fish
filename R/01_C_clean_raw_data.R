#### Load packages ####
library(tidyverse)
library(readr)
library(dplyr)
library(plyr)
#### Load data ####
bites <- read_csv("data/raw/bites.csv")
# load observed algae consumption (weight before_after)
data = read_csv("data/raw/data.csv")

### Estimate per capita number of bites per minute #####
bites <- bites %>% 
  mutate(per_capita_bites = round(Total_bites_per_minute/fish_eating_minute)) %>%
  # fish density 
  mutate(Fish_density = Fish_density/594.96) %>%
  # algae density
  mutate(Algae_density = Algae_density/594.96) %>%
  # replace NA with 0 
  replace_na(list(per_capita_bites=0)) %>%
  # estimate total number of bites per trial 
  ddply(.(Fish_density, Algae_density), transform, total_bites_per_trial = sum(Total_bites_per_minute)) %>%
  mutate_if(is.numeric, round, digits = 3)

#### 
data1 = data %>%
  #slice(rep(1:n(), each = 15)) %>%
  #Area of the natural pool in the Rocas Atoll where the trials were made: 594.96 m²
  # Standarizing for the area where experiments were made ####
  mutate(Fish_density = fish_number/594.96) %>%
  mutate(Fish_biomass_density = biomass_fish/594.96) %>%
  
  mutate(Algae_density = trunc(initial_weight))  %>%
  mutate(Algae_density = Algae_density/594.96)  %>%
  # Estimate algae weight loss by natural causes ####
  mutate(factor_loss = control_after/control_before) %>%
  # total amount of algae consumed per trial####
  mutate(total_amount_resource_consumed = (initial_weight-weight_algae_after_trial)*factor_loss) %>%
  mutate_if(is.numeric, round, digits = 3) 

##  Merge the two dataframes (bites & data1)  based on the values of the columns: Algae_density & Fish_density

prova = left_join(bites,data1, by = c("Algae_density", "Fish_density"))
# order columns 
prova = prova %>%
       select(-Sequence, -initial_weight, -control_after,-control_before,
              -weight_algae_after_trial, -avgtime,-factor_loss, everything())
prova = prova[ ,c(1,4,2,9,3,10,11,5:8,12:19)]      
##### Consumption rate:  grams algae / min * ind ######
prova = prova %>%
  mutate(consumption_rate = total_amount_resource_consumed/total_bites_per_trial   * per_capita_bites)

#### write clean csv ####
write.csv(prova, file="data/processed/data_cleaned.csv", row.names=F)

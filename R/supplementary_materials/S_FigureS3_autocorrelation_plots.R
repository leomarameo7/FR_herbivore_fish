#### Load packages#######
library(rstan) 
library(brms)
library(psych) #to get some extended summary statistics
library(tidyverse)
library(tidybayes)
library(dplyr)
library(reshape2)
library(lemon)

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

#### Reload models with the readRDS() function####
model = readRDS("outputs_results/supplementary_materials/linear_model.rds")
###### Autocorrelation chart #####
# if the autocorrelation crosses the dashed blue line, 
# it means that specific lag is significantly correlated with current series
library(xts)
library(tidyquant)
library(stringr)

k <- 1:15
col_names <- paste0("lag_", k)

tidyverse_lags <- data %>%
  mutate( Time = lubridate::parse_date_time(Minute, 'M')) %>% 
  tq_mutate(
    select     = consumption_rate,
    mutate_fun = lag.xts,
    k          = 1:15,
    col_rename = col_names
  )

tidyverse_count_autocorrelations <- tidyverse_lags %>%
  gather(key = "lag", value = "lag_value", 
         -c(
           "Day" , "trial" , "Minute",                      
                                          "Algae_density","fish_number","Fish_density",               
                                              "biomass_fish","Fish_biomass_density" ,"Total_bites_per_minute" ,      
                                              "fish_eating_minute","per_capita_bites","total_bites_per_trial" ,    
                                              "total_amount_resource_consumed", "Sequence"  ,"initial_weight" ,               
                                              "control_before","control_after","weight_algae_after_trial",      
                                              "avgtime","factor_loss","consumption_rate","Time"
          )
         ) %>%
  mutate(lag = str_sub(lag, start = 5) %>% as.numeric) %>%
  group_by(Algae_density, Fish_density, lag) %>%
  summarize(
    cor = cor(x = consumption_rate, y = lag_value, use = "pairwise.complete.obs"),
    cutoff_upper = 2/(n())^0.5,
    cutoff_lower = -2/(n())^0.5
  )

# Trial as factor 
tidyverse_count_autocorrelations$Algae_density = as.factor(tidyverse_count_autocorrelations$Algae_density)
tidyverse_count_autocorrelations$Fish_density = as.factor(tidyverse_count_autocorrelations$Fish_density)


##### Rename factors #####


tidyverse_count_autocorrelations$Algae_density2 <- factor(tidyverse_count_autocorrelations$Algae_density, labels = c("Resource~density~~0.03~g~m^{-2}", "Resource~density~~0.05~g~m^{-2}",
                                                       "Resource~density~~0.08~g~m^{-2}", "Resource~density~~0.10~g~m^{-2}",
                                                       "Resource~density~~0.13~g~m^{-2}" ))

tidyverse_count_autocorrelations$Fish_density2 <- factor(tidyverse_count_autocorrelations$Fish_density, labels = c("Consumer~density~~0.08~ind.~m^{-2}", "Consumer~density~~0.11~ind.~m^{-2}",
                                                     "Consumer~density~~0.13~ind.~m^{-2}", "Consumer~density~~0.15~ind.~m^{-2}",
                                                     "Consumer~density~~0.18~ind.~m^{-2}" ))

#### Plot autocorrelation #####
p = tidyverse_count_autocorrelations %>%
   ggplot( aes( x = lag, y = cor)) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  # Plot autocorrelations
  geom_point(size = 1) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  # Add cutoffs
  geom_line(aes(y = cutoff_upper), color = "blue", linetype = 2) +
  geom_line(aes(y = cutoff_lower), color = "blue", linetype = 2) +
  # Add facets
  facet_wrap(~Algae_density2+Fish_density2, scales = "free", 
             labeller=labeller(Algae_density2=label_parsed, Fish_density2=label_parsed, .multi_line=T)) +
  
  #coord_capped_cart(bottom='both', left='both') +
  # Aesthetics
  expand_limits(y = c(-1, 1)) +
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15)) +
  labs(
    y = "Pearson correlation coefficient",
    x = "Lags"
  ) +

  theme(panel.border=element_blank(), 
        axis.line=element_line(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family = "sans"),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
       # axis.text.x = element_text(size = 10),
        #axis.text.y  = element_text(size = 10),
    #strip.background = element_blank(),
    #strip.text.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1))

p



##### Maximum likelihood covariances (correlations) of the posterior samples #####
# Covariance or correlation matrix of population-level parameters
library(coda)
stan_ac(model$fit)
resid = resid(model, type = "pearson")[, "Estimate"]
plot(acf(resid, lag = 15))





### Saving plot####
#Set new windo size and replot whatever plot you just made. 
dev.new(width = 10, height = 7.5, unit="in", noRStudioGD = T)
last_plot() 
#Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "Pearson_autocorrelation.pdf",
       plot= p, 
       device="pdf",
       path ="outputs_results/supplementary_materials/",
       width = dev.size()[1],
       height = dev.size()[2],
       dpi = 400)

dev.off() 


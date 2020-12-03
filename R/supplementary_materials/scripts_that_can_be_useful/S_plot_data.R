# load packages 
library(readr)
library(ggplot2)
library(tidyverse)
library(scales)
library(extrafont)
# import fonts - only once
#font_import()
# load fonts - every session
loadfonts(device = "postscript", quiet = TRUE)
### read raw data ###
data=read.csv("data/raw/data.csv",header=T)

#### Estimate algae weight  loss by natural causes ###
data <- data %>%   
  mutate(factor_loss = control_after/control_before)
### Area of the natural pool in the Rocas Atoll where the trials were made: 594.96 m²
### standarizing for the area where experiments were made ####
data <- data %>%   
  mutate(fish_density = fish_number/594.96)
#### standarize resource density ####
data <- data %>% 
  mutate(resource_density = initial_weight/594.96)
#### Consumption rate (grams/individuals*time) ####
data <- data %>% 
  mutate(consumption_rate = 
           (initial_weight-weight_algae_after_trial)*factor_loss/(avgtime*fish_number))

####I ggplot (total algae consumed vs algae density per fish abundance), color point#####
b <- ggplot(data, aes(x = resource_density, y = consumption_rate, 
                      color = as.factor(fish_number),
                      shape = as.factor(fish_number))) +
  geom_point(size=2) +
  scale_shape_manual(values=c(15, 16, 17,18,8)) + #### shape of the points
  geom_line(size=1.25) + # color of the line
  scale_color_manual(values = c( "black", "blue", "orange","red","purple")) +
  labs(x=expression(Resource~density~" "~g/m^{-2}),
        y=expression(Consumption~rate~" "~g/min^{-1}*ind^{-1}),
       colour = "Consumer abundance (individuals)",
       shape = "Consumer abundance (individuals)")  +
  theme_classic()+
  theme(text = element_text(family = "Calibri"),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.position=("top"),
        legend.text = element_text(size=8, face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y  = element_text(size = 10))+
  scale_y_continuous(position = "left", limits=c(0,0.03), 
                     breaks=seq(0,0.03,0.005), labels = scales::scientific)+
  scale_x_continuous( limits=c(0,0.15), breaks=seq(0,0.15,0.01))
b  

### Plot with consumption rates with units(g/min*g)####

#Consumption rate (grams algae/g fish*time) #
data <- data %>% 
  mutate(consumption_rate = 
           (initial_weight-weight_algae_after_trial)*factor_loss/(avgtime*biomass_fish))

#Plot II 
c <- ggplot(data, aes(x = resource_density, y = consumption_rate, 
                      color = as.factor(fish_number),
                      shape = as.factor(fish_number))) +
  geom_point(size=2) +
  scale_shape_manual(values=c(15, 16, 17,18,8)) + #### shape of the points
  geom_line(size=1.25) + # color of the line
  scale_color_manual(values = c( "black", "blue", "orange","red","purple")) +
  labs(x=expression(Resource~density~" "~g/m^{-2}),
       y=expression(Consumption~rate~" "~g~algae/min^{-1}*g~fish^{-1}),
       colour = "Consumer abundance (individuals)",
       shape = "Consumer abundance (individuals)")  +
  theme_classic()+
  theme(text = element_text(family = "Calibri"),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.position=("top"),
        legend.text = element_text(size=8, face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y  = element_text(size = 10))+
  scale_y_continuous(position = "left", limits=c(0,2.4e-04), 
                     breaks=seq(0,2.4e-04,4.8e-05), labels = scales::scientific)+
  scale_x_continuous( limits=c(0,0.15), breaks=seq(0,0.15,0.01))
c  

### Plot with consumption rates with units(g algae/min)####

#Consumption rate (grams algae/g fish*time) #
data <- data %>% 
  mutate(consumption_rate = 
           (initial_weight-weight_algae_after_trial)*factor_loss/(avgtime))

#Plot II 
a <- ggplot(data, aes(x = resource_density, y = consumption_rate, 
                      color = as.factor(fish_number),
                      shape = as.factor(fish_number))) +
  geom_point(size=2) +
  scale_shape_manual(values=c(15, 16, 17,18,8)) + #### shape of the points
  geom_line(size=1.25) + # color of the line
  scale_color_manual(values = c( "black", "blue", "orange","red","purple")) +
  labs(x=expression(Resource~density~" "~g/m^{-2}),
       y=expression(Consumption~rate~" "~g~algae/min^{-1}),
       colour = "Consumer abundance (individuals)",
       shape = "Consumer abundance (individuals)")  +
  theme_classic() +
  theme(text = element_text(family = "Calibri"),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.position=("top"),
        legend.text = element_text(size=8, face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y  = element_text(size = 10))+
  scale_y_continuous(position = "left", limits=c(0,1.4), 
                     breaks=seq(0,1.4,0.2))+
  scale_x_continuous( limits=c(0,0.15), breaks=seq(0,0.15,0.01))
a 

### save the plots####

ggsave(filename="plot_explore_data.png",
       plot=b, 
       device="png",
       path ="outputs_results/figures/", 
       units = "in", # other options are "in", "cm", "mm" 
       dpi = 300)


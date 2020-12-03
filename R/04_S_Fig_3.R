### Load packages####
library(tidyverse)
library(ggplot2)
library(egg)
library(brms)
library(ggpubr)
library(modelr)
library(tidybayes)
### Load data #####
data = read.csv("data/processed/data_cleaned.csv",header=T)
#### Reload models with the readRDS() function####
fitP2 = readRDS("fitP2.rds")
### define theme ggplot
personal_theme = theme_classic() +
theme(
      panel.grid.major = element_blank(),
      aspect.ratio = 0.75,
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(size = 0.75),
      text = element_text(family = "sans"),
      axis.title.x = element_text(color = "black", size = 14),
      axis.title.y = element_text(color = "black", size = 14),
      axis.text.x = element_text(colour = "black", size = 12),
      axis.text.y  = element_text(colour = "black", size = 12),
      legend.key.size = unit(0.75,'lines'),
      legend.key.height = unit(1,"lines"),
      legend.text = element_text(size = 11),
      legend.position="top")

#data = data %>%
  #group_by(Algae_density,Fish_density) %>%
  #' add_fitted_draws: given a data frame and a model, adds draws from the (possibly transformed) posterior "fit" (aka the linear/link-level predictor), 
  #' the posterior predictions of the model, or the residuals of a model to the data frame in a long format.
 # add_fitted_draws(fitP2, n=5000) 

data_1 = data %>%
  mutate(Algae_density = round(Algae_density, digits=2) )%>% 
  mutate(Fish_density = round(Fish_density, digits=2)) %>% 
  group_by(Algae_density,Fish_density) %>%
  #' add_predicted_draws adds draws from posterior predictions to the data. 
  #' .prediction column containing draws from the posterior predictive distribution.
  add_predicted_draws(fitP2, n = 3000) %>% 
  filter(.prediction > 0) 
   
  
##### Plot 1 : resource density vs consumption rate  ####
p =  ggplot(data = data_1, aes(x = Algae_density, y = consumption_rate )) +
  stat_lineribbon(aes(y = .prediction), alpha = 0.5) +
  geom_point(data = data_1, aes(x = Algae_density, y = consumption_rate, shape = ordered(Fish_density))) +
  scale_shape_manual(values = c(15, 16, 17,18,3), 
                     name = expression(Consumer~density~" "~ind.~m^{-2})) +
  scale_fill_grey(start = 0.9, end = 0.6, guide = 'none') +
  personal_theme + 
  labs(x = expression(Resource~density~" "~g~m^{-2}),
                      y = expression(Consumption~rate~" "~g~min^{-1}~ind^{-1})) +
  scale_y_continuous(limits=c(0, 1.55), breaks=seq(0, 1.55, 0.15))+
  scale_x_continuous(limits = c(0.02 ,0.14), breaks = seq(0.02, 0.14, 0.02))
p

#### Plot 2: consumption rate vs fish density ####

p2 = ggplot(data = data_1, aes(x = Fish_density, y = consumption_rate )) +
  stat_lineribbon(aes(y = .prediction), alpha = 0.5) +
  geom_point(data = data_1, aes(x = Fish_density, y = consumption_rate, shape = ordered(Algae_density))) +
  scale_shape_manual(values = c(15, 16, 17,18,3), 
                     name = expression(Resource~density~" "~g~m^{-2})) +
  scale_fill_grey(start = 0.9, end = 0.6, guide = 'none') +
  personal_theme + 
  labs(x = expression(Consumer~density~" "~ind.~m^{-2}),
       y = expression(Consumption~rate~" "~g~min^{-1}~ind^{-1})) +
  scale_y_continuous(limits=c(0, 1.55), breaks=seq(0, 1.55, 0.15)) +
  scale_x_continuous(limits=c(0.08,0.18), breaks=seq(0.08,0.18,0.02))

p2

#### Multi plot ####
multi = ggarrange(p, p2, nrow = 1, ncol = 2)
multi 
#Set new windo size and replot whatever plot you just made. 
#dev.new(width = 10, height = 7.5, unit = "in", noRStudioGD = T)
#last_plot() 
#Save the plot and set the size using `dev.siz()` 
# so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "Figure_2.png",
       plot = multi, 
       device = "png",
       path = "outputs_results/figures/",
       #width = dev.size()[1],
       #height = dev.size()[2],
       dpi = 600)

dev.off()












# Create ggplot object for the conditional effect function: 
pP2 = plot(conditional_effects(fitP2), plot = FALSE, method = "predict" )

### Plot_1 Y = consumption rate ; x= algae density ####
p1 = pP2[["Algae_density"]]
# data frame observed data 
d= p1[["plot_env"]][["df_points"]]
# fish densisty as factor
d$Fish_density= round(d$Fish_density, digits=2)# round digits
d$Fish_density=as.factor(d$Fish_density)
#plot 
d$Algae_density = round(d$Algae_density, digits=2)

p1 = p1 +
  geom_point(data = d, 
             aes(x = d$Algae_density, y = d$resp__, shape = d$Fish_density),
             size=2, fill="black", 
             inherit.aes = FALSE)+
  scale_shape_manual(values=c(15, 16, 17,18,3), 
                     name = expression(Consumer~density~" "~ind.~m^{-2}))+
  geom_line(size=1) +
  personal_theme +
  labs(x=expression(Resource~density~" "~g~m^{-2}),
       y=expression(Consumption~rate~" "~g~min^{-1}~ind^{-1}))+
 scale_y_continuous(limits=c(0,1.5),breaks=seq(0,1.5,0.15))+
  scale_x_continuous(limits=c(0.025,0.135), breaks=seq(0.025, 0.135,0.01))
p1
  
### Plot_2 Y = consumption rate ; x= fish density ####
p2= pP2[["fish_density"]]
d1= p2[["plot_env"]][["df_points"]]
# fish densisty as factor
d1$resource_density= round(d1$resource_density, digits=2) # round digits
d1$resource_density=as.factor(d1$resource_density)
#### plot_2 #####
p2= p2 +
  geom_point(data = d1, 
             aes(x= d1$fish_density, y=d1$resp__, shape=d1$resource_density),
             size=2, fill="black", 
             inherit.aes = FALSE)+
  scale_shape_manual(values=c(15, 16, 17,18,3), 
                     name = expression(Resource~density~" "~g~m^{-2}))+
  geom_line(size=1)+
  personal_theme +
  labs(x=expression(Consumer~density~" "~ind.~m^{-2}),
       y=expression(Consumption~rate~" "~g~min^{-1}~ind^{-1}))+
  scale_y_continuous(limits=c(0,0.035),breaks=seq(0,0.035,0.005))+
  scale_x_continuous(limits=c(0.07,0.18), breaks=seq(0.07,0.18,0.01))
p2



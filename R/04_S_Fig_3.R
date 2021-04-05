### Load packages####
library(tidyverse)
library(ggplot2)
library(brms)
library(ggpubr)
library(readxl)
library(tidybayes)
### define theme ggplot
personal_theme = theme_classic() +
  theme(
    panel.grid.major = element_blank(),
   # aspect.ratio = 0.5,
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
    legend.key.size = unit(0.75,'lines'),
    legend.key.height = unit(1,"lines"),
    legend.text = element_text(size = 11),
    legend.position='none')

#### Reload models with the readRDS() function####
fit_P2 = readRDS("outputs_results/models/fit_P2.rds")
### Load data_observed #####
data <- readxl::read_excel('data/processed/data_cleaned.xlsx')
data_observed <- data %>% 
  mutate(ratio = round(data$Algae_density/data$Fish_density, digits = 3))
unique(data_observed$ratio) # 20 combinations of the ratio algae/fish density

data_predicted <- data_observed %>%
  group_by(Algae_density, Fish_density) %>%
  #' add_predicted_draws adds draws from posterior predictions to the data_observed. 
  #' .prediction column containing draws from the posterior predictive distribution.
  add_predicted_draws(fit_P2, n = 3000) %>% 
  mutate(.prediction_per_capita = ifelse(fish_eating_minute == 0, 0, .prediction/fish_eating_minute) ) %>% 
  mutate(ratio = Algae_density / Fish_density) %>% 
  ungroup()  
  

#data_predicted <- data_predicted %>%  filter(.prediction_per_capita >0 & .prediction_per_capita < 1)

####  Resource density vs consumption rate  ####
data_observed$Algae_density <- round(data_observed$Algae_density, digits = 2)
data_predicted$Algae_density <- round(data_predicted$Algae_density, digits = 2)

p =  ggplot() +
  stat_lineribbon(data = data_predicted, aes(y = .prediction_per_capita, x = Algae_density), 
                  alpha = 0.5,
                  .width = c(.5, .95)) +
  
  geom_boxplot( aes(x = data_observed$Algae_density, y = data_observed$Cons_rate_pc, group = data_observed$Algae_density, alpha = 0.2), 
                outlier.colour = NA,
                width = 0.01,
                notch = F) +
  
  scale_fill_grey(start = 0.8, end = 0.5, guide = 'none') +
  personal_theme + 
  labs(x = expression(Algae~density~" "~g~m^{-2}),
       y = expression(Per~capita~consumption~rate~""~(g~min^{-1}~ind^{-1}))) +
  scale_x_continuous(breaks = c(0.03,0.05,0.08,0.10,0.13)) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, 0.2))
p

ggsave(filename = "Fig_3a.pdf",
       plot = p, 
       device = "pdf",
       path = "outputs_results/figures/",
       dpi = 600)

#### Alternative Plot: consumption rate vs fish density ####
data_observed$Fish_density <- round(data_observed$Fish_density, digits = 2)
data_predicted$Fish_density <- round(data_predicted$Fish_density, digits = 2)

p2 <-  ggplot() +
  
  stat_lineribbon(data = data_predicted, aes(y = .prediction_per_capita, x = Fish_density), 
                  alpha = 0.5,
                  .width = c(.5, .95)) +
  
  geom_boxplot( aes(x = data_observed$Fish_density, y = data_observed$Cons_rate_pc, group = data_observed$Fish_density, alpha = 0.2), 
                outlier.colour = NA,
                width = 0.01,
                notch = F) +
  scale_fill_grey(start = 0.8, end = 0.5, guide = 'none') +
  personal_theme + 
  labs(x = expression(Fish~density~" "~ind.~m^{-2}),
       y = expression(Per~capita~consumption~rate~""~(g~min^{-1}~ind^{-1}))) +
  scale_x_continuous(breaks=c(0.08,0.11,0.13,0.15))+
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, 0.2))

p2

ggsave(filename = "Fig_3b.pdf",
       plot = p2, 
       device = "pdf",
       path = "outputs_results/figures/",
       dpi = 600)

#### Figure 3c #####

p3 <-  ggplot() +
  stat_lineribbon(data = data_predicted, aes(y = .prediction_per_capita, x = ratio), 
                  alpha = 0.5,
                  .width = c(.5, .95)) +
  
  geom_boxplot(aes(x = data_observed$ratio, y = data_observed$Cons_rate_pc, group = data_observed$ratio, alpha = 0.2), 
                outlier.colour = NA,
                width = 1,
                notch = F) +
  
  scale_fill_grey(start = 0.8, end = 0.5, guide = 'none') +
  personal_theme + 
  labs(x = expression(Ratio~Algae~"/"~Fish),
       y = expression(Per~capita~consumption~rate~""~(g~min^{-1}~ind^{-1}))) +
 scale_x_continuous(limits = c(0.15, 1.5), breaks = seq(0.15, 1.5, 0.15))+
 scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))

p3


#### Multi plot ####

library("gridExtra")
ciccio <- grid.arrange(                             
             arrangeGrob(p, p2, ncol = 2), # First row with 2 plots in 2 different columns
             p3, # Second row with one plot spaning over 2 columns
             nrow = 2) 

ggsave(filename = "Fig_3.pdf",
       plot = ciccio, 
       device = "pdf",
       path = "outputs_results/figures/",
       dpi = 600)


####  facet wrap #####
data_predicted<- data_predicted%>% 
  mutate(consumer_density = case_when(Fish_density == "0.08" ~ "Fish~density~~0.08~ind.~m^{-2}",
  Fish_density == "0.11" ~ "Fish~density~~0.11~ind.~m^{-2}",
  Fish_density == "0.13" ~ "Fish~density~~0.13~ind.~m^{-2}",
  Fish_density == "0.15" ~ "Fish~density~~0.15~ind.~m^{-2}"))

data_observed <- data_observed %>% 
  mutate(consumer_density = case_when(Fish_density == "0.08" ~ "Fish~density~~0.08~ind.~m^{-2}",
                                      Fish_density == "0.11" ~ "Fish~density~~0.11~ind.~m^{-2}",
                                      Fish_density == "0.13" ~ "Fish~density~~0.13~ind.~m^{-2}",
                                      Fish_density == "0.15" ~ "Fish~density~~0.15~ind.~m^{-2}"))

data_predicted$consumer_density <- as.factor(data_predicted$consumer_density)


ciccio <- ggplot(data = data_predicted) +
  
  stat_lineribbon(data = data_predicted, aes(y = .prediction_per_capita, x = Algae_density), 
                  alpha = 0.5,
                  .width = c(.5, .95)) +
  
  geom_boxplot( data = data_observed , aes(x = Algae_density, y = Cons_rate_pc, group = Algae_density, alpha = 0.2),
                outlier.colour = NA,
                width = 0.01,
                notch = F) +
  
  scale_fill_grey(start = 0.8, end = 0.5, guide = 'none') +
  facet_wrap(~consumer_density, scales= "free_y", labeller = label_parsed)+
  personal_theme + 
  labs(x = expression(Algae~density~" "~g~m^{-2}),
       y = expression(Consumption~rate~" "~g~min^{-1}~ind^{-1})) +
  scale_x_continuous(breaks=c(0.03,0.05,0.08,0.10,0.13))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))
  
ciccio


ggsave(filename = "Fig_alternative_2.pdf",
       plot = ciccio, 
       device = "pdf",
       path = "outputs_results/figures/",
       dpi = 600)






# Create ggplot object for the conditional effect function: 
pP2 = plot(conditional_effects(fitP2), plot = FALSE, method = "predict" )

### Plot_1 Y = consumption rate ; x= algae density ####
p1 = pP2[["Algae_density"]]
# data_observed frame observed data_observed 
d= p1[["plot_env"]][["df_points"]]
# fish densisty as factor
d$Fish_density= round(d$Fish_density, digits=2)# round digits
d$Fish_density=as.factor(d$Fish_density)
#plot 
d$Algae_density = round(d$Algae_density, digits=2)

p1 = p1 +
  geom_point(data_observed = d, 
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
  geom_point(data_observed = d1, 
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



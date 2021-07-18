### Load packages####
library(tidyverse)
library(tidybayes)
library(RColorBrewer)
library("gridExtra")
### define theme ggplot
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
    legend.title = element_text(size=16))

### Load data_observed #####
data_observed <- read_csv('data/raw/data_raw.csv')
unique(data_observed$P)
unique(data_observed$P_biomass_kg_m2)

#### Fig.3_a Resource density vs consumption rate  ####
data_observed$N <- round(data_observed$N, digits = 2)
unique(data_observed$N)
summary(data_observed$C)
# predictors as factors 
data_observed$P_factor <- as.factor(round(data_observed$P, digits = 2))
data_observed$N_factor <- as.factor(round(data_observed$N, digits = 2))
# define colors #
nb.cols <- 4
mycolors <- colorRampPalette(brewer.pal(4, "Set1"))(nb.cols)


p <-  ggplot(data = data_observed, aes(y = C, x = N, color = P_factor)) +
  geom_point(aes(y = C, x = N), inherit.aes = FALSE, position = position_jitter(w = 0.001, h = 0),
             alpha = 0.45)+
  stat_pointinterval(.width = c(.50, .95), position = position_dodge(width = .0075))+
  personal_theme + 
  labs(color = expression(Fish~density~" "~ind.~m^{-2}),
       x = expression(Seaweed~density~" "~g~m^{-2}),
       y = expression(Per~~capita~~consumption~~rate~""~(g~min^{-1}~ind^{-1}))) +
  scale_x_continuous(breaks = c(0.03,0.05,0.08,0.10,0.13)) +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.15))+
  theme(legend.position="top")+
  scale_color_manual(values = mycolors)
p


ggsave(filename = "Fig_3_a.pdf",
       plot = p, 
       width = 10,
       height = 10,
       path = "outputs_results/figures/",
       dpi = 600)

#### Fig.3_b Plot: consumption rate vs fish density ####
data_observed$P <- round(data_observed$P, digits = 2)
unique(data_observed$P)
summary(data_observed$C)
nb.cols <- 5
mycolors <- colorRampPalette(brewer.pal(5, "Set2"))(nb.cols)
data_observed$N_factor <- as.factor(round(data_observed$N, digits = 2))


p2 <-  ggplot(data = data_observed, aes(y = C, x = P, color = N_factor)) +
  geom_point(aes(y = C, x = P), inherit.aes = FALSE, position = position_jitter(w = 0.001, h = 0),
                alpha = 0.45)+
  stat_pointinterval(.width = c(.50, .95), position = position_dodge(width = .0075))+
  personal_theme + 
  labs(color = expression(Seaweed~density~" "~g~m^{-2}),
       x = expression(Fish~density~" "~ind.~m^{-2}),
       y = expression(Per~capita~consumption~rate~""~(g~min^{-1}~ind^{-1}))) +
  scale_x_continuous(breaks=c(0.08,0.11,0.13,0.15))+
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.15))+
  theme(legend.position="top")+
  scale_color_manual(values = mycolors)

p2

ggsave(filename = "Fig_3b.pdf",
       plot = p2, 
       width = 10,
       height = 10,
       path = "outputs_results/figures/",
       dpi = 600)

#### Multi plot ####
ciccio <- grid.arrange(                             
             arrangeGrob(p, p2, ncol = 2)) 
ciccio
ggsave(filename = "Fig_3.pdf",
       plot = ciccio, 
       width = 15,
       height = 14,
       path = "outputs_results/figures/",
       dpi = 600)







#### Do not run the code below #####

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



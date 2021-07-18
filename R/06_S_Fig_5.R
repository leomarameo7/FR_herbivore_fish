### Load packages####
library(tidyverse)
library(tidybayes)
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
data_observed <- data_observed %>%  mutate(ratio = N / P) 

#### Load best fitted model ####
fit_R1 = readRDS("outputs_results/models/fit_with_fish_density/fit_R1.rds")

#### load data predicted 

data_predicted <- data_observed %>%
  group_by(N, P) %>%
  #' add_predicted_draws adds draws from posterior predictions to the data_observed. 
  #' .prediction column containing draws from the posterior predictive distribution.
  add_predicted_draws(fit_R1, n = 3000) %>% 
  filter(.prediction >0) %>% 
  ungroup()  

data_fitted <- data_observed %>%
  group_by(N, P) %>%
  #' add_predicted_draws adds draws from posterior predictions to the data_observed. 
  #' .prediction column containing draws from the posterior predictive distribution.
  add_fitted_draws(fit_R1, n = 3000) %>% 
  ungroup()  

colnames(data_predicted)
summary(data_predicted$.prediction)
summary(data_fitted$.value)
#### Figure 5 #####
summary(data_predicted$ratio)
summary(data_observed$ratio)
summary(data_observed$C)
summary(data_predicted$.value)
unique(data_observed$ratio)
unique(data_predicted$ratio)

p3_predicted <-  ggplot() +
  stat_lineribbon(data = data_predicted, aes(y = .prediction, x = ratio), 
                  alpha = 0.5,
                  .width = c(.5, .95)) +
  stat_pointinterval(aes(x = data_observed$ratio, y = data_observed$C, group = data_observed$ratio),
                     .width = c(0.5,0.75),point_size= 1.25, thickness=0.9,alpha=0.75,
                     position = position_dodge(width = .0075))+
  
  scale_fill_grey(start = 0.8, end = 0.5, guide = 'none') +
  personal_theme + 
  labs(x = expression(Ratio~Seaweed~"/"~Fish~(g~"·"~ind.^{-1})),
       y = expression(Per~capita~consumption~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1}))) +
  scale_x_continuous(limits = c(0.15, 1.6), breaks = seq(0.15, 1.6, 0.1))+
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.15))+
  annotate("text", x = 0.15, y = 1.4, parse = TRUE, size= 4,hjust=0,vjust=0,
           label = "'Arditi-Ginzburg type I (R1):  ' * y == a*'·'*frac(N, P)*'       '* paste(italic(R) ^ 2, \" = 0.62\")*'  [0.57; 0.65]'" )

p3_predicted


ggsave(filename = "Fig_5.pdf",
       plot = p3_predicted, 
       width = 22,
       height = 16,
       units="cm",
       path = "outputs_results/figures/",
       dpi = 600)


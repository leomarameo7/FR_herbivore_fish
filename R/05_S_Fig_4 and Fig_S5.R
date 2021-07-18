### Load packages####
library(tidyverse)
library(tidybayes)
library(brms)
library(gridExtra)
library(patchwork)
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
data_observed <- read_csv('data/processed/data_fitting_models.csv')
#### COMPARE MODELS ####
#### Reload models with the readRDS() function####
fit_00 <- readRDS("outputs_results/models/fit_with_fish_density/fit_00.rds")
fit_H1 <- readRDS("outputs_results/models/fit_with_fish_density/fit_H1.rds")
fit_H2 <- readRDS("outputs_results/models/fit_with_fish_density/fit_H2.rds")
fit_R1 <- readRDS("outputs_results/models/fit_with_fish_density/fit_R1.rds")
fit_R2 <- readRDS("outputs_results/models/fit_with_fish_density/fit_R2.rds")
fit_P1 <- readRDS("outputs_results/models/fit_with_fish_density/fit_P1.rds")
fit_P2 <- readRDS("outputs_results/models/fit_with_fish_density/fit_P2.rds")
fit_IV <- readRDS("outputs_results/models/fit_with_fish_density/fit_IV.rds")
fit_IR <- readRDS("outputs_results/models/fit_with_fish_density/fit_IR.rds")
#### R1 ####

d_r1 <- fitted(fit_R1, probs = c(0.025,0.25, 0.75,0.975)) %>%
  as_tibble() %>%
  bind_cols(data_observed)
  
R_1 <- ggplot(data = d_r1, aes(x = Estimate , y = C)) +
  geom_abline(linetype = 2, color = "black", size = .75) +
  geom_point(size = 1.5, color = "black", alpha = 3/4) +
  # predicted 
 # geom_linerange(aes(xmin = Q2.5, xmax = Q97.5), 
            #  size = 0.5, color = "black") +
  
  geom_linerange(aes(xmin = Estimate-Est.Error,  xmax = Estimate+Est.Error), 
                 size = 0.75, color = "black")+
  # observed
 # geom_linerange(aes(ymin = q_2.5, ymax = q_97.5),
      #           size = 0.95, color = "black")+
  
  geom_linerange(aes(ymin = C-se, ymax = C+se), 
             size = 0.75, color = "black") +

  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  scale_y_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  personal_theme +
  labs(y = expression(Observed~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1})),
       x = expression(Predicted~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1}))) +
  annotate("text", x = 0.1, y = 0.6, parse = TRUE, size= 5, hjust=0,vjust=0,
           label = "'Arditi-Ginzburg type I ratio-dependent (R1):  '*'       '* paste(italic(R) ^ 2, \" = 0.62\")*'  [0.57; 0.65]'" )

R_1


#### Ivlev resource dependent ####

d_IV <- fitted(fit_IV, probs = c(0.25, 0.75)) %>%
  as_tibble() %>%
  bind_cols(data_observed)

IV <- ggplot(data = d_IV, aes(x = Estimate , y = C)) +
  geom_abline(linetype = 2, color = "black", size = .75) +
  geom_point(size = 1.5, color = "black", alpha = 3/4) +
  # observed
  
  geom_linerange(aes(ymin = C-se, ymax = C+se), 
                 size = 0.75, color = "black") +
  # predicted
  geom_linerange(aes(xmin = Estimate - Est.Error, 
                     xmax = Estimate + Est.Error),
                 size = 0.75, color = "black")+
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  scale_y_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  personal_theme +
  labs(y = expression(Observed~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1})),
       x = expression(Predicted~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1}))) +
  annotate("text", x = 0.15, y = 0.6, parse = TRUE, size= 5, hjust=0,vjust=0,
           label = "'Ivlev resource-dependent (IV):  '*'       '* paste(italic(R) ^ 2, \" = 0.56\")*'  [0.47; 0.62]'" )

IV


#### Holling type II resource-dependent ####
d_H2 <- fitted(fit_H2) %>%
  as_tibble() %>%
  bind_cols(data_observed)

H2 <- ggplot(data = d_H2, aes(x = Estimate , y = C)) +
  geom_abline(linetype = 2, color = "black", size = .75) +
  geom_point(size = 1.5, color = "black", alpha = 3/4) +
# observed

geom_linerange(aes(ymin = C-se, ymax = C+se), 
               size = 0.75, color = "black") +
  # predicted
  geom_linerange(aes(xmin = Estimate - Est.Error, 
                     xmax = Estimate + Est.Error),
                 size = 0.75, color = "black")+
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  scale_y_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  personal_theme+
  labs(y = expression(Observed~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1})),
       x = expression(Predicted~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1}))) +
  annotate("text", x = 0.15, y = 0.6, parse = TRUE, size= 5, hjust=0,vjust=0,
           label = "'Holling type II resource-dependent (H2):  '*'       '* paste(italic(R) ^ 2, \" = 0.36\")*'  [0.06; 0.55]'" )

H2

#### Null model ####

null <- fitted(fit_00) %>%
  as_tibble() %>%
  bind_cols(data_observed)

fit_0 <- ggplot(data = null, aes(x = Estimate , y = C)) +
  geom_abline(linetype = 2, color = "black", size = .75) +
  geom_point(size = 1.5, color = "black", alpha = 3/4) +
  # observed
  
  geom_linerange(aes(ymin = C-se, ymax = C+se), 
                 size = 0.75, color = "black") +
  
  geom_linerange(aes(xmin = Estimate - Est.Error, 
                     xmax = Estimate + Est.Error),
                 size = 0.75, color = "black")+
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  scale_y_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  personal_theme +
  labs(y = expression(Observed~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1})),
       x = expression(Predicted~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1}))) +
  annotate("text", x = 0.15, y = 0.6, parse = TRUE, size= 5, hjust=0,vjust=0,
           label = "'Null model:  '*'       '* paste(italic(R) ^ 2, \" = 0.0\")*'  [0.0; 0.0]'" )

fit_0


#### Multi plot ####


join <- fit_0 + H2 + IV + R_1 + plot_layout(ncol = 2)+
  plot_annotation(tag_levels = "a") &
  theme(plot.tag.position = c(0.1, 1),
         plot.tag = element_text(face = 'bold', size=18))
join

ggsave(filename = "Fig_4.pdf",
       plot = join, 
       width = 20,
       height = 15,
       path = "outputs_results/figures/",
       dpi = 600)


#### supplemenary materials #####
#### Holling type I resource-dependent ####

d_H1 <- fitted(fit_H1) %>%
  as_tibble() %>%
  bind_cols(data_observed)

H1 <- ggplot(data = d_H1, aes(x = Estimate , y = C)) +
  geom_abline(linetype = 2, color = "black", size = .75) +
  geom_point(size = 1.5, color = "black", alpha = 3/4) +
# observed
geom_linerange(aes(ymin = C-se, ymax = C+se), 
               size = 0.75, color = "black") +
  
  geom_linerange(aes(xmin = Estimate - Est.Error, 
                     xmax = Estimate + Est.Error),
                 size = 0.75, color = "black")+
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  scale_y_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  personal_theme+
  labs(y = expression(Observed~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1})),
       x = expression(Predicted~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1}))) +
  annotate("text", x = 0.15, y = 0.6, parse = TRUE, size= 5, hjust=0,vjust=0,
           label = "'Holling type I resource-dependent (H1):  '*'       '* paste(italic(R) ^ 2, \" = 0.45\")*'  [0.20; 0.59]'" )

H1

#### Ivlev ratio-dependent ####

d_ivr <- fitted(fit_IR) %>%
  as_tibble() %>%
  bind_cols(data_observed)

IVR <- ggplot(data = d_ivr, aes(x = Estimate , y = C)) +
  geom_abline(linetype = 2, color = "black", size = .75) +
  geom_point(size = 1.5, color = "black", alpha = 3/4) +
  # observed
  geom_linerange(aes(ymin = C-se, ymax = C+se), 
                 size = 0.75, color = "black") +
  
  geom_linerange(aes(xmin = Estimate - Est.Error, 
                     xmax = Estimate + Est.Error),
                 size = 0.75, color = "black")+
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  scale_y_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  personal_theme +
  labs(y = expression(Observed~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1})),
       x = expression(Predicted~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1}))) +
  annotate("text", x = 0.15, y = 0.6, parse = TRUE, size= 5, hjust=0,vjust=0,
           label = "'Ivlev ratio-dependent (IVR):  '*'       '* paste(italic(R) ^ 2, \" = 0.53\")*'  [0.40; 0.62]'" )

IVR

#### P1 consumer-dependent ####

d_p1 <- fitted(fit_P1) %>%
  as_tibble() %>%
  bind_cols(data_observed)

P1 <- ggplot(data = d_p1, aes(x = Estimate , y = C)) +
  geom_abline(linetype = 2, color = "black", size = .75) +
  geom_point(size = 1.5, color = "black", alpha = 3/4) +
  # observed
  geom_linerange(aes(ymin = C-se, ymax = C+se), 
                 size = 0.75, color = "black") +
  
  geom_linerange(aes(xmin = Estimate - Est.Error, 
                     xmax = Estimate + Est.Error),
                 size = 0.75, color = "black")+
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  scale_y_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  personal_theme +
  labs(y = expression(Observed~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1})),
       x = expression(Predicted~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1}))) +
  annotate("text", x = 0.15, y = 0.6, parse = TRUE, size= 5, hjust=0,vjust=0,
           label = "'Hassell-Varley consumer-dependent (P1):  '*'       '* paste(italic(R) ^ 2, \" = 0.61\")*'  [0.56; 0.65]'" )

P1

#### P2 consumer-dependent ####

d_p2 <- fitted(fit_P2) %>%
  as_tibble() %>%
  bind_cols(data_observed)

P2 <- ggplot(data = d_p2, aes(x = Estimate , y = C)) +
  geom_abline(linetype = 2, color = "black", size = .75) +
  geom_point(size = 1.5, color = "black", alpha = 3/4) +
  # observed
  geom_linerange(aes(ymin = C-se, ymax = C+se), 
                 size = 0.75, color = "black") +
  
  geom_linerange(aes(xmin = Estimate - Est.Error, 
                     xmax = Estimate + Est.Error),
                 size = 0.75, color = "black")+
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  scale_y_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  personal_theme +
  labs(y = expression(Observed~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1})),
       x = expression(Predicted~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1}))) +
  annotate("text", x = 0.15, y = 0.6, parse = TRUE, size= 5, hjust=0,vjust=0,
           label = "'Arditi-Akcakaya consumer-dependent (P2):  '*'       '* paste(italic(R) ^ 2, \" = 0.50\")*'  [0.27; 0.64]'" )

P2

#### R2 hyperbolic ratio-dependent ####

d_r2 <- fitted(fit_R2) %>%
  as_tibble() %>%
  bind_cols(data_observed)

R2 <- ggplot(data = d_r2, aes(x = Estimate , y = C)) +
  geom_abline(linetype = 2, color = "black", size = .75) +
  geom_point(size = 1.5, color = "black", alpha = 3/4)  +
  # observed
  geom_linerange(aes(ymin = C-se, ymax = C+se), 
                 size = 0.75, color = "black") +
  
  geom_linerange(aes(xmin = Estimate - Est.Error, 
                     xmax = Estimate + Est.Error),
                 size = 0.75, color = "black")+
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  scale_y_continuous(limits = c(0, .6), breaks = seq(0, .6, 0.1))+
  personal_theme +
  labs(y = expression(Observed~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1})),
       x = expression(Predicted~~per~~capita~~consumption~~rate~""~(g~"·"~min^{-1}~"·"~ind.^{-1}))) +
  annotate("text", x = 0.15, y = 0.6, parse = TRUE, size= 5, hjust=0,vjust=0,
           label = "'Arditi-Ginzburg type II ratio-dependent (R2):  '*'       '* paste(italic(R) ^ 2, \" = 0.51\")*'  [0.33; 0.63]'" )

R2

#### Multi plot ####

Fig_S <- H1 + P2 + R2 + IVR + P1 + plot_layout(ncol = 2, nrow = 3) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag.position = c(0.1, 1),
        plot.tag = element_text(face = 'bold', size=16))

ggsave(filename = "Fig_Supplementary_OP_prova.pdf",
       plot = Fig_S, 
       width = 20,
       height = 24,
       path = "outputs_results/figures/",
       dpi = 600)

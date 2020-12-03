### Load packages####
library(readr)
library(tidyverse)
library(brms) # Bayesian Regression Models using 'Stan'
library(loo) # loo: Efficient Leave-One-Out Cross-Validation and WAIC for Bayesian Models
library(Rcpp) #  Seamless R and C++ Integration
library(rstan) # R Interface to Stan

#### Reload models with the readRDS() function####
fit0= readRDS("fit0.rds")
fit1= readRDS("fit1.rds")
fit2= readRDS("fit2.rds")
fit3= readRDS("fit3.rds")
fitR1= readRDS("fitR1.rds")
fitR2= readRDS("fitR2.rds")
fitR3= readRDS("fitR3.rds")
fitP1= readRDS("fitP1.rds")
fitP2= readRDS("fitP2.rds")
fitP3= readRDS("fitP3.rds")
### load table WAIC####
table_WAIC <- read_csv("outputs_results/tables/WAIC_table.csv")

### Plot WAIC values and standart errors####
table_WAIC$model=factor(table_WAIC$model, 
                        levels=c("fitP2","fitR1","fitR2","fitP1",
                      "fitR3" ,"fitP3","fit2","fit1","fit0","fit3"))
p<- table_WAIC[, 2:3] %>% 
  data.frame() %>% 
  rownames_to_column(var = "table_WAIC$model") %>%
  
  ggplot(aes(x    = table_WAIC$model, 
             y    = waic, 
             ymin = waic - se_waic, 
             ymax = waic + se_waic)) +
  geom_pointrange(shape = 21, 
                  color = "grey45",
                  fill = "black") +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "WAIC plot") +
  theme_classic() +
  theme(text= element_text(family = "sans"), #family = arial
        axis.ticks.y     = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y =element_text(size=10),
        axis.title =element_text(size=12),
        plot.title = element_text(hjust=0.5),
        aspect.ratio = .5) +
  scale_y_continuous(name   = "Widely applicable information criterion (WAIC) ", limits = c(-215.0, -170.0),
                     breaks = seq(-215.0, -170.0,5))+
  scale_x_discrete(name= " Functional response model")

p
### Saving WAIC plot #####
ggsave("WAIC_plot.png", 
       plot = p, 
       path ="outputs_results/figures/",
       scale = 2,
       dpi = 300,
       units = c( "cm"))

###Load LOO table ####
table_loo <- read_csv("outputs_results/tables/LOO_table.csv")
### Plot LOO values and standart errors####
table_loo$model=factor(table_loo$model, 
                       levels=c("fitR1","fitP2","fitR2","fitP1",
                                "fitR3","fitP3","fit2","fit1","fit0", 
                                "fit3"))
LOO<- table_loo[, 2:3] %>% 
  data.frame() %>% 
  rownames_to_column(var = "table_loo$model") %>%
  ggplot(aes(x    = table_loo$model, 
             y    = looic, 
             ymin = looic - se_looic, 
             ymax = looic + se_looic)) +
  geom_pointrange(shape = 21, 
                  color = "grey45",
                  fill = "black") +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "LOO plot") +
  theme_classic() +
  theme(text= element_text(family = "sans"), #family = arial
        axis.ticks.y     = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y =element_text(size=10),
        axis.title =element_text(size=12),
        plot.title = element_text(hjust=0.5),
        aspect.ratio = .5) +
  scale_y_continuous(name   = "Leave one out information criterion (LOO)",
                     limits = c(-215.0, -170.0),
                     breaks = seq(-215.0, -170.0,5))+
  scale_x_discrete(name= " Functional response model")

LOO
### Saving LOO plot #####
ggsave("LOO_plot.png",
       plot = LOO, 
       path ="outputs_results/figures/",
       dpi = 300,
       scale=2,
       units = c( "cm"))

# Based on the LOO comparisons, none of the PREDATOR/RATIO dependet models was a clear favorite.
#' Although the predator dependent model (i.e., fitP2) had lower numeric estimate than 
#' the ratio dependent (i.e., fitR2, fitR1), the standard errors on the difference scores were the same magnitude 
#' as the difference estimates themselves. As for comparing the two variants of the ratio dependent models 
#' directly, their WAIC estimates were almost indistinguishable.

### Load packages ####
library(readr)
library(tidyverse)
library(brms)
library(ggpubr)
library(ggplot2)
library(bayesplot)
#### Reload models with the readRDS() function ####
#### Reload models with the readRDS() function####
fit0 = readRDS("outputs_results/models/fit_00.rds")
fitR1 = readRDS("outputs_results/models/fit_R1.rds")
fitP1 = readRDS("outputs_results/models/fit_P1.rds")
fitP2 = readRDS("outputs_results/models/fit_P2.rds")
# My theme ####
th = theme_classic()+
  theme(
    panel.grid.major = element_blank(),
    aspect.ratio = 0.75,
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size = 0.75),
    text = element_text(family = "sans"),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    legend.key.size=unit(0.75,'lines'),
    legend.key.height=unit(1,"lines"),
    legend.text=element_text(size=12),
    strip.text = element_text(size = 14),
    strip.background = element_rect(colour=NA, fill=NA))

##### Graphical posterior predictive checks (PPC) #####
#' Compare the models with separate posterior predictive checks using pp_check().
#'The idea is that if the model is a good fit to the data we should be able to generate data 𝑦rep from the posterior predictive distribution that looks a lot like. the observed data y. That is, given y, the 𝑦rep we generate should be plausible.

null <- pp_check(fit0, type = "dens_overlay",nsamples = 500) + 
  ggtitle("Null model") + 
  th+
  scale_x_continuous(limits=c(-2, 6.5), breaks=seq(-2, 6.5,0.5))

R1 <-pp_check(fitR1,type = "dens_overlay", nsamples = 500) + 
  ggtitle("R1 model") +
  th +
  scale_x_continuous(limits=c(-2, 6.5), breaks=seq(-2, 6.5,0.5)) 

P1 <- pp_check(fitP1, type = "dens_overlay", nsamples = 500) + 
  ggtitle("P1 model")+
  th +
  scale_x_continuous(limits=c(-2, 6.5), breaks=seq(-2, 6.5,0.5))

P2 <- pp_check(fitP2, type = "dens_overlay", nsamples = 500) +
  ggtitle("P2 model") +
  th +
  scale_x_continuous(limits=c(-2, 6.5), breaks=seq(-2, 6.5,0.5))



multi = ggarrange(null, R1,P1, P2, nrow = 2, ncol = 2)
multi
ggsave(filename = "prova_pp_check.pdf",
       plot = multi, 
       device="pdf",
       path ="outputs_results/supplementary_materials/",
       dpi = 400)










### Null model Predictive checks#### 
pp_check(fitP2, nsamples = 5000, type = "stat_2d") + theme_bw(base_size = 20)

null = pp_check(fitP2, type = 'stat', 
                stat = "mean",
                nsamples = 5000, 
                binwidth = 0.00005) +
  theme(legend.position = "none") +
  #labs(x = "Mean consumption rate") +
  #scale_x_continuous(limits=c(0.14, 0.3), breaks=seq(0.14, 0.3,0.01)) +
  ggtitle("Null model")

null


### P2 Predictive checks#### 
P2_check = pp_check(fitP2, type = 'stat', stat = "mean",
                   nsamples = 10000, binwidth = 0.00005) +
  theme(legend.position = "none") +
  labs(x = "Mean consumption rate") +
  scale_x_continuous(limits=c(0.14, 0.3), breaks=seq(0.14, 0.3,0.01)) +
  ggtitle("P2: hyperbolic")
  
P2_check

### Fit P1 Predictive checks#### 

P1_check= pp_check(fitP1, type = 'stat',
                   stat = "mean", nsamples = 10000, binwidth=0.00005) +
  labs( x = NULL)+
  theme(legend.position = "none")+
  scale_x_continuous(limits=c(0.14, 0.3), breaks=seq(0.14, 0.3,0.01)) +
  ggtitle("P1: linear")

P1_check

###  P3 Predictive checks#### 

P3_check= pp_check(fitP3, type = 'stat',
                   stat = "mean", nsamples = 10000, binwidth=0.00005) +
  labs( x = "Mean consumption rate")+
  theme(legend.position = "none")+
  scale_x_continuous(limits=c(0.14, 0.3), breaks=seq(0.14, 0.3,0.01)) +
  ggtitle("P3: sigmoidal")

P3_check


##### Multi plot #####
multi = ggarrange(null, P1_check, P2_check, P3_check, nrow = 2, ncol = 2)
multi 

#'the method "stat_2d" shows distribution of a test quantity 𝑇(𝑦rep) compared 
#'to 𝑇(𝑦), the value of the quantity in the observed data.
#'pp_check will simulate 𝑆 datasets 𝑦rep1,…,𝑦rep𝑆, each containing 𝑁 observations. Here 𝑆 is the size of the posterior sample (the number of MCMC draws from
#' the posterior distribution of the model parameters) and 𝑁 is the length of 𝑦
#'We can then check if 𝑇(𝑦) is consistent with the distribution of (𝑇(𝑦yep1),…,𝑇(𝑦y
#' ep𝑆)). In the plot below we see that the mean of the observations is plausible when compared
#'  to the distribution of the means of the 𝑆 𝑦rep datasets:
 #### Null model , stat_2d #####
null_check_stat2 = pp_check(fit0, type  = "stat_2d", 
                          stat = c("mean", "sd"), nsamples = 10000)+
  labs( y = "Standard deviation", x=NULL)+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  ggtitle("Null model")

  scale_y_continuous(limits=c(0.002, 0.012), breaks=seq(0.002, 0.012,0.0015))+
  scale_x_continuous(limits=c(0.004, 0.015), breaks=seq(0.004, 0.015,0.0015))

null_check_stat2
#### P2 model , stat_2d #####

P2_check_stat2 = pp_check(fitP2, type  = "stat_2d", 
                        stat = c("mean", "sd"), nsamples = 2000)+
  labs( y = "Standard deviation", x=NULL)+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  ggtitle("P2: hyperbolic")
  
P2_check_stat2

#### P1 model , stat_2d #####

P1_check_stat2 = pp_check(fitP1, type  = "stat_2d", 
                        stat = c("mean", "sd"), nsamples = 2000)+
labs( x = "Mean consumption rate", y = "Standard deviation")+
    theme(plot.title = element_text(hjust = 0.5),legend.position = "none") +
  ggtitle("P1: linear")
  # scale_y_continuous(limits=c(0.002, 0.012), breaks=seq(0.002, 0.012,0.0015))+
 # scale_x_continuous(limits=c(0.004, 0.015), breaks=seq(0.004, 0.015,0.0015))
  
P1_check_stat2

#### P3 , stat_2d #####
P3_check_stat2 = pp_check(fitP3, type  = "stat_2d", 
                          stat = c("mean", "sd"), nsamples = 10000)+
  labs( x = "Mean consumption rate", y = "Standard deviation")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none") +
  ggtitle("P3: sigmoidal")


P3_check_stat2


#'In the plots above, the dark line is the distribution of the observed outcomes y and each of the 10000 lighter points is the kernel density estimate of one of the replications of y from the posterior predictive distribution (i.e., one of the rows in yrep). This plot makes it easy to see that what model fails to predict the consumption rate observed

### Multiplot #####

##### Multi plot #####
multi_2 = ggarrange(null_check_stat2, P1_check_stat2,
                    P2_check_stat2, P3_check_stat2, nrow = 2, ncol = 2)
multi_2
### Saving  multiplot #####
ggsave("Predictive_Checks_plot.png", 
       plot = multi, path ="outputs_results/figures/",
       scale = 2, dpi = 300,
       units = c( "cm"))

### other possible plots ###




### visual and numerical summaries of model parameters and convergence diagnostics for MCMC simulations.
#The shinystan R package, allows for interactive exploration of model diagnostics ####
#Just use launch_shinystan on any model object from brms
install.packages("shinystan")
launch_shinystan(fitP1)



  

### Load packages####
library(readr)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(tidyverse)
library(brms)
library(loo)
library(rstanarm)
library(StanHeaders)
library(Rcpp)
library(rstan)
library(parallel)
library(bayesplot)
library(ggplot2)
library(rcartocolor)
library(ggpubr)
library(ggmcmc)
#### Reload models with the readRDS() function####
fit0= readRDS("fit0.rds")
fitP1= readRDS("fitP1.rds")
fitP2= readRDS("fitP2.rds")
fitP3= readRDS("fitP3.rds")

#Because we now have multiple parameters of interest we can visualize the 
#convergence in so-called caterpillar plots. We see that after a few iterations 
#(far before the end of the warm up period of 5000), 
#the 5 chains converge into a nice fat caterpillar.
plot_model(fitP1,show.values = TRUE, 
           bpe = "mean",
           bpe.style = "dot",
           prob.inner = .4,
           prob.outer = .8,
           colors = "bw")+
  labs( x = "Parameters", y = "Estimate", title = "Model P1")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        axis.line = element_line(colour = "black"))
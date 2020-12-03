### Load packages##
library(readr)
library(tidyverse)
require(fitdistrplus)
library(logspline)
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

#Which distribution fit my data best ? Verificar a distribuçao de frequencia de var. respuesta. 
#See https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best

#use the function descdist to gain some ideas about possible candidate data distributions.
descdist(data$consumption_rate, discrete = FALSE)
# It seems that possible distribution is the BETA, but he beta distribution is 
#a family of continuous probability distributions defined on the interval [0, 1]. It is not the case. 
#test with other distributions
fit.normal = fitdist(data$consumption_rate,"norm")
fit.beta = fitdist(data$consumption_rate,"beta")
fit.gamma=fitdist(data$consumption_rate,"gamma")
fit.weibull=fitdist(data$consumption_rate,"weibull")

#Graphical comparison of multiple fitted distributions (for non-censored data) 
cdfcomp(list(fit.normal,fit.beta,fit.gamma,fit.weibull),horizontals=F, 
        addlegend=TRUE, legendtext=c("Normal","beta","Gamma","Weibull"), lwd =1.7)

qqcomp(list(fit.normal,fit.beta,fit.gamma,fit.weibull),
       addlegend=TRUE,legendtext=c("Normal","beta","Gamma","Weibull"))

### Load packages####
library(readr)
library(tidyverse)
library(brms)
library(loo)
library(rstanarm)
library(Rcpp)
library(rstan)
library(parallel)
library(bayesplot)
### Read cleaned data ####
data=read.csv("data/processed/data_cleaned.csv",header=T)

### Null model fitting - constant  #####
#'Prior choice recommendations. retrived from:
#'https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
#'Weakly informative prior, very weak: normal(0, 10);
#'Generic weakly informative prior: normal(0, 1);
#'Specific informative prior: normal(0.4, 0.2) or whatever.
fit0 <- brm(formula = bf(consumption_rate ~ a, # a = attack rate // in this null model we consider a as the maximum killing rate
                       a ~ 1, nl = TRUE),
          data = data, family = gaussian(),
          prior = c(set_prior("normal(0, 1)", nlpar = "a", lb = 0)),
                   # set_prior("exponential(1)", class = "sigma")), # residual standard deviation
          chains = 3,  #'estimation of posterior probability distributions using 
                       #'a stochastic process known as Markov chain Monte Carlo (MCMC) estimation
                       #'“Using 3 or 4 chains is conventional, and quite often more than enough to reassure
                       #' us that the sampling is working properly” 
          iter = 10000,
          warmup = 500,
          sample_prior = TRUE,
          cores = getOption("mc.cores",1),
          control = list(adapt_delta = 0.99, max_treedepth = 15),
          seed = 7777) # A single numeric value passed to set.seed to make results reproducible.
         # file = "fit0" : Either NULL or a character string. In the latter case, the fitted model object is
                          #saved via saveRDS in a file named after the string supplied in file. The .rds
                          #extension is added automatically. If the file already exists, brm will load and
                          #return the saved model object instead of refitting the model. As existing files
                         #won’t be overwritten, you have to manually remove the file in order to refit
                         # and save the model under an existing

fit0 = add_criterion(fit0, c("loo","waic"),#Names of model fit criteria to compute. Currently supported are "loo", "waic",
                                                  #"kfold", "loo_subsample", "bayes_R2" (Bayesian R-squared), "loo_R2" (LOOadjusted R-squared), 
                                                  # and "marglik" (log marginal likelihood).
                    model_name = "fit0",
                    overwrite = T, file = "fit0")
summary(fit0)
#'Using plot() for a brm() fit returns both density and trace lots for the parameters.
plot(fit0)

##### FitP1_Predator dependent_linear #####

fitP1 <- brm(formula = bf(consumption_rate ~ a*Algae_density*Fish_density^m, 
                        a ~ 1, m ~ 1, nl = TRUE),
           data = data, family = gaussian(),
           prior = c(set_prior("normal(0, 1)", nlpar = "a", lb = 0), # a= Proportion of algae that an individual consume per unit of time
                     set_prior("uniform(-3, 3)", nlpar = "m", lb = -3, ub= 3)),
           chains = 3,
           iter = 10000,
           warmup = 500,
           sample_prior = TRUE,
           cores = getOption("mc.cores",1),
           seed = 7777,
           control = list(adapt_delta = 0.99, max_treedepth = 15))
         
fitP1 = add_criterion(fitP1,c("loo","waic"),model_name = "fitP1",
                     overwrite = T, file = "fitP1")
summary(fitP1)
plot(fitP1)

#  plot the difference between the prior and the posterior distribution of different parameters
parnames(fitP1)
plot(hypothesis(fitP1, " a_Intercept > 0"))
plot(hypothesis(fitP1, " m_Intercept > 0"))
#Marginal posterior predictive checks
pp_check(fitP1, nsamples = 500)

#FitP2_theta = 1 / hyperbolic######
#The fit will take 234 seconds 
fitP2 <- brm(formula = bf(consumption_rate ~ a*(Algae_density*Fish_density^m)/(1+a*h*(Algae_density*Fish_density^m)), 
                        a ~ 1, h ~ 1, m ~ 1, nl = TRUE),
           data = data, family = gaussian(),
           prior = c(set_prior("normal(0, 1)", nlpar = "a", lb = 0),
                     set_prior("uniform(-3, 3)", nlpar = "m", lb = -3, ub = 3),
                     set_prior("normal(0, 1)", nlpar = "h", lb = 0)),
           chains = 3,
           iter = 10000,
           warmup = 500,
           sample_prior = TRUE,
           cores = getOption("mc.cores",1),
           seed = 7777,
           control = list(adapt_delta = 0.99, max_treedepth = 15))
          
fitP2 = add_criterion(fitP2,c("loo","waic"), model_name = "fitP2",
                      overwrite = T, file = "fitP2")

summary(fitP2)
print(fitP2, digits=4)
plot(fitP2)
#  plot the difference between the prior and the posterior distribution of different parameters
plot(hypothesis(fitP2, " a_Intercept > 0"))
plot(hypothesis(fitP2, " h_Intercept > 0"))
plot(hypothesis(fitP2, " m_Intercept < 0"))
#Marginal posterior predictive checks
pp_check(fitP2,nsamples = 500)
plot(conditional_effects(fitP2), points = TRUE)
#FitP3_theta = 2 / Sigmoidal #####
# Fit will take 122 seconds
fitP3 <- brm(formula = bf(consumption_rate ~ a*(Algae_density^2)*Fish_density^m/(1+a*h*(Algae_density^2)*Fish_density^m), 
                        a ~ 1, h ~ 1, m ~ 1, nl = TRUE),
           data = data, family = gaussian(),
           prior = c(set_prior("normal(0, 1)", nlpar = "a", lb = 0),
                     set_prior("normal(-3, 3)", nlpar = "m", lb = -3, ub=3),
                     set_prior("normal(0, 1)", nlpar = "h", lb = 0)),
           chains = 3,
           iter = 10000,
           warmup = 500,
           sample_prior = TRUE,
           cores = getOption("mc.cores",1),
           seed = 7777,
           control = list(adapt_delta = 0.99, max_treedepth = 15))

fitP3= add_criterion(fitP3,c("loo","waic"),model_name = "fitP3",
                     overwrite = T, file = "fitP3")

summary(fitP3)
plot(fitP3)
#  plot the difference between the prior and the posterior distribution of different parameters
plot(hypothesis(fitP3, " a_Intercept > 0"))
plot(hypothesis(fitP3, " h_Intercept > 0"))
plot(hypothesis(fitP3, " m_Intercept > 0"))
#Marginal posterior predictive checks
pp_check(fitP3,nsamples = 500)
plot(conditional_effects(fitP3), points = TRUE)


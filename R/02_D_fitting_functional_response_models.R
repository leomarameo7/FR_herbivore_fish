### load libraries ####
library(brms); library(readxl);library(dplyr); library(ggplot2); library(ggpubr)
library(performance)
#READ DATA
data_s <- read_csv('data/processed/data_fitting_models.csv')
           
###### BAYESIAN REGRESSION MODELS USING STAN #####
chains = 3 #number of Markovian chains to run
iter = 3000 #iterations per Markovian chain
warmup = 1000 #burn-in iterations before parameter sampling
cores = 3 #set a computer core for each chain if possible
seed = 777 #make model run reproducible 
control = list(adapt_delta = 0.95, max_treedepth = 15)

#FUNCTIONAL RESPONSE MODELS

#NULL MODEL (independent constant)
fit_00 <- brm(formula = bf(C ~ c, nl = T, c ~ 1), #model eq, and parameter dependencies
              data = data_s, family = gaussian(link = 'identity'),  #distrib. family
              prior = c(set_prior('normal(0, .5)', nlpar = 'c', lb = 0)), #priors
              chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
              sample_prior = T, #save parameter sampling information 
              file = 'outputs_results/models/fit_with_fish_density/fit_00',  #save model output
              control = control)#control chain steps
print(fit_00, digits = 4)       #to see posterior summary results
r2_00 <- r2_bayes(fit_00, robust = TRUE, ci = 0.95) #Model fit according to R2 

#HOLLING TYPE I (resource dependent - linear)
fit_H1 <- brm(formula = bf(C ~ a * N, 
                           nl = T, a ~ 1),
              data = data_s, family = gaussian(link = 'identity'),
              prior = c(set_prior('normal(0, .5)', nlpar = 'a', lb = 0)),
              chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
              sample_prior = T, file = 'outputs_results/models/fit_with_fish_density/fit_H1',  control = control)
print(fit_H1, digits = 4)
r2_H1 <- r2_bayes(fit_H1, robust = TRUE, ci = 0.95)
r2_H1
#HOLLING TYPE II (resource dependent - hyperbolic)
fit_H2 <- brm(formula = bf(C ~ a * N / (1 + a * h * N), 
                           nl = T, a ~ 1, h ~ 1), 
              data = data_s, family = gaussian(link = 'identity'),
              prior = c(set_prior('normal(0, .5)', nlpar = 'a', lb = 0),
                        set_prior('normal(0, 2)', nlpar = 'h', lb = 0)),
              chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
              sample_prior = T, file = 'outputs_results/models/fit_with_fish_density/fit_H2',  control = control)
print(fit_H2, digits = 4)
r2_H2 <- r2_bayes(fit_H2, robust = TRUE, ci = 0.95)

#ARDITI-GINZBURG TYPE I (ratio dependent - linear)
fit_R1 <- brm(formula = bf(C ~ a * (N/P), 
                           nl = T, a ~ 1), 
              data = data_s, family = gaussian(link = 'identity'),
              prior = c(set_prior('normal(0, .5)', nlpar = 'a', lb = 0)),
              chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
              sample_prior = T, file = 'outputs_results/models/fit_with_fish_density/fit_R1',  control = control)
print(fit_R1, digits = 4)
r2_bayes(fit_R1, robust = TRUE, ci = 0.95)
#ARDITI-GINZBURG TYPE II (ratio dependent - hyperbolic)
fit_R2 <- brm(formula = bf(C ~ a * (N/P) / (1 + a * h * (N/P)), 
                           nl = T, a ~ 1, h ~ 1),
              data = data_s, family = gaussian(link = 'identity'),
              prior = c(set_prior('normal(0, .5)', nlpar = 'a', lb = 0),
                        set_prior('normal(0, 2)', nlpar = 'h', lb = 0)),
              chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
              sample_prior = T, file = 'outputs_results/models/fit_with_fish_density/fit_R2',  control = control)
print(fit_R2, digits = 4)

r2_bayes(fit_R2, robust = TRUE, ci = 0.95)


#ARDITI-AKCAKAYA TYPE I (predator dependent - linear)
fit_P1 <- brm(formula = bf(C ~ a * (N/P^m), 
                           nl = T, a ~ 1, m ~ 1), 
              data = data_s, family = gaussian(link = 'identity'),
              prior = c(set_prior('normal(0, .5)', nlpar = 'a', lb = 0),
                        set_prior('normal(1, .5)', nlpar = 'm')),
              chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
              sample_prior = T, file = 'outputs_results/models/fit_with_fish_density/fit_P1',  control = control)
print(fit_P1, digits = 4)
r2_bayes(fit_P1, robust = TRUE, ci = 0.95)

#ARDITI-AKCAKAYA TYPE II (predator dependent - hyperbolic)
fit_P2 <- brm(formula = bf(C ~ a * (N/P^m) / (1 + a * h * (N/P^m)), 
                           nl = T, a ~ 1, h ~ 1, m ~ 1), 
              data = data_s, family = gaussian(link = 'identity'),
              prior = c(set_prior('normal(0, .5)', nlpar = 'a', lb = 0),
                        set_prior('normal(0, 2)', nlpar = 'h', lb = 0),
                        set_prior('normal(1, .5)', nlpar = 'm')),
              chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
              sample_prior = T, file = 'outputs_results/models/fit_with_fish_density/fit_P2',  control = control)
print(fit_P2, digits = 4)
r2_bayes(fit_P2, robust = TRUE, ci = 0.95)

#IVLEV SIMPLE (resource dependent - hyperbolic)
fit_IV <- brm(formula = bf(C ~ a * (1 - exp(-d * N)), 
                           nl = T, a ~ 1, d ~ 1), 
              data = data_s, family = gaussian(link = 'identity'),
              prior = c(set_prior('normal(0, .5)', nlpar = 'a', lb = 0),
                        set_prior('normal(0, 1)', nlpar = 'd', lb = 0)),
              chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
              sample_prior = T, file = 'outputs_results/models/fit_with_fish_density/fit_IV',  control = control)
print(fit_IV, digits = 4)
r2_bayes(fit_IV, robust = TRUE, ci = 0.95)

#IVLEV RATIO (ratio dependent - hyperbolic)
fit_IR <- brm(formula = bf(C ~ a * (1 - exp(-d * (N/P))), 
                           nl = T, a ~ 1, d ~ 1), 
              data = data_s, family = gaussian(link = 'identity'),
              prior = c(set_prior('normal(0, .5)', nlpar = 'a', lb = 0),
                        set_prior('normal(0, 1)', nlpar = 'd', lb = 0)),
              chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
              sample_prior = T, file = 'outputs_results/models/fit_with_fish_density/fit_IR',  control = control)
print(fit_IR, digits = 4)
r2_bayes(fit_IR, robust = TRUE, ci = 0.95)


########################################################################################
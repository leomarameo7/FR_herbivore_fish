# LOAD PACKAGES
library(brms) #bayesian modelling with STAN code
library(readxl) #read excel data
library(tidyverse)
##### READ DATA AND SET CATEGORICAL VARIABLES #####
data <- read_excel('data/processed/data_cleaned.xlsx')
data$Sequence <- as.factor(data$Sequence) #order of the experiments during a day

#####NULL MODEL (non dependent of algae and fish densities)####
fit_00 <- brm(formula = bf(Cons_rate ~ (a+b)*fish_eating_minute, 
                           a ~ 1, b ~ 1|Sequence, nl = TRUE),
              data = data, family = gaussian(link = 'identity'),
              prior = c(set_prior("normal(0, 0.5)", nlpar = "a", lb = 0),
                        set_prior("normal(0, 0.5)", nlpar = "b"),
                        set_prior("cauchy(0, 0.1)", class  = "sigma")),
              chains = 3, 
              iter = 1500, 
              warmup = 500, 
              sample_prior = TRUE,
              save_pars = save_pars(all = T),
              seed = 777,
              control = list(adapt_delta = 0.99, max_treedepth = 15),
              file = "outputs_results/models/fit_00")

#HOLLING TYPE I (linear function, only algae dependent)
fit_H1 <- brm(formula = bf(Cons_rate ~ (Algae_prev_d*a+b)*fish_eating_minute, 
                           a ~ 1, b ~ 1|Sequence, nl = TRUE),
              data = data, family = gaussian(link = 'identity'),
              prior = c(set_prior("normal(0, 0.5)", nlpar = "a", lb = 0),
                        set_prior("normal(0, 0.5)", nlpar = "b"),
                        set_prior("cauchy(0, 0.1)", class  = "sigma")),
              chains = 3, iter = 1500, warmup = 500, sample_prior = TRUE,
              save_pars = save_pars(all = T),
              seed = 777,
              control = list(adapt_delta = 0.99, max_treedepth = 15),
              file = "outputs_results/models/fit_H1")

#HOLLING TYPE II (hyperbolic function, only algae dependent)
fit_H2 <- brm(formula = bf(Cons_rate ~ (Algae_prev_d*a/(1+a*h*Algae_prev_d)+b)*fish_eating_minute, 
                           a ~ 1, b ~ 1|Sequence, h ~ 1, nl = TRUE),
              data = data, family = gaussian(link = 'identity'),
              prior = c(set_prior("normal(0, 0.5)", nlpar = "a", lb = 0),
                        set_prior("normal(0, 2)", nlpar = "h", lb = 0),
                        set_prior("normal(0, 0.5)", nlpar = "b"),
                        set_prior("cauchy(0, 0.1)", class  = "sigma")),
              chains = 3, iter = 1500, warmup = 500, sample_prior = TRUE,
              save_pars = save_pars(all = T),
              seed = 777,
              control = list(adapt_delta = 0.99, max_treedepth = 15),
              file = "outputs_results/models/fit_H2")

#ARDITI-GINZBURG TYPE I (linear function, ratio algae:fish dependent)
fit_R1 <- brm(formula = bf(Cons_rate ~ ((Algae_prev_d/Fish_density)*a+b)*fish_eating_minute, 
                           a ~ 1, b ~ 1|Sequence, nl = TRUE),
              data = data, family = gaussian(link = 'identity'),
              prior = c(set_prior("normal(0, 0.5)", nlpar = "a", lb = 0),
                        set_prior("normal(0, 0.5)", nlpar = "b", lb = 0),
                        set_prior("cauchy(0, 0.1)", class  = "sigma")),
              chains = 3, iter = 1500, warmup = 500, sample_prior = TRUE,
              save_pars = save_pars(all = T),
              seed = 777,
              control = list(adapt_delta = 0.99, max_treedepth = 15),
              file = "outputs_results/models/fit_R1")

#ARDITI-GINZBURG TYPE II (hyperbolic function, ratio algae:fish dependent)
fit_R2 <- brm(formula = bf(Cons_rate ~ ((Algae_prev_d/Fish_density)*a/(1+a*h*(Algae_prev_d/Fish_density))+b)*fish_eating_minute, 
                           a ~ 1, b ~ 1|Sequence, h ~ 1, nl = TRUE),
              data = data, family = gaussian(link = 'identity'),
              prior = c(set_prior("normal(0, 0.5)", nlpar = "a", lb = 0),
                        set_prior("normal(0, 2)", nlpar = "h", lb = 0),
                        set_prior("normal(0, 0.5)", nlpar = "b"),
                        set_prior("cauchy(0, 0.1)", class  = "sigma")),
              chains = 3, iter = 1500, warmup = 500, sample_prior = TRUE,
              save_pars = save_pars(all = T),
              seed = 777,
              control = list(adapt_delta = 0.99, max_treedepth = 15),
              file = "outputs_results/models/fit_R2")


#ARDITI-AKCAKAYA TYPE I (linear function with exponential fish interference)
fit_P1 <- brm(formula = bf(Cons_rate ~ ((Algae_prev_d/Fish_density^m)*a+b)*fish_eating_minute, 
                           a ~ 1, b ~ 1|Sequence, m ~ 1, nl = TRUE),
              data = data, family = gaussian(link = 'identity'),
              prior = c(set_prior("normal(0, 0.5)", nlpar = "a", lb = 0),
                        set_prior("normal(1, 0.5)", nlpar = "m"),
                        set_prior("normal(0, 0.5)", nlpar = "b"),
                        set_prior("cauchy(0, 0.1)", class  = "sigma")),
              chains = 3, iter = 1500, warmup = 500, sample_prior = TRUE,
              save_pars = save_pars(all = T),
              seed = 777,
              control = list(adapt_delta = 0.99, max_treedepth = 15),
              file = "outputs_results/models/fit_P1")

#ARDITI-AKCAKAYA TYPE II (hyperbolic function with exponential fish interference)
fit_P2 <- brm(formula = bf(Cons_rate ~ ((Algae_prev_d/Fish_density^m)*a/(1+a*h*(Algae_prev_d/Fish_density^m))+b)*fish_eating_minute, 
                           a ~ 1, b ~ 1|Sequence, h ~ 1, m ~ 1, nl = TRUE),
              data = data, family = gaussian(link = 'identity'),
              prior = c(set_prior("normal(0, 0.5)", nlpar = "a", lb = 0),
                        set_prior("normal(0, 2)", nlpar = "h", lb = 0),
                        set_prior("normal(1, 0.5)", nlpar = "m"),
                        set_prior("normal(0, 0.5)", nlpar = "b"),
                        set_prior("cauchy(0, 0.1)", class  = "sigma")),
              chains = 3, iter = 1500, warmup = 500, sample_prior = TRUE,
              save_pars = save_pars(all = T),
              seed = 777,
              control = list(adapt_delta = 0.99, max_treedepth = 15),
              file = "outputs_results/models/fit_P2")

#IVLEV resource-dependent (only algae dependent)
fit_IV <- brm(formula = bf(Cons_rate ~ (a*(1-exp(-d*Algae_prev_d))+b)*fish_eating_minute, 
                           a ~ 1, b ~ 1|Sequence, d ~ 1, nl = TRUE),
              data = data, family = gaussian(link = 'identity'),
              prior = c(set_prior("normal(0, 0.5)", nlpar = "a", lb = 0),
                        set_prior("normal(0, 2)", nlpar = "d", lb = 0),
                        set_prior("normal(0, 0.5)", nlpar = "b"),
                        set_prior("cauchy(0, 0.1)", class  = "sigma")),
              chains = 3, iter = 1500, warmup = 500, 
              sample_prior = TRUE,
              save_pars = save_pars(all = T),
              seed = 777,
              control = list(adapt_delta = 0.99, max_treedepth = 15),
              file = "outputs_results/models/fit_IV")

# IVLEV (ratio dependent)
fit_IVR <- brm(formula = bf(Cons_rate ~ (a*(1-exp(-d*Algae_prev_d/Fish_density))+b)*fish_eating_minute, 
                           a ~ 1, b ~ 1|Sequence, d ~ 1, nl = TRUE),
              data = data, family = gaussian(link = 'identity'),
              prior = c(set_prior("normal(0, 0.5)", nlpar = "a", lb = 0),
                        set_prior("normal(0, 2)", nlpar = "d", lb = 0),
                        set_prior("normal(0, 0.5)", nlpar = "b"),
                        set_prior("cauchy(0, 0.1)", class  = "sigma")),
              chains = 3, iter = 1500, warmup = 500, sample_prior = TRUE,
              save_pars = save_pars(all = T),
              seed = 777,
              control = list(adapt_delta = 0.99, max_treedepth = 15),
              file = "outputs_results/models/fit_IVR")


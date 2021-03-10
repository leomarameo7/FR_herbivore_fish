### Load packages####
library(readr)
library(tidyverse)
library(brms) # Bayesian Regression Models using 'Stan'
library(loo) # loo: Efficient Leave-One-Out Cross-Validation for Bayesian Models
#### COMPARE MODELS ####
#'Leave-one-out cross-validation (LOO-CV, or LOO for short) and the widely applicable information criterion (WAIC) are methods for estimating pointwise out-of-sample prediction accuracy from a fitted Bayesian model using the log-likelihood evaluated at the posterior simulations of the parameter values.
#'LOO and WAIC have various advantages over simpler estimates of predictive error such as AIC and DIC but are less used in practice because they involve additional computational steps.
#'Here we’ll compare the models with the loo() function, 
#### Reload models with the readRDS() function####
fit0 <- readRDS("outputs_results/models/fit_00.rds")
fit_H1 <- readRDS("outputs_results/models/fit_H1.rds")
fit_H2 <- readRDS("outputs_results/models/fit_H2.rds")
fitR1 <- readRDS("outputs_results/models/fit_R1.rds")
fitR2 <- readRDS("outputs_results/models/fit_R2.rds")
fitP1 <- readRDS("outputs_results/models/fit_P1.rds")
fitP2 <- readRDS("outputs_results/models/fit_P2.rds")
fit_Ivlev <- readRDS("outputs_results/models/fit_IV.rds")
fit_Ivlev_RD <- readRDS("outputs_results/models/fit_IV_RD.rds")
fit_BD_linear <- readRDS("outputs_results/models/fit_BD_linear.rds")
fit_BD_hyperbolic <- readRDS("outputs_results/models/fit_BD_hyperbolic.rds")
### LOOIC ####
loo = loo(fit0,
                fit_H1,
                fit_H2,
                fitR1,
                fitR2,
                fitP1,
                fitP2,
                fit_Ivlev,
          fit_Ivlev_RD,
          fit_BD_linear ,
          fit_BD_hyperbolic, 
          reloo = TRUE) # we  see in the individual LOO outputs that there are several problematic observations for which the approximations may have not have been very accurate.We use "reloo" to calculate the ELPD without the assumption that these observations are negligible. This will refit the model [n] times to compute the ELPDs for the problematic observations directly.

# moment_match = TRUE : Instead of refitting with MCMC, we can perform a faster moment matching correction to the importance sampling for the problematic observations. This can be done with the loo_moment_match() function in the loo package, which takes our existing loo object as input and modifies it.

loo_1 = as.data.frame(print(loo$diffs,simplify=F)) 
loo_1 = loo_1 %>% 
  select(elpd_loo,elpd_diff, se_diff) %>%
  mutate_if(is.numeric, round,digits = 2)
View(loo_1)

#### Export LOOIC table to CSV #####
write.csv(loo_1,
          "outputs_results/tables/LOO_table.csv", row.names = T)


#' the models have been rank ordered from the lowest to the highest elpd_diff . 



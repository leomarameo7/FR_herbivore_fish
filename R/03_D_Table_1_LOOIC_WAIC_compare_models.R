### Load packages####
library(readr)
library(tidyverse)
library(brms) # Bayesian Regression Models using 'Stan'
library(loo) # loo: Efficient Leave-One-Out Cross-Validation and WAIC for Bayesian Models
library(Rcpp) #  Seamless R and C++ Integration
library(rstan) # R Interface to Stan
#### COMPARE MODELS ####
#'Leave-one-out cross-validation (LOO-CV, or LOO for short) and the widely applicable information criterion (WAIC)
#' are methods for estimating pointwise out-of-sample prediction accuracy from a fitted Bayesian model using 
#' the log-likelihood evaluated at the posterior simulations of the parameter values.
#'LOO and WAIC have various advantages over simpler estimates of predictive error such as 
#'AIC and DIC but are less used in practice because they involve additional computational steps.
#'Here we’ll compare the models with the loo_compare() function, 
# First by the LOO and then by the WAIC.
#### Reload models with the readRDS() function####
fit0= readRDS("fit0.rds")
#fit1= readRDS("fit1.rds")
#fit2= readRDS("fit2.rds")
#fit3= readRDS("fit3.rds")
#fitR1= readRDS("fitR1.rds")
#fitR2= readRDS("fitR2.rds")
#fitR3= readRDS("fitR3.rds")
fitP1= readRDS("fitP1.rds")
fitP2= readRDS("fitP2.rds")
fitP3= readRDS("fitP3.rds")
#fit_bed_1 = readRDS("fit_bed_1.rds")
#fit_bed_2 = readRDS("fit_bed_2.rds")


### LOOIC ####
loo =loo_compare(fit0,
                #fit1,
               # fit2,
               # fit3,
                #fitR1,
                #fitR2,
                #fitR3,
                fitP1,
                fitP2,
                fitP3,
                #fit_bed_1,
                #fit_bed_2, 
                criterion = "loo")
loo = as.data.frame(print(loo,simplify=F))
loo$model = row.names(loo)
loo = loo %>%
  mutate_if(is.numeric, round,digits = 2)
#### LOOIC weights: they are calculated by taking averages of log- likelihood over the posterior distribution.
#And they are also just an estimate of out-of-sample deviance. 
l = as_tibble(model_weights(fit0, 
                          # fit1,
                          # fit2,
                          # fit3,
                          # fitR1,
                          # fitR2,
                          # fitR3,
                           fitP1,
                           fitP2,
                           fitP3,
                          # fit_bed_1,
                           #fit_bed_2,
                           weights = "loo", method = "stacking")) %>% #'The stacking method (method="stacking"), which is the default 
  # 'for loo_model_weights(), combines all models by maximizing the leave-one-out 
  #' predictive density of the combination distribution. 
  #' That is, it finds the optimal linear combining weights for maximizing the leave-one-out log score.
  rename(weight = value) %>% 
  mutate(model  = c("fit0",
                   # "fit1",
                   # "fit2",
                   # "fit3",
                   # "fitR1",
                   # "fitR2",
                   # "fitR3",
                    "fitP1", 
                    "fitP2",
                    "fitP3"),
                   # "fit_bed_1",
                   # "fit_bed_2"),
                   weight = weight %>%
           round(digits = 2)) %>% 
  select(model, weight) %>% 
  arrange(desc(weight))

table_LOO = full_join(x = loo,y = l, by=NULL)
table_LOO = table_LOO %>%
  select(model, looic, se_looic ,weight, elpd_diff,se_diff, elpd_loo,  
         se_elpd_loo ,p_loo,se_p_loo)

View(table_LOO)
#### Export LOOIC table to CSV #####
write.csv(table_LOO,
          "outputs_results/tables/LOO_table.csv",row.names = FALSE)


#' the models have been rank ordered from the lowest to the highest elpd_diff . 
#' Note also the weights !
#'  elpd_waic= expected log pointwise predictive density
#'  waic = -2 * elpd_waic (i.e., converted to the deviance scale)
#'  p_waic= estimated effective number of parameters

### WAIC ####
waic = loo_compare(fit0,fitP1, fitP2, fitP3, criterion = "waic")
waic = as.data.frame(print(waic,simplify=F))
waic$model = row.names(waic)

waic = waic %>%
  mutate_if(is.numeric, round,digits = 2)
#### WAIC weights: they are calculated by taking averages of log- likelihood over the posterior distribution.
#And they are also just an estimate of out-of-sample deviance. 
w = as_tibble(model_weights(fit0, fitP1, fitP2, fitP3,
                           weights = "waic",method = "stacking")) %>% 
  rename(weight = value) %>% 
  mutate(model  = c("fit0",  "fitP1", "fitP2", "fitP3"),
         weight = weight %>% round(digits = 2)) %>% 
  select(model, weight) %>% 
  arrange(desc(weight))

table_WAIC=full_join(x = waic,y = w, by=NULL)

table_WAIC= table_WAIC %>%
  select(model,waic, se_waic,weight, elpd_diff,se_diff, elpd_waic,  
         se_elpd_waic ,p_waic,se_p_waic)

View(table_WAIC)

#### Export WAIC table to CSV #####
write.csv(table_WAIC,
          "outputs_results/tables/WAIC_table.csv",
          row.names = FALSE)

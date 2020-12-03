fit0 = readRDS("fit0.rds")
fit1 = readRDS("fit1.rds")
fit2 = readRDS("fit2.rds")
fit3 = readRDS("fit3.rds")
fitR1= readRDS("fitR1.rds")
fitR2= readRDS("fitR2.rds")
fitR3= readRDS("fitR3.rds")
fitP1= readRDS("fitP1.rds")
fitP2= readRDS("fitP2.rds")
fitP3= readRDS("fitP3.rds")

data=read.csv("data/processed/data_cleaned.csv",header=T)


##### Fit 1_Prey dependent models_linear ####
fit1<-brm(formula = bf(consumption_rate ~ a*Algae_density, 
                       a ~ 1, nl = TRUE),
          data = data, family = gaussian(),
          prior = c(set_prior("normal(0, 1)", nlpar = "a", lb = 0)),
          #set_prior("half_Cauchy (0,5) class = "sigma")), By default, sigma has a half cauchy prior with 'mean' 0 and 'standard deviation' 5. 
          chains = 3,
          iter = 3000,
          warmup = 500,
          sample_prior = TRUE,
          cores = getOption("mc.cores",1),
          seed = 7777,
          control = list(adapt_delta = 0.99, max_treedepth = 15))

fit1= add_criterion(fit1,c("loo","waic"),model_name = "fit1",
                    overwrite = T, file = "fit1") 
summary(fit1)


# Using plot() for a brm() fit returns both density and trace lots for the parameters.
plot(fit1)
plot(hypothesis(fit1, " a_Intercept > 0"))

#Marginal posterior predictive checks
pp_check(fit1,nsamples = 500)
# predicted means of the response conditional on all other predictors 
#(whose values you can specify via the conditions argument). 

plot(conditional_effects(fit1), points = TRUE)

#####Fit2 / theta = 1 / hyperbolic / prey-dependent####
#h = handling time 
fit2<-brm(formula = bf(consumption_rate ~ a*Algae_density/(1+a*h*Algae_density), 
                       a ~ 1, h ~ 1, nl = TRUE),
          data = data, family = gaussian(),
          prior = c(set_prior("normal(0, 1)", nlpar = "a", lb = 0),
                    set_prior("normal(0, 1)", nlpar = "h", lb = 0)),
          #set_prior("exponential(1)", class = "sigma")),
          chains = 3,
          iter = 3000,
          warmup = 500,
          sample_prior = TRUE,
          cores = getOption("mc.cores",1),
          seed = 7777,
          control = list(adapt_delta = 0.99, max_treedepth = 15))

fit2= add_criterion(fit2,c("loo","waic"),model_name = "fit2",
                    overwrite = T, file = "fit2")

summary(fit2)
plot(fit2)
#Marginal posterior predictive checks
pp_check(fit2,nsamples = 500)
plot(conditional_effects(fit2), points = TRUE, spaghetti = T, nsamples = 500)

#####Fit 3_ theta = 2 /sigmoidal / prey dependent####
fit3<-brm(formula = bf(consumption_rate ~ a*Algae_density^2/(1+a*h*Algae_density^2), 
                       a ~ 1, h ~ 1, nl = TRUE),
          data = data, family = gaussian(),
          prior = c(set_prior("normal(0, 1)", nlpar = "a", lb = 0),
                    set_prior("normal(0, 1)", nlpar = "h", lb = 0)),
          
          chains = 3,
          iter = 3000,
          warmup = 500,
          sample_prior = TRUE,
          cores = getOption("mc.cores",1),
          seed = 7777,
          control = list(adapt_delta = 0.99, max_treedepth = 15))
# file = "fit3")
fit3= add_criterion(fit3,c("loo","waic"),model_name = "fit3",
                    overwrite = T, file = "fit3")


summary(fit3)
plot(fit3)
#Marginal posterior predictive checks
pp_check(fit3,nsamples = 500)
plot(conditional_effects(fit3), points = TRUE)

###FitR1_ratio dependent models ######
#linear ratio dependent
fitR1<-brm(formula = bf(consumption_rate ~ a*(Algae_density/Fish_density), 
                        a ~ 1, nl = TRUE),
           data = data, family = gaussian(),
           prior = c(set_prior("normal(0, 1)", nlpar = "a", lb = 0)),
           
           chains = 3,
           iter = 3000,
           warmup = 500,
           sample_prior = TRUE,
           cores = getOption("mc.cores",1),
           seed = 7777,
           control = list(adapt_delta = 0.99, max_treedepth = 15))

fitR1= add_criterion(fitR1,c("loo","waic"),model_name = "fitR1",
                     overwrite = T, file = "fitR1")


summary(fitR1)
plot(fitR1)

#Marginal posterior predictive checks
pp_check(fitR1,nsamples = 500)
plot(conditional_effects(fitR1), points = TRUE)

#### Fit R2_theta = 1/hyperbolic /ratio dependent #####
fitR2<-brm(formula = bf(consumption_rate ~ a*(Algae_density/Fish_density)/(1+a*h*(Algae_density/Fish_density)), 
                        a ~ 1, h ~ 1, nl = TRUE),
           data = data, family = gaussian(),
           prior = c(set_prior("normal(0, 1)", nlpar = "a", lb = 0),
                     set_prior("normal(0, 1)", nlpar = "h", lb = 0)),
           
           chains = 3,
           iter = 3000,
           warmup = 500,
           sample_prior = TRUE,
           cores = getOption("mc.cores",1),
           seed = 7777,
           control = list(adapt_delta = 0.99, max_treedepth = 15))

fitR2= add_criterion(fitR2,c("loo","waic"),model_name = "fitR2",
                     overwrite = T, file = "fitR2")

summary(fitR2)
plot(fitR2)
#Marginal posterior predictive checks
pp_check(fitR2,nsamples = 500)
plot(conditional_effects(fitR2), points = TRUE)

##### FitR3_theta = 2 /sigmoidal /ratio dependent#####
fitR3 <- brm(formula = bf(consumption_rate ~ a*(Algae_density/Fish_density)^2/(1+a*h*(Algae_density/Fish_density)^2), 
                          a ~ 1, h ~ 1, nl = TRUE),
             data = data, family = gaussian(),
             prior = c(set_prior("normal(0, 1)", nlpar = "a", lb = 0),
                       set_prior("normal(0, 1)", nlpar = "h", lb = 0)),
             
             chains = 3,
             iter = 3000,
             warmup = 500,
             sample_prior = TRUE,
             cores = getOption("mc.cores",1),
             seed = 7777,
             control = list(adapt_delta = 0.99, max_treedepth = 15))

fitR3= add_criterion(fitR3,c("loo","waic"),model_name = "fitR3",
                     overwrite = T, file = "fitR3")


summary(fitR3)
plot(fitR3)
#Marginal posterior predictive checks
pp_check(fitR3,nsamples = 500)
plot(conditional_effects(fitR3), points = TRUE)


### Beddington type 1  functional response: #####

fit_bed_1 <-brm(formula = bf(consumption_rate ~ a * Algae_density / (1 + c * Fish_density), 
                             c ~ 1, a ~ 1 , nl = TRUE),
                data = data, family = gaussian(),
                prior = c(set_prior("normal(0, 1)", nlpar = "c", lb = 0),
                          set_prior("normal(0, 1)", nlpar = "a", lb = 0)),
                chains = 3,
                iter = 3000,
                warmup = 500,
                sample_prior = TRUE,
                cores = getOption("mc.cores",1),
                seed = 7777,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                file = "fit_bed_1")

fit_bed_1 = add_criterion(fit_bed_1,c("loo","waic"),model_name = "fit_bed_1",
                          overwrite = T, file = "fit_bed_1")


###  Beddington type 2  functional response: #####

fit_bed_2 <-brm(formula = bf(consumption_rate ~ a * Algae_density * Fish_density / (1 + a * h * Algae_density + c * Fish_density), 
                             c ~ 1, a ~ 1 , h ~ 1,  nl = TRUE),
                data = data, family = gaussian(),
                prior = c(set_prior("normal(0, 1)", nlpar = "c", lb = 0),
                          set_prior("normal(0, 1)", nlpar = "h", lb = 0),
                          set_prior("normal(0, 1)", nlpar = "a", lb = 0)),
                chains = 3,
                iter = 3000,
                warmup = 500,
                sample_prior = TRUE,
                cores = getOption("mc.cores",1),
                seed = 7777,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                file = "fit_bed_2")

fit_bed_2 = add_criterion(fit_bed_2, c("loo","waic"),model_name = "fit_bed_2",
                          overwrite = T, file = "fit_bed_2")

### Ivlev functional response: #####

fitIVLEV <-brm(formula = bf(consumption_rate ~ c*(1-exp(-d*Algae_density)), 
                            c ~ 1, d ~ 1 , nl = TRUE),
               data = data, family = gaussian(),
               prior = c(set_prior("normal(0, 1)", nlpar = "c", lb = 0),
                         set_prior("normal(0, 1)", nlpar = "d", lb = 0)),
               chains = 3,
               iter = 3000,
               warmup = 500,
               sample_prior = TRUE,
               cores = getOption("mc.cores",1),
               seed = 7777,
               control = list(adapt_delta = 0.99, max_treedepth = 15), file = "fitIVLEV")

fitIVLEV= add_criterion(fitIVLEV,c("loo","waic"),model_name = "fitIVLEV",
                        overwrite = T, file = "fitIVLEV")


### LOOIC ####
loo=loo_compare(fit0, 
                 fit1,
                 fit2,
                 fit3,
                fitP1_prova,
                fitP2_prova,
                fitP3_prova,
                criterion = "loo")
loo=as.data.frame(print(loo,simplify=F))
loo$model=row.names(loo)
loo = loo %>%
  mutate_if(is.numeric, round,digits = 2)
#### LOOIC weights: they are calculated by taking averages of log- likelihood over the posterior distribution.
#And they are also just an estimate of out-of-sample deviance. 
l= as_tibble(model_weights(fit0, 
                           fit1,
                           fit2,
                           fit3,
                           fitP1_prova,
                           fitP2_prova,
                           fitP3_prova,
                           weights = "loo", method = "stacking")) %>% #'The stacking method (method="stacking"), which is the default 
  # 'for loo_model_weights(), combines all models by maximizing the leave-one-out 
  #' predictive density of the combination distribution. 
  #' That is, it finds the optimal linear combining weights for maximizing the leave-one-out log score.
  rename(weight = value) %>% 
  mutate(model  = c("fit0",
                    "fit1",
                    "fit2",
                    "fit3",
                    "fitP1_prova",
                    "fitP2_prova",
                    "fitP3_prova"), weight = weight %>%
           round(digits = 2)) %>% 
  select(model, weight) %>% 
  arrange(desc(weight))

table_LOO=full_join(x = loo,y = l, by=NULL)
table_LOO= table_LOO %>%
  select(model, looic, se_looic ,weight, elpd_diff,se_diff, elpd_loo,  
         se_elpd_loo ,p_loo,se_p_loo)

View(table_LOO)



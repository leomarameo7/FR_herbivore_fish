### Load packages####
library(tidyverse) #Manipulate database
library(brms) # Bayesian Regression Models using 'Stan'
library(performance)
####  function to export bayesian R2 into data frames #####
#' @export
as.data.frame.r2_bayes <- function(x, ...) {
  out <- data.frame(
    R2 = x$R2_Bayes,
    SD = attributes(x)$SE$R2_Bayes,
    CI = attributes(x)$CI$R2_Bayes$CI,
    CI_low = attributes(x)$CI$R2_Bayes$CI_low,
    CI_high = attributes(x)$CI$R2_Bayes$CI_high,
    stringsAsFactors = FALSE
  )
  
  
  if (!is.null(x$R2_Bayes_marginal)) {
    out_marginal <- data.frame(
      R2 = x$R2_Bayes_marginal,
      SD = attributes(x)$SE$R2_Bayes_marginal,
      CI = attributes(x)$CI$R2_Bayes_marginal$CI,
      CI_low = attributes(x)$CI$R2_Bayes_marginal$CI_low,
      CI_high = attributes(x)$CI$R2_Bayes_marginal$CI_high,
      stringsAsFactors = FALSE
    )
    
    
    out$Component <- "conditional"
    out_marginal$Component <- "marginal"
    out <- rbind(out, out_marginal)
  }
  
  
  out
}
#### COMPARE MODELS ####
#### Reload models with the readRDS() function####
fit_00 <- readRDS("outputs_results/models/fit_with_fish_density/fit_00.rds")
fit_H1 <- readRDS("outputs_results/models/fit_with_fish_density/fit_H1.rds")
fit_H2 <- readRDS("outputs_results/models/fit_with_fish_density/fit_H2.rds")
fit_R1 <- readRDS("outputs_results/models/fit_with_fish_density/fit_R1.rds")
fit_R2 <- readRDS("outputs_results/models/fit_with_fish_density/fit_R2.rds")
fit_P1 <- readRDS("outputs_results/models/fit_with_fish_density/fit_P1.rds")
fit_P2 <- readRDS("outputs_results/models/fit_with_fish_density/fit_P2.rds")
fit_IV <- readRDS("outputs_results/models/fit_with_fish_density/fit_IV.rds")
fit_IR <- readRDS("outputs_results/models/fit_with_fish_density/fit_IR.rds")
#### Bayesian R2 #####
#### Bayesian R2 #####
zero_d <- as.data.frame.r2_bayes(r2_bayes(fit_00,ci = 0.95)) %>% 
  mutate(model= "Null model" ) %>% 
  mutate(Predictor= "Fish density")

h1_d <- as.data.frame.r2_bayes(r2_bayes(fit_H1,ci = 0.95)) %>% 
  mutate(model= "H1" ) %>% 
  mutate(Predictor= "Fish density")

h2_d <- as.data.frame.r2_bayes(r2_bayes(fit_H2,ci = 0.95)) %>% 
  mutate(model= "H2" ) %>% 
  mutate(Predictor= "Fish density")

r1_d <- as.data.frame.r2_bayes(r2_bayes(fit_R1,ci = 0.95)) %>% 
  mutate(model= "R1" ) %>% 
  mutate(Predictor= "Fish density")

r2_d <- as.data.frame.r2_bayes(r2_bayes(fit_R2,ci = 0.95)) %>% 
  mutate(model= "R2" ) %>% 
  mutate(Predictor= "Fish density")

p2_d <- as.data.frame.r2_bayes(r2_bayes(fit_P2,ci = 0.95)) %>% 
  mutate(model= "P2" ) %>% 
  mutate(Predictor= "Fish density")

p1_d <- as.data.frame.r2_bayes(r2_bayes(fit_P1,ci = 0.95)) %>% 
  mutate(model= "P1" ) %>% 
  mutate(Predictor= "Fish density")

iv_d <- as.data.frame.r2_bayes(r2_bayes(fit_IV,ci = 0.95)) %>% 
  mutate(model= "IV" ) %>% 
  mutate(Predictor= "Fish density")

ivr_d <- as.data.frame.r2_bayes(r2_bayes(fit_IR,ci = 0.95)) %>% 
  mutate(model= "IVR" ) %>% 
  mutate(Predictor= "Fish density")

#### Estimate Bayesian R2 for Functional Response with biomass density as predictor  ####
#### Reload models with the readRDS() function####
fit_00 <- readRDS("outputs_results/models/fit_with_fish_biomass/fit_00.rds")
fit_H1 <- readRDS("outputs_results/models/fit_with_fish_biomass/fit_H1.rds")
fit_H2 <- readRDS("outputs_results/models/fit_with_fish_biomass/fit_H2.rds")
fit_R1 <- readRDS("outputs_results/models/fit_with_fish_biomass/fit_R1.rds")
fit_R2 <- readRDS("outputs_results/models/fit_with_fish_biomass/fit_R2.rds")
fit_P1 <- readRDS("outputs_results/models/fit_with_fish_biomass/fit_P1.rds")
fit_P2 <- readRDS("outputs_results/models/fit_with_fish_biomass/fit_P2.rds")
fit_IV <- readRDS("outputs_results/models/fit_with_fish_biomass/fit_IV.rds")
fit_IR <- readRDS("outputs_results/models/fit_with_fish_biomass/fit_IR.rds")

#### Bayesian R2 #####
zero <- as.data.frame.r2_bayes(r2_bayes(fit_00,ci = 0.95)) %>% 
  mutate(model= "Null model" ) %>% 
  mutate(Predictor= "Fish biomass")

h1<- as.data.frame.r2_bayes(r2_bayes(fit_H1,ci = 0.95)) %>% 
  mutate(model= "H1" ) %>% 
  mutate(Predictor= "Fish biomass")
h2<- as.data.frame.r2_bayes(r2_bayes(fit_H2,ci = 0.95)) %>% 
  mutate(model= "H2" ) %>% 
  mutate(Predictor= "Fish biomass")

r1 <- as.data.frame.r2_bayes(r2_bayes(fit_R1,ci = 0.95)) %>% 
      mutate(model= "R1" ) %>% 
  mutate(Predictor= "Fish biomass")

r2 <- as.data.frame.r2_bayes(r2_bayes(fit_R2,ci = 0.95)) %>% 
  mutate(model= "R2" ) %>% 
  mutate(Predictor= "Fish biomass")

p2 <- as.data.frame.r2_bayes(r2_bayes(fit_P2,ci = 0.95)) %>% 
  mutate(model= "P2" ) %>% 
  mutate(Predictor= "Fish biomass")
p1 <- as.data.frame.r2_bayes(r2_bayes(fit_P1,ci = 0.95)) %>% 
  mutate(model= "P1" ) %>% 
  mutate(Predictor= "Fish biomass")

iv <- as.data.frame.r2_bayes(r2_bayes(fit_IV,ci = 0.95)) %>% 
  mutate(model= "IV" ) %>% 
  mutate(Predictor= "Fish biomass")

ivr <- as.data.frame.r2_bayes(r2_bayes(fit_IR,ci = 0.95)) %>% 
  mutate(model= "IVR" ) %>% 
  mutate(Predictor= "Fish biomass")

###### Join R2 FOR ALL MODELS #####

R2_biomass <- rbind(zero_d, h1_d,h2_d,r1_d,r2_d,p1_d, p2_d,iv_d,ivr_d,
                    zero, h1, h2, r1 , r2, p1,p2, iv, ivr) %>% 
   select(-CI, -SD) %>% 
   mutate_if(is.numeric, round, digits=2) %>% 
  mutate(R2_new = paste0(R2," [", CI_low, "; ", CI_high, "]")) %>% 
  select(Predictor,model,R2_new) 

View(R2_biomass)

write.csv(R2_biomass, file  = "outputs_results/tables/Table_S3_r2_.csv", row.names = F)

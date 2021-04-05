library(tidyverse)
library(reshape2)
library(brms)
#### Reload models with the readRDS() function####
fit0 = readRDS("outputs_results/models/fit_00.rds")
fitH1 = readRDS("outputs_results/models/fit_H1.rds")
fitH2 = readRDS("outputs_results/models/fit_H2.rds")
fitR1 = readRDS("outputs_results/models/fit_R1.rds")
fitR2 = readRDS("outputs_results/models/fit_R2.rds")
fitP1 = readRDS("outputs_results/models/fit_P1.rds")
fitP2 = readRDS("outputs_results/models/fit_P2.rds")
fit_Ivlev = readRDS("outputs_results/models/fit_IV.rds")
fit_Ivlev_ratio = readRDS("outputs_results/models/fit_IVR.rds")# Ivlev ratio dependent model

#### Extract paramenters estimates and create a data frame for H1 model#####
null <- print(fit0)
estimate_null = as_data_frame(null[["fixed"]]) %>%
  rbind(null[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("α: constant maximum intake rate","Sequence", "Res. Stand. Dev."))%>% 
  add_column(Model = c("Null","Null","Null"))

#### Extract paramenters estimates and create a data frame for H1 model#####
H1 <- print(fitH1)
estimate_H1 = as_data_frame(H1[["fixed"]]) %>%
  rbind(H1[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","Sequence", "Res. Stand. Dev."))%>% 
  add_column(Model = c("H1","H1","H1"))

#### Extract paramenters estimates and create a data frame for H2 model#####
H2 <- print(fitH2)
estimate_H2 = as_data_frame(H2[["fixed"]]) %>%
  rbind(H2[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","Sequence", "h: handling time", "Res. Stand. Dev."))%>% 
  add_column(Model = c("H2","H2","H2","H2"))
#### Extract paramenters estimates and create a data frame for R1 model#####
R1 <- print(fitR1)
estimate_R1 = as_data_frame(R1[["fixed"]]) %>%
  rbind(R1[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","Sequence", "Res. Stand. Dev."))%>% 
  add_column(Model = c("R1","R1","R1"))

#### Extract paramenters estimates and create a data frame for R2 model#####
R2 <- print(fitR2)
estimate_R2 = as_data_frame(R2[["fixed"]]) %>%
  rbind(R2[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","Sequence", "h: handling time", "Res. Stand. Dev."))%>% 
  add_column(Model = c("R2","R2","R2","R2"))

#### Extract paramenters estimates and create a data frame for P1 model#####
P1 <- print(fitP1)
estimate_P1 = as_data_frame(P1[["fixed"]]) %>%
  rbind(P1[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","Sequence","m: mutual interference", "Res. Stand. Dev."))%>% 
  add_column(Model = c("P1","P1", "P1","P1"))

#### Extract paramenters estimates and create a data frame for P2 model#####
c <- print(fitP2)
estimate_P2 = as_data_frame(c[["fixed"]]) %>%
  rbind(c[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","Sequence","h: handling time","m: mutual interference", "Res. Stand. Dev."))%>% 
  add_column(Model = c("P2","P2","P2", "P2","P2"))
#### Extract paramenters estimates and create a data frame for Ivlev resource dependent model#####
Ivlev <- print(fit_Ivlev)
estimate_Ivlev = as_data_frame(Ivlev[["fixed"]]) %>%
  rbind(Ivlev[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: maximum feeding rate","Sequence","d: satiation coeff.", "Res. Stand. Dev."))%>% 
  add_column(Model = c("IV","IV","IV", "IV"))

#### Extract paramenters estimates and create a data frame for Ivlev ratio dependent model#####
Ivlev_ratio <- print(fit_Ivlev_ratio)
estimate_IVR = as_data_frame(Ivlev_ratio[["fixed"]]) %>%
  rbind(Ivlev_ratio[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: maximum feeding rate","Sequence","d: satiation coeff.", "Res. Stand. Dev."))%>% 
  add_column(Model = c("IVR","IVR","IVR", "IVR"))


### join the  data frames ####
parameters = rbind(
  estimate_null,
  estimate_H1,
                   estimate_H2,
                   estimate_R1,
                   estimate_R2,
                   estimate_P1,
                   estimate_P2,
                   estimate_Ivlev, 
                   estimate_IVR)%>% 
  #filter(Parameter != "Res. Stand. Dev.") %>% 
  rename(l_95_CI="l-95% CI") %>% 
  rename(u_95_CI="u-95% CI") %>% 
  mutate(Estimate = paste0(Post.Mean," [", l_95_CI, "; ", u_95_CI, "]")) %>% 
  select("Model","Parameter","Estimate", "Rhat", "ESS") 

write.csv(parameters, file  = "outputs_results/tables/Table_parameter_estimates.csv", row.names = F)


spread <- parameters %>% 
  reshape2::dcast(Model~Parameter) %>%  
  slice(1,2,3,4,7,8,5,6)
#### save table #####


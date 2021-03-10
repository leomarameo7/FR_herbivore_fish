library(tidyverse)
library(reshape2)
library(brms)
#### Reload models with the readRDS() function####
fit0 = readRDS("outputs_results/models/fit_00.rds")
fit_H1 = readRDS("outputs_results/models/fit_H1.rds")
fit_H2 = readRDS("outputs_results/models/fit_H2.rds")
fitR1 = readRDS("outputs_results/models/fit_R1.rds")
fitR2 = readRDS("outputs_results/models/fit_R2.rds")
fitP1 = readRDS("outputs_results/models/fit_P1.rds")
fitP2 = readRDS("outputs_results/models/fit_P2.rds")
fit_Ivlev = readRDS("outputs_results/models/fit_IV_RD.rds")
fit_BD = readRDS("outputs_results/models/fit_BD.rds")

#### Extract paramenters estimates and create a data frame for the NULL model#####
null =print(fit0)
estimate_null = as_data_frame(null[["fixed"]]) %>%
  rbind(null[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("c: maximum intake rate","Sequence", "Res. Stand. Dev."))%>% 
  add_column(Model = c("Null","Null","Null"))

#### Extract paramenters estimates and create a data frame for H1 model#####
H1 =print(fit_H1)
estimate_H1 = as_data_frame(H1[["fixed"]]) %>%
  rbind(H1[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","Sequence", "Res. Stand. Dev."))%>% 
  add_column(Model = c("H1","H1","H1"))

#### Extract paramenters estimates and create a data frame for H2 model#####
H2 =print(fit_H2)
estimate_H2 = as_data_frame(H2[["fixed"]]) %>%
  rbind(H2[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","Sequence","h: handling time", "Res. Stand. Dev."))%>% 
  add_column(Model = c("H2","H2","H2","H2"))

#### Extract paramenters estimates and create a data frame for R1 model#####
R2 =print(fitR2)
estimate_R2 = as_data_frame(R2[["fixed"]]) %>%
  rbind(R2[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","Sequence", "h: handling time", "Res. Stand. Dev."))%>% 
  add_column(Model = c("R2","R2","R2","R2"))

#### Extract paramenters estimates and create a data frame for R2 model#####
R1 =print(fitR1)
estimate_R1 = as_data_frame(R1[["fixed"]]) %>%
  rbind(R1[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","Sequence", "Res. Stand. Dev."))%>% 
  add_column(Model = c("R1","R1","R1"))

#### Extract paramenters estimates and create a data frame for P1 model#####
P1=print(fitP1)
estimate_P1 = as_data_frame(P1[["fixed"]]) %>%
  rbind(P1[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","Sequence","m: mutual interference", "Res. Stand. Dev."))%>% 
  add_column(Model = c("P1","P1", "P1","P1"))

#### Extract paramenters estimates and create a data frame for P2 model#####
c=print(fitP2)
estimate_P2 = as_data_frame(c[["fixed"]]) %>%
              rbind(c[["spec_pars"]]) %>%
              mutate_if(is.numeric, round, digits = 2) %>%
              select(-c(Tail_ESS, Est.Error)) %>% 
              rename(ESS = Bulk_ESS) %>% 
              rename(Post.Mean = Estimate) %>% 
              add_column(Parameter = c("a: attack rate","Sequence","h: handling time","m: mutual interference", "Res. Stand. Dev."))%>% 
              add_column(Model = c("P2","P2","P2", "P2","P2"))

#### Extract paramenters estimates and create a data frame for Ivlev model#####
Ivlev=print(fit_Ivlev)
estimate_Ivlev = as_data_frame(Ivlev[["fixed"]]) %>%
  rbind(Ivlev[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: maximum feeding rate","Sequence","d: satiation coeff.", "Res. Stand. Dev."))%>% 
  add_column(Model = c("Ivlev","Ivlev","Ivlev", "Ivlev"))
#### Extract paramenters estimates and create a data frame for BD model#####
BD = print(fit_BD)
estimate_BD = as_data_frame(BD[["fixed"]]) %>%
  rbind(BD[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: maximum feeding rate","Sequence","h: handling time","c: consumer encounter rate", "Res. Stand. Dev."))%>% 
  add_column(Model = c("BD","BD","BD", "BD","BD"))

### join the  data frames ####
parameters = rbind(estimate_null,
                   estimate_H1,
                   estimate_H2,
                   estimate_R1,
                   estimate_R2,
                   estimate_P1,
                   estimate_P2,
                   estimate_Ivlev,
                   estimate_BD) %>% 
  select("Model","Parameter","Post.Mean","l-95% CI","u-95% CI","Rhat","ESS")



#### save table #####
write.csv(parameters, file  = "outputs_results/tables/parameters_estimates.csv", row.names = F)




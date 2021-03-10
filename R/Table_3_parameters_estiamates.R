library(tidyverse)
library(reshape2)
library(brms)
#### Reload models with the readRDS() function####
fitR1 = readRDS("outputs_results/models/fit_R1.rds")
fitR2 = readRDS("outputs_results/models/fit_R2.rds")
fitP1 = readRDS("outputs_results/models/fit_P1.rds")
fitP2 = readRDS("outputs_results/models/fit_P2.rds")
fit_Ivlev = readRDS("outputs_results/models/fit_IV_RD.rds") # Ivlev ratio dependent model

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

#### Extract paramenters estimates and create a data frame for Ivlev ratio dependent model#####
Ivlev <- print(fit_Ivlev)
estimate_Ivlev = as_data_frame(Ivlev[["fixed"]]) %>%
  rbind(Ivlev[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: maximum feeding rate","Sequence","d: satiation coeff.", "Res. Stand. Dev."))%>% 
  add_column(Model = c("Ivlev","Ivlev","Ivlev", "Ivlev"))


### join the  data frames ####
parameters = rbind(
                   estimate_R1,
                   estimate_R2,
                   estimate_P1,
                   estimate_P2,
                   estimate_Ivlev) %>% 
  filter(Parameter != "Res. Stand. Dev.") %>% 
  rename(l_95_CI="l-95% CI") %>% 
  rename(u_95_CI="u-95% CI") %>% 
  mutate(new_col = paste0(Post.Mean," [", l_95_CI, "; ", u_95_CI, "]")) %>% 
  select("Model","Parameter","new_col") 

spread <- parameters %>% 
  reshape2::dcast(Model~Parameter) %>% 
  slice(4,5,2,3,1)
#### save table #####
write.csv(spread, file  = "outputs_results/tables/Table_3.csv", row.names = F)

library(tidyverse)
library(reshape2)
library(brms)
#### Reload models with the readRDS() function####
fit0 = readRDS("fit0.rds")
fitP1= readRDS("fitP1.rds")
fitP2= readRDS("fitP2.rds")
fitP3= readRDS("fitP3.rds")
#### Extract paramenters estimates and create a data frame for P2 model#####
c=print(fitP2)
estimate_P2 = as_data_frame(c[["fixed"]]) %>%
              rbind(c[["spec_pars"]]) %>%
              mutate_if(is.numeric, round, digits = 3) %>%
              select(-c(Tail_ESS, Est.Error)) %>% 
              rename(ESS = Bulk_ESS) %>% 
              rename(Post.Mean = Estimate) %>% 
              add_column(Parameter = c("a: attack rate","h: handling time","m: mutual interference", "Res. Stand. Dev."))%>% 
              add_column(Model = c("P2: hyperbolic","P2: hyperbolic","P2: hyperbolic", "P2: hyperbolic"))

#### Extract paramenters estimates and create a data frame for P3 model#####
P1=print(fitP1)
estimate_P1 = as_data_frame(P1[["fixed"]]) %>%
  rbind(P1[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","m: mutual interference", "Res. Stand. Dev."))%>% 
  add_column(Model = c("P1: linear","P1: linear", "P1: linear"))

#### Extract paramenters estimates and create a data frame for P3 model#####
P3=print(fitP3)
estimate_P3 = as_data_frame(P3[["fixed"]]) %>%
  rbind(P3[["spec_pars"]]) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(-c(Tail_ESS, Est.Error)) %>% 
  rename(ESS = Bulk_ESS) %>% 
  rename(Post.Mean = Estimate) %>% 
  add_column(Parameter = c("a: attack rate","h: handling time","m: mutual interference", "Res. Stand. Dev."))%>% 
  add_column(Model = c("P3: sigmoidal","P3: sigmoidal","P3: sigmoidal", "P3: sigmoidal"))

### join the threee data frame ####
parameters= rbind(estimate_P1,estimate_P2,estimate_P3)
####Factor to columns and columns to factor#####
#prova= melt(parameters, id.vars=6:7)
#prova2= dcast(prova, Model + Parameter ~ variable)
#### save table #####
write.csv(parameters, file  = "outputs_results/tables/parameters_estimate.csv", row.names = F)




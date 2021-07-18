#### Load packages ####
library(tidyverse)
library(readr)
library(dplyr)
library(plyr)
#### load data ####
data_c <- read.csv("data/raw/data_raw.csv")

##### Summarize data per trial (mean between minutes) ######
data_s = data_c %>% group_by(Day,ID_experiment,Sequence,Sequence_order) %>%
  summarise(N = mean(N), 
            P = mean(P), 
            se = sd(C)/sqrt(length(C)),
            sd = sd(C),
            q_2.5 = quantile(C,prob=.025),
            q_97.5 = quantile(C,prob=.975),
            q_25= quantile(C,prob=.25), 
            q_75= quantile(C,prob=.75), 
            C = mean(C),
            C_median = median(C))

##### Normality check ok #####
hist(data_s$C)
shapiro.test(data_s$C)

write.csv(data_s, file="data/processed/data_fitting_models.csv", row.names=F)

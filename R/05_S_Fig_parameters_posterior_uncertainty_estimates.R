library(bayesplot)
library(tidybayes)
library(tidyverse)
library(ggplot2)
library(brms)
library(dplyr)
library(reshape2)
#### Reload models with the readRDS() function####
fitR1 = readRDS("outputs_results/models/fit_R1.rds")
fitP1 = readRDS("outputs_results/models/fit_P1.rds")
fitP2 = readRDS("outputs_results/models/fit_P2.rds")
#### Extracting the posterior draws ####
# Here we’ll do so with the brms::posterior_samples() function.
posts <-
  tibble(model = c("fitP1", "fitP2","fitR1")) %>%
  mutate(fit   = map(model, get)) %>% 
  mutate(post  = map(fit, posterior_samples))

head(posts)

#'The first column, name is just a character vector with the names of the fits. 
#'It’s in the map() functions within the two mutate()lines where all the magic happens. 
#'With the first, we used the get() function to snatch up the brms fit objects matching the names 
#'in the name column. In the second, we used the posterior_samples() function to extract the posterior draws 
#'from each of the fits saved in fit. Do you see how each for in the post column contains an entire 
#'20,000 ×3 or 4 data frame ?
#'That’s why we refer to this as a nested tibble. 
#'We have data frames compressed within data frames. 
#'If you’d like to access the data within the post column, just unnest().
p = posts %>% 
  unnest(post) %>% 
  select(-c(sigma, lp__,fit)) %>% 
  melt(id.vars=c("model"), value.name = "value") %>% 
  mutate(variable = recode(variable, b_a_Intercept = "a: attack rate" )) %>% 
  mutate(variable = recode(variable, b_m_Intercept = "m: mutual interference")) %>%
  mutate(variable = recode(variable, b_h_Intercept = "h: handling time")) %>%
  rename(parameter = variable)%>%
  mutate(model = recode(model ,fitP1 = "P1",fitP2 = "P2", fitR1 = "R1"))
  
head(p)  

### plots###
# Set theme
personal_theme = theme_set(theme_bw() +
                theme(text = element_text(family = "sans"),
                panel.grid   = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line = element_line(size = 0.75),
                axis.title.x = element_text(color = "black", size = 16),
                axis.title.y = element_text(color = "black", size = 16),
                axis.text.x = element_text(color = "black", size = 14),
                axis.text.y  = element_text(color = "black", size = 14, hjust = 0),
                plot.title = element_text(hjust = 0.5, size = 14),
                panel.background = element_blank()))
# parameter and model as factors
p$parameter=as.factor(p$parameter)
p$model=as.factor(p$model)
# data frame for m parameter , drop NA values for models that does not have the parameter in their formula
p_mutual_inter <- p %>% 
  filter(parameter == 'm: mutual interference')%>%
  drop_na()
#summary of the p_mutual_inter data frame
summary(p_mutual_inter$value)
#### Plot m: mutual interference uncertanty estimates ####
fun_mean <- function(x){
  return(data.frame(y=mean(x),label = round(mean(x,na.rm=T),2)))
  }
m = ggplot(data=p_mutual_inter, aes(x = value, y = model)) +
     geom_halfeyeh(point_interval= mean_qi,.width = c(0.66, 0.95)) +
  stat_summary(fun.data = fun_mean, geom="text", vjust =-0.8)+
     labs(x = "Parameter estimate",
         y = NULL,
         title = "m: mutual interference") +
      scale_x_continuous(limits = c(0.6, 2.6),
                      breaks = seq(0.6, 2.6,0.2))+
   personal_theme
m

# data frame for a: attack rate parameter , drop NA values for models that does not have the parameter in their formula
a_mutual_inter <- p %>% 
    filter(parameter == 'a: attack rate')%>%
    drop_na() # remove outliners
 #summary of the a_mutual_inter data frame
 summary(a_mutual_inter$value)
#### Plot a: attack rate uncertanty estimates ####
a =  ggplot(data=a_mutual_inter, aes(x = value, y = model)) +
    geom_halfeyeh(point_interval= mean_qi,.width = c(0.66, 0.95))+
   stat_summary(fun.data = fun_mean, geom="text", vjust =-0.8)+
  personal_theme +
    labs(x = NULL,
         y = NULL,
         title = "a: attack rate") +
    scale_x_continuous(limits = c(0, 0.7),breaks = seq(0,0.7,0.1))
a

# data frame for h: handling time parameter , drop NA values for models that does not have the parameter in their formula
h_mutual_inter<- p %>% 
    filter(parameter == 'h: handling time')%>%
    drop_na()
#summary of the a_mutual_inter data frame
summary(h_mutual_inter$value)
#### Plot h: handling time uncertanty estimates ####
h = ggplot(data=h_mutual_inter, aes(x = value, y = model)) +
    geom_halfeyeh(point_interval= mean_qi,.width = c(0.66, 0.95))+
  stat_summary(fun.data = fun_mean, geom="text", vjust =-0.8)+
  personal_theme +
    labs(x = NULL,
         y = NULL,
         title = "h: handling time")+
    scale_x_continuous(limits = c(0.05, 3.2),breaks = seq(0.05,3.2,0.31))  
h
  
### Multi plot####
plot = gridExtra::grid.arrange(a,
                          h,
                          m, ncol=1)  
plot
### Saving multiplot####
#Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "Figure_2.pdf",
       plot=plot, 
       device="pdf",
       path ="outputs_results/figures/",
       dpi = 400)





##### other alternative plots #####

posteriorP1 <- as.array(fitP1)
posteriorP2<- as.array(fitP2)
posteriorR1 = as.array(fitR1)
  

 P1 = mcmc_dens(
  posteriorP1, 
  pars = c("b_a_Intercept","b_m_Intercept"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean") + 
  ggplot2::labs(
    title = "P1 posterior distributions",
    subtitle = "with medians and 80% intervals"
  )
 
 
 P2 = mcmc_dens(
   posteriorP2, 
   pars = c("b_a_Intercept","b_m_Intercept","b_h_Intercept"),
   prob = 0.8, # 80% intervals
   prob_outer = 0.99, # 99%
   point_est = "mean") + 
   ggplot2::labs(
     title = "P2 posterior distributions",
     subtitle = "with medians and 80% intervals"
)
 
 R1 = mcmc_dens(
   posteriorR1, 
   pars = c("b_a_Intercept","b_m_Intercept", "b_h_Intercept"),
   prob = 0.8, # 80% intervals
   prob_outer = 0.99, # 99%
   point_est = "mean") + 
   ggplot2::labs(
     title = "R1 posterior distributions",
     subtitle = "with medians and 80% intervals"
   )

#####
#'To show the uncertainty intervals as shaded areas under the 
#'estimated posterior density curves we can use the mcmc_areas 
#'function.
P2= mcmc_areas(
  posteriorP2,
  pars = c("b_a_Intercept","b_m_Intercept"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)
 
 
 
 
 

library(bayesplot)
library(tidybayes)
library(tidyverse)
library(ggplot2)
library(brms)
library(dplyr)
library(reshape2)
#### Reload models with the readRDS() function####
fit0= readRDS("fit0.rds")
fitP1= readRDS("fitP1.rds")
fitP2= readRDS("fitP2.rds")
fitP3= readRDS("fitP3.rds")
#### Extracting the posterior draws ####
#Here we’ll do so with the brms::posterior_samples() function.
posts <-
  tibble(model = c("fit0","fitP1", "fitP2","fitP3")) %>%
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
  mutate(model = recode(model, fit0 = "Null" ,fitP1="P1: linear",fitP2="P2: hyperbolic", fitP3="P3: sigmoidal"))
  
head(p)  

### plots###
# Set theme
personal_theme = theme_set(theme_classic() +
                theme(text = element_text(family = "sans"),
                panel.grid   = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line = element_line(size = 0.75),
                axis.title.x = element_text(color = "black", size = 14),
                axis.title.y = element_text(color = "black", size = 14),
                axis.text.x = element_text(color = "black", size = 12),
                axis.text.y  = element_text(color = "black", size = 12, hjust = 0),
                plot.title = element_text(hjust = 0.5, size = 12),
                panel.background = element_blank()))
# parameter and model as factors
p$parameter=as.factor(p$parameter)
p$model=as.factor(p$model)
# data frame for m parameter , drop NA values for models that does not have the parameter in their formula
p_mutual_inter<- p %>% 
  filter(parameter == 'm: mutual interference')%>%
  drop_na()
#summary of the p_mutual_inter data frame
 # summary(p_mutual_inter$value)
#### Plot m: mutual interference uncertanty estimates ####
m = ggplot(data=p_mutual_inter, aes(x = value, y = model)) +
     geom_halfeyeh(point_interval= mean_qi,.width = c(0.66, 0.95)) +
      personal_theme +
     labs(x = "Parameter estimate",
         y = NULL,
         title = "m: mutual interference") +
      scale_x_continuous(limits = c(-3, 0.5),
                      breaks = seq(-3,.5,0.25))
m

# data frame for a: attack rate parameter , drop NA values for models that does not have the parameter in their formula
a_mutual_inter <- p %>% 
    filter(parameter == 'a: attack rate')%>%
    filter(model != "Null") %>%
    drop_na() # remove outliners
 #summary of the a_mutual_inter data frame
#  summary(a_mutual_inter$value)
#### Plot a: attack rate uncertanty estimates ####
a =  ggplot(data=a_mutual_inter, aes(x = value, y = model)) +
    geom_halfeyeh(point_interval= mean_qi,.width = c(0.66, 0.95))+
  personal_theme +
    labs(x = NULL,
         y = NULL,
         title = "a: attack rate") +
    scale_x_continuous(limits = c(0, 5.5),breaks = seq(0,5.5,0.5))
a

# data frame for h: handling time parameter , drop NA values for models that does not have the parameter in their formula
h_mutual_inter<- p %>% 
    filter(parameter == 'h: handling time')%>%
    drop_na()
#summary of the a_mutual_inter data frame
 # summary(h_mutual_inter$value)
#### Plot h: handling time uncertanty estimates ####
h = ggplot(data=h_mutual_inter, aes(x = value, y = model)) +
    geom_halfeyeh(point_interval= mean_qi,.width = c(0.66, 0.95))+
  personal_theme +
    labs(x = NULL,
         y = NULL,
         title = "h: handling time")+
    scale_x_continuous(limits = c(0, 4.25),breaks = seq(0,4.25,.5))  
h
  
### Multi plot####
plot = gridExtra::grid.arrange(a,
                          h,
                          m, ncol=1)  
plot
### Saving multiplot####
#Set new windo size and replot whatever plot you just made. 
dev.new(width = 10, height = 7.5, unit="in", noRStudioGD = T)
last_plot() 
#Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "Figure_3.png",
       plot=plot, 
       device="png",
       path ="outputs_results/figures/",
       width = dev.size()[1],
       height = dev.size()[2],
       dpi = 600)

dev.off() 





##### other alternative plots #####

posteriorP1 <- as.array(fitP1)
posteriorP2<- as.array(fitP2)
posteriorP3 = as.array(fitP3)
  

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
 
 P3 = mcmc_dens(
   posteriorP3, 
   pars = c("b_a_Intercept","b_m_Intercept", "b_h_Intercept"),
   prob = 0.8, # 80% intervals
   prob_outer = 0.99, # 99%
   point_est = "mean") + 
   ggplot2::labs(
     title = "P3 posterior distributions",
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
 
 
 
 
 

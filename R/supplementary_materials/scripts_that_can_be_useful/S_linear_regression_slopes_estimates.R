#### Load packages#######
library(rstan) 
library(brms)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(reshape2)
#### Load data fish biomass temporal dynamics----------
a <- read_csv("data/processed/data_cleaned.csv", col_types = cols())
#View(a)

# Limiting the number of decimals in Algae_density and Fish_density columns
is.num <- sapply(a$Algae_density, is.numeric)
a$Algae_density <- as.numeric(lapply(a$Algae_density[is.num], round, 2))

is.num <- sapply(a$Fish_density, is.numeric)
a$Fish_density <- as.numeric(lapply(a$Fish_density[is.num], round, 2))

#### Reload models with the readRDS() function####
model = readRDS("outputs_results/supplementary_materials/linear_model.rds")

##### set personal theme for plot ####

personal_theme= theme_classic()+
  theme(
    panel.grid.major = element_blank(),
    aspect.ratio = 0.75,
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size = 0.75),
    text = element_text(family = "sans"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.position="none")

#### Plot estimate linear regression slopes #####

posts <- tibble(modello = "model") %>%
  mutate(fit   = map(modello, get)) %>% 
  mutate(post  = map(fit, posterior_samples))

p = posts %>% 
  unnest(post) %>% 
select(1, 34:57) 


p <- melt(p) 

p <-  p %>% 
  mutate(variable = recode(variable,  
        "r_Algae_density:Fish_density[0.025_0.084,Minute]" = "Resource~density~~0.03~g~m^{-2} and Consumer~density~~0.08~ind.~m^{-2}" ))


### plots###
# variable  as factors
p$variable = as.factor(p$variable)

m = ggplot(data= p, aes(x = value, y = variable)) +
  # code, the dots are the posterior medians, the thick inner lines the 50% intervals, and the thinner outer lines the 95% intervals
  geom_halfeyeh(point_interval= mean_qi,.width = c(0.66, 0.95)) +
  personal_theme +
  labs(x = "Estimate slope of linear regression",
       y = NULL)+
  geom_vline(xintercept = 0.0, linetype = "dashed")+
scale_x_continuous(limits = c(-0.05, 0.05), breaks = seq(-0.05, 0.05 , 0.01))
m

### Saving plot####
#Set new windo size and replot whatever plot you just made. 
dev.new(width = 10, height = 7.5, unit="in", noRStudioGD = T)
last_plot() 
#Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "linear_regression_slopes.png",
       plot= m, 
       device="png",
       path ="outputs_results/supplementary_materials/",
       width = dev.size()[1],
       height = dev.size()[2],
       dpi = 400)

dev.off() 

#' Because we fit a hierarchical model with varying intercepts and slopes of time, 
#' we can also test the individual specific parameters. 
#' For example, we can look at every individual’s estimated slopes:
#' Retrived from : https://vuorre.netlify.app/post/2020/02/06/how-to-calculate-contrasts-from-a-fitted-brms-model/

x <- hypothesis(model, "Minute = 0", group = "Algae_density:Fish_density", scope = "coef")

#'In the above, we asked for the results of the hypothesis test, split by group Algae_density:Fish_density
#' (which is the grouping factor in our hierarchical model), and indicated coef as the scope. 
#' The latter means that the estimates are the subject-specific deviations with the fixed effect added, 
#' as opposed to ranef, which are zero-centered.

# Results of hypothesis() in a data.frame
data2 =x$hypothesis 


plot = ggplot(data = data2, 
              aes(x=Estimate, y=as.factor(Group))) +
  geom_errorbarh(aes(xmin = CI.Lower, xmax = CI.Upper)) +
  geom_point()+
  personal_theme +
  labs( x = "Estimate linear regression slope", y = "Trial(Algae density: Fish Density)")

plot

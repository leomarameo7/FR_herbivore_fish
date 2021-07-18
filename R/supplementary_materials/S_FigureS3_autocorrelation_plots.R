#### Load packages#######
library(rstan) 
library(brms)
library(psych) #to get some extended summary statistics
library(tidyverse)
library(tidybayes)
library(dplyr)
library(reshape2)
library(lemon)

# needed for data manipulation and plotting
##### Load data #####
data <- as_tibble(read_csv("data/raw/data_raw.csv"))

data = data %>% 
  mutate(trial = rep(c(1:20),each = 15)) 

#### Reload models with the readRDS() function####
model = readRDS("outputs_results/supplementary_materials/linear_model.rds")
###### Autocorrelation chart #####
# if the autocorrelation crosses the dashed blue line, 
# it means that specific lag is significantly correlated with current series
library(xts)
library(tidyquant)
library(stringr)

k <- 1:15
col_names <- paste0("lag_", k)

tidyverse_lags <- data %>%
  mutate( Time = lubridate::parse_date_time(Minute, 'M')) %>% 
  tq_mutate(
    select     = C,
    mutate_fun = lag.xts,
    k          = 1:15,
    col_rename = col_names
  )

tidyverse_count_autocorrelations <- tidyverse_lags %>%
  gather(key = "lag", value = "lag_value", 
         -c("Day" , "trial" , "Minute", "N","P"  ,"C" , "fish_eating_minute",    
                                               "Sequence"  ,"initial_weight" ,               
                                              "control_before","control_after"  ,"Time"
          )
         ) %>%
  mutate(lag = str_sub(lag, start = 5) %>% as.numeric) %>%
  group_by(N, P, lag) %>%
  summarize(
    cor = cor(x = C, y = lag_value, use = "pairwise.complete.obs"),
    cutoff_upper = 2/(n())^0.5,
    cutoff_lower = -2/(n())^0.5
  )

# Trial as factor 
tidyverse_count_autocorrelations$N = as.factor(tidyverse_count_autocorrelations$N)
tidyverse_count_autocorrelations$P = as.factor(tidyverse_count_autocorrelations$P)


##### Rename factors #####


tidyverse_count_autocorrelations$N2 <- factor(tidyverse_count_autocorrelations$N, labels = c("Resource~density~~0.03~g~m^{-2}", "Resource~density~~0.05~g~m^{-2}",
                                                       "Resource~density~~0.08~g~m^{-2}", "Resource~density~~0.10~g~m^{-2}",
                                                       "Resource~density~~0.13~g~m^{-2}" ))

tidyverse_count_autocorrelations$P2 <- factor(tidyverse_count_autocorrelations$P, labels = c("Consumer~density~~0.08~ind.~m^{-2}", "Consumer~density~~0.11~ind.~m^{-2}",
                                                     "Consumer~density~~0.13~ind.~m^{-2}", "Consumer~density~~0.15~ind.~m^{-2}"))

#### Plot autocorrelation #####
p = tidyverse_count_autocorrelations %>%
   ggplot( aes( x = lag, y = cor)) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  # Plot autocorrelations
  geom_point(size = 1) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  # Add cutoffs
  geom_line(aes(y = cutoff_upper), color = "blue", linetype = 2) +
  geom_line(aes(y = cutoff_lower), color = "blue", linetype = 2) +
  # Add facets
  facet_wrap(~N2+P2, scales = "free", 
             labeller=labeller(N2=label_parsed, P2=label_parsed, .multi_line=T)) +
  
  #coord_capped_cart(bottom='both', left='both') +
  # Aesthetics
  expand_limits(y = c(-1, 1)) +
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15)) +
  labs(
    y = "Pearson correlation coefficient",
    x = "Lags"
  ) +

  theme(panel.border=element_blank(), 
        axis.line=element_line(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family = "sans"),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
       # axis.text.x = element_text(size = 10),
        #axis.text.y  = element_text(size = 10),
    #strip.background = element_blank(),
    #strip.text.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1))

p


### Saving plot####
#Set new windo size and replot whatever plot you just made. 
dev.new(width = 10, height = 7.5, unit="in", noRStudioGD = T)
last_plot() 
#Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "Pearson_autocorrelation.pdf",
       plot= p, 
       device="pdf",
       path ="outputs_results/supplementary_materials/",
       width = dev.size()[1],
       height = dev.size()[2],
       dpi = 600)

dev.off() 


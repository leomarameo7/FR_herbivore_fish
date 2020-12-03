####Load packages---------------------------
library(ggplot2)
library(readr)
library(tidyverse)
library(lemon)
#### Load data fish biomass temporal dynamics----------
a <- read_csv("data/processed/data_cleaned.csv", col_types = cols())
#View(a)

# Limiting the number of decimals in Algae_density and Fish_density columns
is.num <- sapply(a$Algae_density, is.numeric)
a$Algae_density <- as.numeric(lapply(a$Algae_density[is.num], round, 2))

is.num <- sapply(a$Fish_density, is.numeric)
a$Fish_density <- as.numeric(lapply(a$Fish_density[is.num], round, 2))

# as factors

a$Algae_density =as_factor(a$Algae_density)
a$Fish_density = as_factor(a$Fish_density)


##### Rename factors #####


a$Algae_density2 <- factor(a$Algae_density, labels = c("Resource~density~~0.03~g~m^{-2}", "Resource~density~~0.05~g~m^{-2}",
                                                       "Resource~density~~0.08~g~m^{-2}", "Resource~density~~0.10~g~m^{-2}",
                                                       "Resource~density~~0.13~g~m^{-2}" ))

a$Fish_density2 <- factor(a$Fish_density, labels = c("Consumer~density~~0.08~ind.~m^{-2}", "Consumer~density~~0.11~ind.~m^{-2}",
                                                       "Consumer~density~~0.13~ind.~m^{-2}", "Consumer~density~~0.15~ind.~m^{-2}",
                                                        "Consumer~density~~0.18~ind.~m^{-2}" ))
#### Plot #####

p = ggplot(data = a, aes(x = Minute, y = consumption_rate )) + 
  geom_point()+
   geom_smooth(method = "lm") +
  facet_wrap(~Algae_density2+Fish_density2, scales = "free",
             labeller=labeller(Algae_density2=label_parsed, Fish_density2=label_parsed, .multi_line=T)) +
              #facet_rep_grid(Algae_density2  ~ Fish_density2,
              #labeller = label_bquote( rows = atop(Resource~density~" "~g~m^{-2}),cols = atop(Consumer~density~" "~ind.~m^{-2}))) +
              #labeller = labeller(.cols = label_parsed, .rows= label_parsed, .multi_line = T)
              #labeller = label_wrap_gen(width = 20, multi_line = TRUE)
                 
  coord_capped_cart(bottom='both', left='both') +
  theme_bw() + 
  theme(panel.border=element_blank(), 
        axis.line=element_line(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family = "sans")) +
        #strip.text.y = element_text(angle = 0, hjust = 0.5, vjust=-0.5)
        #strip.text.y = element_text(margin = margin(10, 5, 10, 5), hjust = 1)
 
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15)) +
  
  labs(x = "Time (minutes)", y = expression(Consumption~rate~" "~g~min^{-1}~ind^{-1}))

p

# Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and ####
#cannot possibly have a typo.

#Set new windo size and replot whatever plot you just made. 
dev.new(width = 10, height = 7.5, unit="in", noRStudioGD = T)
last_plot() 

ggsave(filename = "bites_plot.png",
       plot=p, 
       device="png",
       path ="outputs_results/supplementary_materials/",
       width = dev.size()[1],
       height = dev.size()[2],
       dpi = 400)

dev.off()



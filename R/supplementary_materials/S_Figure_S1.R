####Load packages---------------------------
library(ggplot2)
library(readr)
library(tidyverse)
library(lemon)
#### Load data fish biomass temporal dynamics----------
a <- read_csv("data/raw/data_raw.csv", col_types = cols())
#View(a)

# Limiting the number of decimals in Algae_density and Fish_density columns
is.num <- sapply(a$N, is.numeric)
a$N <- as.numeric(lapply(a$N[is.num], round, 2))

is.num <- sapply(a$P, is.numeric)
a$P <- as.numeric(lapply(a$P[is.num], round, 2))

# as factors

a$N =as_factor(a$N)
a$P = as_factor(a$P)


##### Rename factors #####


a$N_2 <- factor(a$N, labels = c("Resource~density~~0.03~g~m^{-2}", "Resource~density~~0.05~g~m^{-2}",
                                                       "Resource~density~~0.08~g~m^{-2}", "Resource~density~~0.10~g~m^{-2}",
                                                       "Resource~density~~0.13~g~m^{-2}" ))

a$P_2 <- factor(a$P, labels = c("Consumer~density~~0.08~ind.~m^{-2}", "Consumer~density~~0.11~ind.~m^{-2}",
                                                       "Consumer~density~~0.13~ind.~m^{-2}", "Consumer~density~~0.15~ind.~m^{-2}" ))
#### Plot #####

p = ggplot(data = a, aes(x = Minute, y = C )) + 
  geom_point()+
   geom_smooth(method = "lm") +
  facet_wrap(~N_2+P_2, scales = "free",
             labeller=labeller(N_2=label_parsed, P_2=label_parsed, .multi_line=T)) +
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
  
  labs(x = "Time (minutes)", y = expression(Per~capita~Consumption~rate~" "~g~"·"~min^{-1}~"·"~ind^{-1}))

p

# Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and ####
#cannot possibly have a typo.

#Set new windo size and replot whatever plot you just made. 
dev.new(width = 10, height = 7.5, unit="in", noRStudioGD = T)
last_plot() 

ggsave(filename = "bites_plot_supplementary_materials.pdf",
       plot = p, 
       path ="outputs_results/supplementary_materials/",
       width = dev.size()[1],
       height = dev.size()[2],
       dpi = 600)

dev.off()



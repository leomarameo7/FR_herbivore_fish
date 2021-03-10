### Load packages####
library(tidyverse)
library(ggplot2)
library(egg)
library(brms)
library(ggpubr)
#### Reload models with the readRDS() function####
fit0 = readRDS("outputs_results/models/fit_00.rds")
fitR1 = readRDS("outputs_results/models/fit_R1.rds")
fitP1 = readRDS("outputs_results/models/fit_P1.rds")
fitP2 = readRDS("outputs_results/models/fit_P2.rds")
### Read raw data ####
data <- readxl::read_excel('data/processed/data_cleaned.xlsx')

### define theme ggplot
th = theme_classic()+
  theme(
    panel.grid.major = element_blank(),
    aspect.ratio = 0.75,
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size = 0.75),
    text = element_text(family = "sans"),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    legend.key.size=unit(1,'lines'),
    legend.key.height=unit(1,"lines"),
    legend.text=element_text(size=14),
    legend.title = element_text(size=14),
    plot.title = element_text(hjust = 0.5),
    legend.position="top")

#Observed vs Predicted PLOTS for all models
#Predictions
p.0 <- as.data.frame(predict(fit0)) #Null
p.1 <- as.data.frame(predict(fitP1)) #Linear consumer-dependent
p.2 <- as.data.frame(predict(fitP2)) #Hyperbolic consumer-dependent
r.1 <- as.data.frame(predict(fitR1)) # Linear ratio dipendent 

p.exp <- data.frame(data, #the data frame that you used for the brms models 
                    p.0, p.1, p.2, r.1) #your predictions
    
# fish densisty as factor
# fish densisty as factor
p.exp$Fish_density= round(p.exp$Fish_density, digits=2)# round digits
p.exp$Fish_density=as.factor(p.exp$Fish_density)
######PLOTS######
plot_null <- ggplot(p.exp, aes(x = Cons_rate,
                               y = Estimate, 
                               shape = Fish_density)) + 
  geom_abline(aes(slope = 1, intercept = 0)) +
  geom_point(size=2, fill="black")+
  scale_shape_manual(values=c(15, 16, 17,18,3), 
                     name = expression(Consumer~density~" "~ind.~m^{-2}))+
  th + 
  scale_x_continuous(limits=c(0, 6.5), breaks=seq(0, 6.5,1))  + 
  scale_y_continuous(limits=c(0, 5.5), breaks=seq(0, 5.5,1))  + 
  xlab("") + 
  ylab("Predicted") +
  theme(legend.position = "none")+
  ggtitle("Null Model")

plot_null

##### R1 #####

plot_R1 <- ggplot(p.exp, aes(x = Cons_rate, y = Estimate.3, shape=Fish_density)) + 
  th + 
  geom_point(size=2, fill="black")+
  scale_shape_manual(values=c(15, 16, 17,18,3), 
                     name = expression(Consumer~density~" "~ind.~m^{-2}))+
  scale_x_continuous(limits=c(0, 6.5), breaks=seq(0, 6.5,1))  + 
  scale_y_continuous(limits=c(0, 5.5), breaks=seq(0, 5.5,1))  +  
  xlab("") + 
  ylab("Predicted") + 
  theme(legend.position = "none") +
  geom_abline(aes(slope = 1, intercept = 0)) + 
  ggtitle("R1: linear ratio_dependent")

plot_R1
##### P1######
plot_P1 <- ggplot(p.exp, aes(x = Cons_rate, y = Estimate.1, shape = Fish_density)) + 
  th + 
  geom_point(size=2, fill="black")+
  scale_shape_manual(values=c(15, 16, 17,18,3), 
                     name = expression(Consumer~density~" "~ind.~m^{-2}))+
  scale_x_continuous(limits=c(0, 6.5), breaks=seq(0, 6.5,1))  + 
  scale_y_continuous(limits=c(0, 5.5), breaks=seq(0, 5.5,1))  + 
  xlab("Observed") + 
  ylab("Predicted") + 
  theme(legend.position = "none") +
  geom_abline(aes(slope = 1, intercept = 0)) + 
  ggtitle("P1: consumer-dependent linear")
plot_P1
##### P2 #####

plot_P2 <- ggplot(p.exp, aes(x = Cons_rate, y = Estimate.2, shape=Fish_density)) + 
  th + 
  geom_point(size=2, fill="black")+
  scale_shape_manual(values=c(15, 16, 17,18,3), 
                     name = expression(Consumer~density~" "~ind.~m^{-2}))+
  scale_x_continuous(limits=c(0, 6.5), breaks=seq(0, 6.5,1))  + 
  scale_y_continuous(limits=c(0, 5.5), breaks=seq(0, 5.5,1))  + 
  xlab("Observed") + 
  ylab("Predicted") + 
  theme(legend.position = "none") +
  geom_abline(aes(slope = 1, intercept = 0)) + 
  ggtitle("P2: consumer-dependent hyperbolic")
plot_P2 


##### Multi plot #####
library(cowplot)
multi= ggarrange(plot_null, plot_R1, plot_P1,plot_P2, nrow = 2, ncol = 2)
multi 

# arrange the three plots in a single row
prow <- plot_grid( plot_null + theme(legend.position="none"),
                   plot_R1 + theme(legend.position="none"),
                   plot_P1 + theme(legend.position="none"),
                   plot_P2 + theme(legend.position="none"),
                   align = 'vh',
                   nrow = 2)

# extract the legend from one of the plots
# (clearly the whole thing only makes sense if all plots
# have the same legend, so we can arbitrarily pick one.)
legend_b <- get_legend( plot_null+ theme(legend.position="top"))

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
p <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .2))
p


#Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "predicted_vs_observed_1.pdf",
       plot = multi, 
       device="pdf",
       path ="outputs_results/supplementary_materials/",
       dpi = 300)



### Load packages####
library(tidyverse)
library(ggplot2)
library(egg)
library(brms)
library(ggpubr)
#### Reload models with the readRDS() function####
fit0= readRDS("fit0.rds")
fitP1= readRDS("fitP1.rds")
fitP2= readRDS("fitP2.rds")
fitP3= readRDS("fitP3.rds")
### Read raw data ####
data = read.csv("data/processed/data_cleaned.csv",header=T)

### define theme ggplot
th = theme_classic()+
  theme(
    panel.grid.major = element_blank(),
    aspect.ratio = 0.75,
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size = 0.75),
    text = element_text(family = "sans"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y  = element_text(size = 10),
    legend.key.size=unit(0.5,'lines'),
    legend.key.height=unit(1,"lines"),
    legend.text=element_text(size=10),
    legend.position="top")

#Observed vs Predicted PLOTS for all models
#Predictions
p.0 <- as.data.frame(predict(fit0)) #Null
p.1 <- as.data.frame(predict(fitP1)) #Linear
p.2 <- as.data.frame(predict(fitP2)) #Hyperbolic
p.3 <- as.data.frame(predict(fitP3)) #Sigmoidal

p.exp <- data.frame(data, #the data frame that you used for the brms models IMPORTANTE poner todo en el mismo data frame
                    p.0, p.1, p.2, p.3) #your predictions
    
# fish densisty as factor
# fish densisty as factor
p.exp$Fish_density= round(p.exp$Fish_density, digits=2)# round digits
p.exp$Fish_density=as.factor(p.exp$Fish_density)
######PLOTS######
plot_null <- ggplot(p.exp, aes(x = consumption_rate,
                               y = Estimate, 
                               shape = Fish_density)) + 
  geom_abline(aes(slope = 1, intercept = 0)) +
  geom_point(size=2, fill="black")+
  scale_shape_manual(values=c(15, 16, 17,18,3), 
                     name = expression(Consumer~density~" "~ind.~m^{-2}))+
  th + 
  xlim(0, 1.5) + 
  ylim(0, 0.5) + 
  xlab("Observed") + 
  ylab("Predicted") + 
  theme(legend.position = "right") +
  ggtitle("Null Model")

plot_null

##### P1######
plot_P1 <- ggplot(p.exp, aes(x = consumption_rate, y = Estimate.1, shape = Fish_density)) + 
  th + 
  geom_point(size=2, fill="black")+
  scale_shape_manual(values=c(15, 16, 17,18,3), 
                     name = expression(Consumer~density~" "~ind.~m^{-2}))+
  xlim(0, 1.5) + 
  ylim(0, 0.5) +
  xlab("Observed") + 
  ylab("Predicted") + 
  theme(legend.position = "none") +
  geom_abline(aes(slope = 1, intercept = 0)) + 
  ggtitle("P1: linear")
plot_P1
##### P2 #####

plot_P2 <- ggplot(p.exp, aes(x = consumption_rate, y = Estimate.2, shape=Fish_density)) + 
  th + 
  geom_point(size=2, fill="black")+
  scale_shape_manual(values=c(15, 16, 17,18,3), 
                     name = expression(Consumer~density~" "~ind.~m^{-2}))+
 # xlim(0, 0.04) +
 # ylim(0, 0.04) + 
  xlab("Observed") + 
  ylab("Predicted") + 
  theme(legend.position = "none") +
  geom_abline(aes(slope = 1, intercept = 0)) + 
  ggtitle("P2: hyperbolic")
plot_P2 

##### P3 #####

plot_P3 <- ggplot(p.exp, aes(x = consumption_rate, y = Estimate.3, shape=Fish_density)) + 
  th + 
  geom_point(size=2, fill="black")+
  scale_shape_manual(values=c(15, 16, 17,18,3), 
                     name = expression(Consumer~density~" "~ind.~m^{-2}))+
  #xlim(0, 0.04) +
  #ylim(0, 0.04) + 
  xlab("Observed") + 
  ylab("Predicted") + 
  theme(legend.position = "none") +
  geom_abline(aes(slope = 1, intercept = 0)) + 
  ggtitle("P3: sigmoidal")

plot_P3
##### Multi plot #####
multi= ggarrange(plot_null, plot_P1,plot_P2,plot_P3, nrow = 2, ncol = 2)
multi 
#Set new windo size and replot whatever plot you just made. 
dev.new(width = 10, height = 7.5, unit="in", noRStudioGD = T)
last_plot() 
#Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "predicted_vs_observed.png",
       plot=multi, 
       device="png",
       path ="outputs_results/supplementary_materials/",
       width = dev.size()[1],
       height = dev.size()[2],
       dpi = 600)
dev.off()



library(plotly)
library(magrittr)
library(modelr)
library(scatterplot3d)
library(tidybayes)
### Read cleaned data ####
data=read.csv("data/processed/data_cleaned.csv",header=T)


#### scatter plot 3d ####
# x axis: algae Algae density 
# y axis : Fish density 
# z axis: per capita consumption rate 
with(data, 
     scatterplot3d(Algae_density,
                   Fish_density, 
                   consumption_rate, 
                   xlab = expression(Algae~density~" "~g/m^{2}),
                   ylab = expression(Fish~density~" "~g/m^{2}),
                   zlab = expression(Consumption~rate~" "~g/min~"·"~ind),
                   pch = 16, color="black",
                   angle = 66,
                   type="h",
                   box=F,
                   grid = T))

# Aqui, Gabi, começam as tentativas .... o que eu pensei, é de pegar o meu modelo P2 , 
# extrair o que o modelo  prediz (draws predicted) , e com esses predicted, plotar a superficie. 
# Extrair os predicted draws ... acho que consegui
# porem tenho problema a extrair  e plotar essa superficie depois 

#### Reload models with the readRDS() function####
fitP2= readRDS("fitP2.rds")
# second best fit model 
fitP3= readRDS("fitP3.rds")

##### Extract predicted draws from  best fit model P2 #####
#' Where add_fitted_draws is analogous to brms::fitted.brmsfit 
#' (or brms::posterior_linpred), add_predicted_draws is analogous to brms::predict.brmsfit (brms::posterior_predict), 
#' giving draws from the posterior predictive distribution.
data_1 = data %>%
  mutate(Algae_density = round(Algae_density, digits=2) )%>% 
  mutate(Fish_density = round(Fish_density, digits=2)) %>% 
  group_by(Algae_density,Fish_density) %>%
  #' add_predicted_draws adds draws from posterior predictions to the data. 
  #' .prediction column containing draws from the posterior predictive distribution.
  #add_fitted_draws(fitP2, n = 5000) %>% 
  add_predicted_draws(fitP2, n = 3000) %>% 
  filter(.prediction > 0) 
#write.csv(data_1,"data/processed/data_1.csv", row.names = FALSE)

##### 3D plot with plotly####
x <- data_1$Algae_density
y <- data_1$Fish_density
z <- data_1$.value
#z <- data_1$.prediction # .value = predicted consumption rate ( acho que é isso ... mas devo conferir )

x_obs= data$Algae_density
y_obs=data$Fish_density
z_obs= data$consumption_rate

# plotar com o super pacote plot_ly

fig_4 <-  plot_ly(x = x, y = y, z= z, type = 'mesh3d', facecolor = "#FFFFFF") %>%
 add_markers(x = x_obs, y = y_obs, z = z_obs, marker = list(color = rgb(0, 0, 0), size = 5))

axx <- list( nticks = 10, range = c(0,0.13), 
             title = "Algae density",
             gridcolor = "black",
             family = "Arial, sans-serif")

axy <- list(nticks = 10, range = c(0.08,0.18), title = "Fish density" , gridcolor = "black",
            family = "Arial, sans-serif")

axz <- list( nticks = 10,range = c(0,1.65), title= "Consumption rate", gridcolor = "black",
             family = "Arial, sans-serif")

fig_4 <- fig_4 %>% 
          layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

fig_4


##### Add Animation Options####




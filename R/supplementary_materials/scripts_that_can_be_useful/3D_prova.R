
# From https://stackoverflow.com/questions/51414756/creating-surface-3d-plot-of-3-numeric-variables-in-r

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
  add_predicted_draws(fitP2, n = 9000) %>% 
  filter(.prediction > 0) 


data_grid <- data.frame(data_col = data_1$.prediction, 
                        axis_one= data_1$Algae_density, 
                        axis_two= data_1$Fish_density)

library(reshape2)


plot_matrix <- acast(data_grid, axis_one ~ axis_two, value.var="data_col", 
                     fun.aggregate = mean )
# plot_matrix = xtabs(data_col ~ axis_one + axis_two, data = data_grid)
plot_matrix

persp(x = as.numeric(colnames(plot_matrix)), 
      y = as.numeric(rownames(plot_matrix)), 
      z = plot_matrix,
      xlab = "Axis one",
      ylab = "Axis two",
      zlab = "Data",
      ticktype ='detailed',
      theta = 310, 
      phi = 20,
      col = "green", shade = 0.5)




library(plotly)
plot_ly(
  x = as.numeric(colnames(plot_matrix)), 
  y = as.numeric(rownames(plot_matrix)), 
  z = plot_matrix
) %>% 
  add_surface() %>%
  layout(
    title = "",
    scene = list(
      xaxis = list( title = "Algae density"),
      yaxis = list( title = "Fish density"),
      zaxis = list(title = "Consumption rate"),
      camera = list(eye = list(x = 1.95, y = -1.25, z = 1.25))
    ))




plot3D::persp3D(x = as.numeric(colnames(plot_matrix)), 
                y = as.numeric(rownames(plot_matrix)), 
                z = plot_matrix,
                xlab = "log Axis one",
                ylab = "log Axis two",
                zlab = "Data",
                ticktype ='detailed',
                theta = 310, 
                phi = 20)


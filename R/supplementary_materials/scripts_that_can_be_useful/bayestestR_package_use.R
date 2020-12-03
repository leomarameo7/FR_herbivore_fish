library(bayestestR)
library(see)

### Retrived from https://easystats.github.io/bayestestR/

c=describe_posterior(
  fitP1,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

describe_posterior(
  fitP2,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

summary(fitP2)


describe_posterior(
  fitP3,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)


centrality= point_estimate(fitP1)
plot(centrality)

####Uncertainty (CI)######
#'hdi() computes the Highest Density Interval (HDI) of a posterior distribution,
#' i.e., the interval which contains all points within the interval have a higher probability density 
#' than points outside the interval. The HDI can be used in the context of Bayesian posterior characterisation
#'  as Credible Interval (CI).
#'  Unlike equal-tailed intervals (see eti()) that typically exclude 2.5% from each tail of the distribution,
#'   the HDI is not equal-tailed and therefore always includes the mode(s) of posterior distributions.
#'   By default, hdi() returns the 89% intervals (ci = 0.89), deemed to be more stable than, 
#'   for instance, 95% intervals. An effective sample size of at least 10.000 is recommended if 
#'   95% intervals should be computed (Kruschke, 2015). Moreover, 89 indicates the arbitrariness of 
#'   interval limits - its only remarkable property is being the highest prime number that does not 
#'   exceed the already unstable 95% threshold (McElreath, 2018).

hdi=hdi(fitP1,ci=.89)
eti= eti(fitP1,ci=0.89)
plot(hdi)

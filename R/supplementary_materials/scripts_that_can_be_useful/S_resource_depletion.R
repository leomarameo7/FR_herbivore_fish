#'The code provided in this supplement is divided into four sections. 
#'Here, we deal only with the case in which predator functional responses can be described with a Holling’s Type II model (Holling, 1959).
#'Code is provided that helps to illustrate how depletion affects the shape of the predator’s functional response. 
#'Retrived from McCoy et al. 2012, Ecology Letters (https://onlinelibrary.wiley.com/doi/abs/10.1111/ele.12005)

##Load libraries
library(deSolve)
library(bbmle) 
library(ggplot2) 
library(emdbook)
## Rogers random predator equation: 

rogers.pred <- function(N0,a,h,m,P,T) {
  N0 - lambertW(a*h*N0*exp(-a*(T*(P^-m)-h*N0)))/(a*h)
  }

rogers.lsoda <- function(N10, a1, h1, c1, P1, T){
  L1 <- lsoda(y=c(N1=N10), times=seq(0, T, length=2),
              parms=c(a1=a1, h1=h1, c1=c1, P1=P1),
              func=frgrad)
  return(c(N10) - L1[2, -1])
}


##Holling's Type II Functional Response 
holling2.pred <- function(N0,a,h,P,m) {
a*(N0/P^m)/(1+a*h*(N0/P^m)) 
  }

### Aqui se pueden poner varios otros modelos se conseguimos ponerlos con las formas integradas, o estoy errado ? 
#Yo pondria solo el mejor que fue elegido 

#### plot: Illustration of depletion effect ####

curve(rogers.pred(x, a=0.165,h=79.31,m=0.276, P=0.08,T=1),from=0,to=0.15,
      ylab="Consumption rate",
      xlab="Resource density",ylim=c(0,0.05))

curve(rogers.pred(x, a=0.165,h=79.31,m=0.276, P=0.08, T=5)/5, add=TRUE,lty=2, from=0) 


curve(rogers.pred(x, a=0.165, h=79.31, m=0.276, P=0.08, T=15)/15, add=TRUE,lty=4, from=0) 

curve(holling2.pred(x, a=0.124, h=34.688, m=0.934, P=0.08), add=TRUE, lty=1, lwd=2, from=0)


legend(0.05,0.05, c(paste("FR integrated, T=",c(1,5,15),sep=""), "Holling instant."),
       lwd=c(rep(1,3),2),lty=c(1,2,4,1))
       
#'Illustration of depletion effects on the shape of the predators function response. 
#'As duration of the experiment, T, increases the apparent shape of the predators functional response becomes increasingly linear and
#' the deviation from the Holling Type II functional response model increases. 
#' Fitting the Holling Type II model to data for T>0, fitting without accounting for depletion will generate parameters estimates
#'  substantially different from the true predator functional response depicted by the bold solid line (the Holling type II curve).






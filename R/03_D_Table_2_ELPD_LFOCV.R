### Load packages####
library(readxl) #read excel sheets
library(tidyverse) #Manipulate database
library(brms) # Bayesian Regression Models using 'Stan'
library(loo)  # Efficient Leave-One-Out and Leave-Future-Out Cross-Validation 
              # for Bayesian Models

##### READ DATA AND SET CATEGORICAL VARIABLES #####
data <- read_excel('data/processed/data_cleaned.xlsx')
data$Sequence <- as.factor(data$Sequence) #order of the experiments during a day

#### COMPARE MODELS ####
#### Reload models with the readRDS() function####
fit_00 <- readRDS("outputs_results/models/fit_00.rds")
fit_H1 <- readRDS("outputs_results/models/fit_H1.rds")
fit_H2 <- readRDS("outputs_results/models/fit_H2.rds")
fit_R1 <- readRDS("outputs_results/models/fit_R1.rds")
fit_R2 <- readRDS("outputs_results/models/fit_R2.rds")
fit_P1 <- readRDS("outputs_results/models/fit_P1.rds")
fit_P2 <- readRDS("outputs_results/models/fit_P2.rds")
fit_IV <- readRDS("outputs_results/models/fit_IV.rds")
fit_IR <- readRDS("outputs_results/models/fit_IVR.rds")

# some helper functions we'll use throughout

# more stable than log(sum(exp(x))) 
log_sum_exp <- function(x) {
  max_x <- max(x)  
  max_x + log(sum(exp(x - max_x)))
}

# more stable than log(mean(exp(x)))
log_mean_exp <- function(x) {
  log_sum_exp(x) - log(length(x))
}

# compute log of raw importance ratios
# sums over observations *not* over posterior samples
sum_log_ratios <- function(loglik, ids = NULL) {
  if (!is.null(ids)) loglik <- loglik[, ids, drop = FALSE]
  rowSums(loglik)
}

# for printing comparisons later
rbind_print <- function(...) {
  round(rbind(...), digits = 2)
}

#Since our dataset has temporal structure we choose to compare the models using
#LEAVE FUTURE OUT CROSS VALIDATION (LFOCV), the methods were found here
#https://cran.r-project.org/web/packages/loo/vignettes/loo2-lfo.html
#from Paul B?rkner, Jonah Gabry and Aki Vehtari.

N <- 300 #Total observations in our dataset (20 trials x 15 min)
L <- 100 #Minimum N of observatios needed to make predictions (first 5 min of each trial)
M <- 100 #N of observations that to make predictions (last 5 min of each trial)
k_thres <- 0.7 #Pareto comparisons threshold to manage problematic observations

####NULL MODEL (non dependent of algae and fish densities)####

elpds_fit_00 <- rep(NA, N)
past <- 1:L
oos <- (L + 1):(L + M)
df_past <- data[past, , drop = FALSE]
df_oos <- data[c(past, oos), , drop = FALSE]
fit_past <- update(fit_00, newdata = df_past, recompile = FALSE)
loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
loglikm <- rowSums(loglik[, oos])
elpds_fit_00[L + 1] <- log_mean_exp(loglikm)
elpd_fit_00 <- sum(elpds_fit_00, na.rm = TRUE)

# iterate over i > L
i_refit <- L
refits <- L
ks <- NULL
for (i in (L + 1):(N - M)) {
  past <- 1:i
  oos <- (i + 1):(i + M)
  df_past <- data[past, , drop = FALSE]
  df_oos <- data[c(past, oos), , drop = FALSE]
  loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
  
  logratio <- sum_log_ratios(loglik, (i_refit + 1):i)
  psis_obj <- suppressWarnings(psis(logratio))
  k <- pareto_k_values(psis_obj)
  ks <- c(ks, k)
  if (k > k_thres) {
    # refit the model based on the first i observations
    i_refit <- i
    refits <- c(refits, i)
    fit_past <- update(fit_past, newdata = df_past, recompile = FALSE)
    loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_00[i + 1] <- log_mean_exp(loglikm)
  } else {
    lw <- weights(psis_obj, normalize = TRUE)[, 1]
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_00[i + 1] <- log_sum_exp(lw + loglikm)
  }
} 

cat("model was refit ", length(refits), " times")

####HOLLING TYPE I (linear function, only algae dependent)####

elpds_fit_H1 <- rep(NA, N)
past <- 1:L
oos <- (L + 1):(L + M)
df_past <- data[past, , drop = FALSE]
df_oos <- data[c(past, oos), , drop = FALSE]
fit_past <- update(fit_H1, newdata = df_past, recompile = FALSE)
loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
loglikm <- rowSums(loglik[, oos])
elpds_fit_H1[L + 1] <- log_mean_exp(loglikm)
elpd_fit_H1 <- sum(elpds_fit_H1, na.rm = TRUE)

# iterate over i > L
i_refit <- L
refits <- L
ks <- NULL
for (i in (L + 1):(N - M)) {
  past <- 1:i
  oos <- (i + 1):(i + M)
  df_past <- data[past, , drop = FALSE]
  df_oos <- data[c(past, oos), , drop = FALSE]
  loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
  
  logratio <- sum_log_ratios(loglik, (i_refit + 1):i)
  psis_obj <- suppressWarnings(psis(logratio))
  k <- pareto_k_values(psis_obj)
  ks <- c(ks, k)
  if (k > k_thres) {
    # refit the model based on the first i observations
    i_refit <- i
    refits <- c(refits, i)
    fit_past <- update(fit_past, newdata = df_past, recompile = FALSE)
    loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_H1[i + 1] <- log_mean_exp(loglikm)
  } else {
    lw <- weights(psis_obj, normalize = TRUE)[, 1]
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_H1[i + 1] <- log_sum_exp(lw + loglikm)
  }
} 

cat("model was refit ", length(refits), " times")

####HOLLING TYPE II (hyperbolic function, only algae dependent)####
elpds_fit_H2 <- rep(NA, N)
past <- 1:L
oos <- (L + 1):(L + M)
df_past <- data[past, , drop = FALSE]
df_oos <- data[c(past, oos), , drop = FALSE]
fit_past <- update(fit_H2, newdata = df_past, recompile = FALSE)
loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
loglikm <- rowSums(loglik[, oos])
elpds_fit_H2[L + 1] <- log_mean_exp(loglikm)
elpd_fit_H2 <- sum(elpds_fit_H2, na.rm = TRUE)

# iterate over i > L
i_refit <- L
refits <- L
ks <- NULL
for (i in (L + 1):(N - M)) {
  past <- 1:i
  oos <- (i + 1):(i + M)
  df_past <- data[past, , drop = FALSE]
  df_oos <- data[c(past, oos), , drop = FALSE]
  loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
  
  logratio <- sum_log_ratios(loglik, (i_refit + 1):i)
  psis_obj <- suppressWarnings(psis(logratio))
  k <- pareto_k_values(psis_obj)
  ks <- c(ks, k)
  if (k > k_thres) {
    # refit the model based on the first i observations
    i_refit <- i
    refits <- c(refits, i)
    fit_past <- update(fit_past, newdata = df_past, recompile = FALSE)
    loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_H2[i + 1] <- log_mean_exp(loglikm)
  } else {
    lw <- weights(psis_obj, normalize = TRUE)[, 1]
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_H2[i + 1] <- log_sum_exp(lw + loglikm)
  }
} 

cat("model was refit ", length(refits), " times")

####ARDITI-GINZBURG TYPE I (linear function, ratio algae:fish dependent)####

elpds_fit_R1 <- rep(NA, N)
past <- 1:L
oos <- (L + 1):(L + M)
df_past <- data[past, , drop = FALSE]
df_oos <- data[c(past, oos), , drop = FALSE]
fit_past <- update(fit_R1, newdata = df_past, recompile = FALSE)
loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
loglikm <- rowSums(loglik[, oos])
elpds_fit_R1[L + 1] <- log_mean_exp(loglikm)
elpd_fit_R1 <- sum(elpds_fit_R1, na.rm = TRUE)

# iterate over i > L
i_refit <- L
refits <- L
ks <- NULL
for (i in (L + 1):(N - M)) {
  past <- 1:i
  oos <- (i + 1):(i + M)
  df_past <- data[past, , drop = FALSE]
  df_oos <- data[c(past, oos), , drop = FALSE]
  loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
  
  logratio <- sum_log_ratios(loglik, (i_refit + 1):i)
  psis_obj <- suppressWarnings(psis(logratio))
  k <- pareto_k_values(psis_obj)
  ks <- c(ks, k)
  if (k > k_thres) {
    # refit the model based on the first i observations
    i_refit <- i
    refits <- c(refits, i)
    fit_past <- update(fit_past, newdata = df_past, recompile = FALSE)
    loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_R1[i + 1] <- log_mean_exp(loglikm)
  } else {
    lw <- weights(psis_obj, normalize = TRUE)[, 1]
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_R1[i + 1] <- log_sum_exp(lw + loglikm)
  }
} 

cat("model was refit ", length(refits), " times")

####ARDITI-GINZBURG TYPE II (hyperbolic function, ratio algae:fish dependent)####

elpds_fit_R2 <- rep(NA, N)
past <- 1:L
oos <- (L + 1):(L + M)
df_past <- data[past, , drop = FALSE]
df_oos <- data[c(past, oos), , drop = FALSE]
fit_past <- update(fit_R2, newdata = df_past, recompile = FALSE)
loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
loglikm <- rowSums(loglik[, oos])
elpds_fit_R2[L + 1] <- log_mean_exp(loglikm)
elpd_fit_R2 <- sum(elpds_fit_R2, na.rm = TRUE)

# iterate over i > L
i_refit <- L
refits <- L
ks <- NULL
for (i in (L + 1):(N - M)) {
  past <- 1:i
  oos <- (i + 1):(i + M)
  df_past <- data[past, , drop = FALSE]
  df_oos <- data[c(past, oos), , drop = FALSE]
  loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
  
  logratio <- sum_log_ratios(loglik, (i_refit + 1):i)
  psis_obj <- suppressWarnings(psis(logratio))
  k <- pareto_k_values(psis_obj)
  ks <- c(ks, k)
  if (k > k_thres) {
    # refit the model based on the first i observations
    i_refit <- i
    refits <- c(refits, i)
    fit_past <- update(fit_past, newdata = df_past, recompile = FALSE)
    loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_R2[i + 1] <- log_mean_exp(loglikm)
  } else {
    lw <- weights(psis_obj, normalize = TRUE)[, 1]
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_R2[i + 1] <- log_sum_exp(lw + loglikm)
  }
} 

cat("model was refit ", length(refits), " times")

####ARDITI-AKCAKAYA TYPE I (linear function with exponential fish interference)####

elpds_fit_P1 <- rep(NA, N)
past <- 1:L
oos <- (L + 1):(L + M)
df_past <- data[past, , drop = FALSE]
df_oos <- data[c(past, oos), , drop = FALSE]
fit_past <- update(fit_P1, newdata = df_past, recompile = FALSE)
loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
loglikm <- rowSums(loglik[, oos])
elpds_fit_P1[L + 1] <- log_mean_exp(loglikm)
elpd_fit_P1 <- sum(elpds_fit_P1, na.rm = TRUE)

# iterate over i > L
i_refit <- L
refits <- L
ks <- NULL
for (i in (L + 1):(N - M)) {
  past <- 1:i
  oos <- (i + 1):(i + M)
  df_past <- data[past, , drop = FALSE]
  df_oos <- data[c(past, oos), , drop = FALSE]
  loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
  
  logratio <- sum_log_ratios(loglik, (i_refit + 1):i)
  psis_obj <- suppressWarnings(psis(logratio))
  k <- pareto_k_values(psis_obj)
  ks <- c(ks, k)
  if (k > k_thres) {
    # refit the model based on the first i observations
    i_refit <- i
    refits <- c(refits, i)
    fit_past <- update(fit_past, newdata = df_past, recompile = FALSE)
    loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_P1[i + 1] <- log_mean_exp(loglikm)
  } else {
    lw <- weights(psis_obj, normalize = TRUE)[, 1]
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_P1[i + 1] <- log_sum_exp(lw + loglikm)
  }
} 

cat("model was refit ", length(refits), " times")

####ARDITI-AKCAKAYA TYPE II (hyperbolic function with exponential fish interference)####

elpds_fit_P2 <- rep(NA, N)
past <- 1:L
oos <- (L + 1):(L + M)
df_past <- data[past, , drop = FALSE]
df_oos <- data[c(past, oos), , drop = FALSE]
fit_past <- update(fit_P2, newdata = df_past, recompile = FALSE)
loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
loglikm <- rowSums(loglik[, oos])
elpds_fit_P2[L + 1] <- log_mean_exp(loglikm)
elpd_fit_P2 <- sum(elpds_fit_P2, na.rm = TRUE)

# iterate over i > L
i_refit <- L
refits <- L
ks <- NULL
for (i in (L + 1):(N - M)) {
  past <- 1:i
  oos <- (i + 1):(i + M)
  df_past <- data[past, , drop = FALSE]
  df_oos <- data[c(past, oos), , drop = FALSE]
  loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
  
  logratio <- sum_log_ratios(loglik, (i_refit + 1):i)
  psis_obj <- suppressWarnings(psis(logratio))
  k <- pareto_k_values(psis_obj)
  ks <- c(ks, k)
  if (k > k_thres) {
    # refit the model based on the first i observations
    i_refit <- i
    refits <- c(refits, i)
    fit_past <- update(fit_past, newdata = df_past, recompile = FALSE)
    loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_P2[i + 1] <- log_mean_exp(loglikm)
  } else {
    lw <- weights(psis_obj, normalize = TRUE)[, 1]
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_P2[i + 1] <- log_sum_exp(lw + loglikm)
  }
} 

cat("model was refit ", length(refits), " times")

####IVLEV resource-dependent (only algae dependent)####

elpds_fit_IV <- rep(NA, N)
past <- 1:L
oos <- (L + 1):(L + M)
df_past <- data[past, , drop = FALSE]
df_oos <- data[c(past, oos), , drop = FALSE]
fit_past <- update(fit_IV, newdata = df_past, recompile = FALSE)
loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
loglikm <- rowSums(loglik[, oos])
elpds_fit_IV[L + 1] <- log_mean_exp(loglikm)
elpd_fit_IV <- sum(elpds_fit_IV, na.rm = TRUE)

# iterate over i > L
i_refit <- L
refits <- L
ks <- NULL
for (i in (L + 1):(N - M)) {
  past <- 1:i
  oos <- (i + 1):(i + M)
  df_past <- data[past, , drop = FALSE]
  df_oos <- data[c(past, oos), , drop = FALSE]
  loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
  
  logratio <- sum_log_ratios(loglik, (i_refit + 1):i)
  psis_obj <- suppressWarnings(psis(logratio))
  k <- pareto_k_values(psis_obj)
  ks <- c(ks, k)
  if (k > k_thres) {
    # refit the model based on the first i observations
    i_refit <- i
    refits <- c(refits, i)
    fit_past <- update(fit_past, newdata = df_past, recompile = FALSE)
    loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_IV[i + 1] <- log_mean_exp(loglikm)
  } else {
    lw <- weights(psis_obj, normalize = TRUE)[, 1]
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_IV[i + 1] <- log_sum_exp(lw + loglikm)
  }
} 

cat("model was refit ", length(refits), " times")

####IVLEV (ratio dependent)####

elpds_fit_IR <- rep(NA, N)
past <- 1:L
oos <- (L + 1):(L + M)
df_past <- data[past, , drop = FALSE]
df_oos <- data[c(past, oos), , drop = FALSE]
fit_past <- update(fit_IR, newdata = df_past, recompile = FALSE)
loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
loglikm <- rowSums(loglik[, oos])
elpds_fit_IR[L + 1] <- log_mean_exp(loglikm)
elpd_fit_IR <- sum(elpds_fit_IR, na.rm = TRUE)

# iterate over i > L
i_refit <- L
refits <- L
ks <- NULL
for (i in (L + 1):(N - M)) {
  past <- 1:i
  oos <- (i + 1):(i + M)
  df_past <- data[past, , drop = FALSE]
  df_oos <- data[c(past, oos), , drop = FALSE]
  loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
  
  logratio <- sum_log_ratios(loglik, (i_refit + 1):i)
  psis_obj <- suppressWarnings(psis(logratio))
  k <- pareto_k_values(psis_obj)
  ks <- c(ks, k)
  if (k > k_thres) {
    # refit the model based on the first i observations
    i_refit <- i
    refits <- c(refits, i)
    fit_past <- update(fit_past, newdata = df_past, recompile = FALSE)
    loglik <- log_lik(fit_past, newdata = df_oos, oos = oos)
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_IR[i + 1] <- log_mean_exp(loglikm)
  } else {
    lw <- weights(psis_obj, normalize = TRUE)[, 1]
    loglikm <- rowSums(loglik[, oos])
    elpds_fit_IR[i + 1] <- log_sum_exp(lw + loglikm)
  }
} 

cat("model was refit ", length(refits), " times")

ELPD <- c(
  "fit_00" = elpd_fit_00,
  "fit_H1" = elpd_fit_H1,
  "fit_H2" = elpd_fit_H2,
  "fit_R1" = elpd_fit_R1,
  "fit_R2" = elpd_fit_R2,
  "fit_P1" = elpd_fit_P1,
  "fit_P2" = elpd_fit_P2,
  "fit_IV" = elpd_fit_IV,
  "fit_IR" = elpd_fit_IR)

lpd_point <- cbind(elpd_fit_00, elpd_fit_H1, elpd_fit_H2, elpd_fit_R1,
                   elpd_fit_R2, elpd_fit_P1, elpd_fit_P2, elpd_fit_IV, elpd_fit_IR)
pbma_wts <- pseudobma_weights(lpd_point, BB=FALSE)
pbma_BB_wts <- pseudobma_weights(lpd_point) # default is BB=TRUE
stacking_wts <- stacking_weights(lpd_point)
estimates <- round(cbind(ELPD, pbma_wts, pbma_BB_wts, stacking_wts), 2)

write.csv(estimates, 'outputs_results/tables/results.csv')

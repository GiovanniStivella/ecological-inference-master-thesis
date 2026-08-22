set.seed(123)

library(truncnorm)
library(seine)
library(xtable)
# Repeated synthetic experiments to assess whether the estimated effect lies
# inside the confidence interval with the expected frequency

generate_synthetic_data <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  high_income_North <- matrix(rtruncnorm(100, a = 0, b = 1, mean = 0.7, sd = 0.2))
  high_income_Centre <- matrix(rtruncnorm(100, a = 0, b = 1, mean = 0.5, sd = 0.2))
  high_income_South <- matrix(rtruncnorm(100, a = 0, b = 1, mean = 0.3, sd = 0.2))
  
  high_income <- as.data.frame(rbind(high_income_North, high_income_Centre, high_income_South))
  
  x <- as.matrix(cbind(high_income = high_income, low_income = 1 - high_income))
  colnames(x) <- c("high_income", "low_income")
  
  area_yes <- matrix(1, 100)
  area_no <- matrix(0, 100)
  
  area_North <- rbind(area_yes, area_no, area_no)
  area_Centre <- rbind(area_no, area_yes, area_no)
  
  area <- cbind(area_North, area_Centre)
  colnames(area) <- c("area_North", "area_Centre")
  
  covariates <- as.matrix(cbind(x, area))
  
  diff <- c(0, 0.5, -0.3, 0.1)
  south <- as.matrix(c(0.6, 0.2))
  lambda <- matrix(diff, 2, 2)
  
  V12 <- as.matrix(covariates[, 3:4, drop = FALSE])
  eta <- t(apply(V12, 1, function(v) as.numeric(south + lambda %*% v)))
  colnames(eta) <- c("eta_high", "eta_low")
  
  beta <- matrix(rtruncnorm(a = 0, b = 1, n = 600, mean = eta, sd = 0.05), nrow = 300, ncol = 2)
  colnames(beta) <- c("beta_high", "beta_low")
  
  covariates <- cbind(covariates, eta, beta)
  
  outcome <- rowSums(covariates[, 1:2] * covariates[, 7:8])
  total <- rep(1, 300)
  
  as.data.frame(cbind(covariates, outcome, total))
}

n_rep <- 1000
coverage_lm <- data.frame(
  true_high = numeric(n_rep),
  est_high = numeric(n_rep),
  ci_low_high = numeric(n_rep),
  ci_high_high = numeric(n_rep),
  cover_high = logical(n_rep)
)
coverage_ei <- data.frame(
  true_high = numeric(n_rep),
  est_high = numeric(n_rep),
  ci_low_high = numeric(n_rep),
  ci_high_high = numeric(n_rep),
  cover_high = logical(n_rep)
)


for (r in 1:n_rep) {
  dat <- generate_synthetic_data(seed = 1000 + r)
  
  reg <- lm(outcome ~ high_income + area_North + area_Centre, data = dat)
  beta_true <- sum(dat$high_income * dat$beta_high) / sum(dat$high_income)
  ci <- confint(reg, parm = "high_income")
  
  coverage_lm$true_high[r] <- beta_true
  coverage_lm$est_high[r] <- coef(reg)["high_income"]
  coverage_lm$ci_low_high[r] <- ci[1, 1]
  coverage_lm$ci_high_high[r] <- ci[1, 2]
  coverage_lm$cover_high[r] <- (beta_true >= ci[1, 1]) && (beta_true <= ci[1, 2])
  
  synthspec <- ei_spec(dat, predictors = high_income:low_income, outcome = outcome, total = total, covariates = c(area_North,area_Centre))
  
  # Run the semiparametric estimator
  m <- ei_ridge(synthspec)
  rr <- ei_riesz(synthspec, penalty = m$penalty)
  
  ei_estimates <- ei_est(regr = m, riesz = rr, data = synthspec, conf_level = 0.95)
  ei_estimates_df <- as.data.frame(ei_estimates)
  
  coverage_ei$true_high[r] <- beta_true
  coverage_ei$est_high[r] <- ei_estimates_df[1,3]
  coverage_ei$ci_low_high[r] <- ei_estimates_df[1,5]
  coverage_ei$ci_high_high[r] <- ei_estimates_df[1,6]
  coverage_ei$cover_high[r] <- (beta_true >= ei_estimates_df[1,5]) && (beta_true <= ei_estimates_df[1,6])
  
  
  
  
}

mean(coverage_lm$cover_high)
mean(coverage_ei$cover_high)
#remotes::install_github("CoryMcCartan/seine")
#install.packages("truncnorm")
set.seed(123)

library(truncnorm)
library(seine)
library(xtable)

high_income <- matrix(rtruncnorm(300, a=0, b=1, mean = 0.5, sd = 0.2))

zeta <- matrix(rtruncnorm(300, a=0, b=1, mean = 1-high_income, sd = 0.2))

eta_high <- 10^(zeta-1)
eta_low <- exp(zeta-1)

beta_high <- matrix(rtruncnorm(300, a=0, b=1, mean = eta_high, sd = 0.05))
beta_low <- matrix(rtruncnorm(300, a=0, b=1, mean = eta_low, sd = 0.05))


x <- as.matrix(cbind(high_income = high_income, low_income = 1-high_income, zeta = zeta, eta_high = eta_high, eta_low = eta_low, beta_high = beta_high, beta_low = beta_low))
colnames(x) <- c("high_income", "low_income", "zeta", "eta_high", "eta_low", "beta_high", "beta_low")

outcome <- rowSums(x[, 1:2] * x[, 6:7])

total <- rep(1,300)

synthetic_dataset <- as.data.frame(cbind(x, outcome, total))

#I try and run the regressions on our synthetic dataset

#The first idea is to run a linear regression

naive <- lm(outcome~high_income, data = synthetic_dataset)

summary(naive)


# Save a compact, readable regression summary
naive_coef <- summary(naive)$coefficients
rownames(naive_coef) <- gsub("_", "\\_", rownames(naive_coef), fixed = TRUE)

tab <- xtable(
  naive_coef,
  caption = "Naive OLS regression summary",
  digits = 3,
  label = "tab:naive-regression-exp"
)

print(
  tab,
  file = "../Paper/Images/naive_regression_summary_exp.tex",
  include.rownames = TRUE,
  sanitize.text.function = identity
)
#Then I move to McCartan and Kuriwaki methodology

synthspec <- ei_spec(
  synthetic_dataset, 
  predictors = high_income:low_income,
  outcome = outcome,
  total = total,
  covariates = c(zeta)
)

print(synthspec)

# Run the semiparametric estimator
m <- ei_ridge(synthspec)
rr <- ei_riesz(synthspec, penalty = m$penalty)

ei_estimates <- ei_est(
  regr = m,
  riesz = rr,
  data = synthspec,
  conf_level = 0.95
)

# Convert to plain data.frame and escape underscores in predictor names
ei_estimates_df <- as.data.frame(ei_estimates)
ei_estimates_df$predictor <- gsub("_", "\\_", ei_estimates_df$predictor, fixed = TRUE)

tab <- xtable(
  ei_estimates_df,
  caption = "Semiparametric ecological inference estimates",
  digits = 3,
  label = "tab:ei-estimates-exp"
)

print(
  tab,
  file = "../Paper/Images/ei_estimates_summary_exp.tex",
  include.rownames = FALSE,
  sanitize.text.function = identity
)

#I compare with "true" values

high <- sum(synthetic_dataset$high_income*synthetic_dataset$beta_high)/sum(synthetic_dataset$high_income)

low <- sum(synthetic_dataset$low_income*synthetic_dataset$beta_low)/sum(synthetic_dataset$low_income)


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

n_rep <- 200
coverage <- data.frame(
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

  coverage$true_high[r] <- beta_true
  coverage$est_high[r] <- coef(reg)["high_income"]
  coverage$ci_low_high[r] <- ci[1, 1]
  coverage$ci_high_high[r] <- ci[1, 2]
  coverage$cover_high[r] <- (beta_true >= ci[1, 1]) && (beta_true <= ci[1, 2])
}

mean(coverage$cover_high)


#Un'altra cosa da fare è una regressione con covariate
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

#But firstly we compute the true values

high <- sum(synthetic_dataset$high_income*synthetic_dataset$beta_high)/sum(synthetic_dataset$high_income)

low <- sum(synthetic_dataset$low_income*synthetic_dataset$beta_low)/sum(synthetic_dataset$low_income)

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

print(ei_estimates_df)

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

#Interaction with covariates

interaction <- lm(outcome~high_income*zeta, data = synthetic_dataset)

summary(interaction)

#Here I should recover the estimate of beta
interaction_coef <- summary(interaction)$coefficients
beta_fitted_low <- interaction_coef[1]+interaction_coef[3]*zeta
beta_fitted_high <- interaction_coef[1]+interaction_coef[2]+(interaction_coef[3]+interaction_coef[4])*zeta

estimation_dataset <- cbind(synthetic_dataset, beta_fitted_high, beta_fitted_low)

beta_low_int <- sum(estimation_dataset$low_income*estimation_dataset$beta_fitted_low)/sum(estimation_dataset$low_income)

beta_high_int <- sum(estimation_dataset$low_income*estimation_dataset$beta_fitted_high)/sum(estimation_dataset$low_income)

# Save a compact, readable regression summary
interaction_coef <- summary(interaction)$coefficients
rownames(interaction_coef) <- gsub("_", "\\_", rownames(interaction_coef), fixed = TRUE)

tab <- xtable(
  interaction_coef,
  caption = "Parametric estimation with covariates regression summary",
  digits = 3,
  label = "tab:interaction-regression-exp"
)

print(
  tab,
  file = "../Paper/Images/interaction_regression_summary_exp.tex",
  include.rownames = TRUE,
  sanitize.text.function = identity
)
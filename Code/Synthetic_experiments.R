#remotes::install_github("CoryMcCartan/seine")
#install.packages("truncnorm")
set.seed(123)

library(truncnorm)
library(seine)

high_income_North <- matrix(rtruncnorm(100, a=0, b=1, mean = 0.7, sd = 0.2))

high_income_Centre <- matrix(rtruncnorm(100, a=0, b=1, mean = 0.5, sd = 0.2))

high_income_South <- matrix(rtruncnorm(100, a=0, b=1, mean = 0.3, sd = 0.2))

high_income <- as.data.frame(rbind(high_income_North, high_income_Centre, high_income_South))

x <- as.matrix(cbind(high_income = high_income, low_income = 1-high_income))
colnames(x) <- c("high_income", "low_income")

#As the only covariate, I use geographical location
area_yes <- matrix(1,100)
area_no <- matrix(0,100)

area_North <- rbind(area_yes, area_no, area_no)
area_Centre <- rbind(area_no, area_yes, area_no)

area <- cbind(area_North, area_Centre)
colnames(area) <- c("area_North", "area_Centre")

covariates <- as.matrix(cbind(x, area))

diff <- c(0,0.5,-0.3,0.1)

south <- as.matrix(c(0.6,0.2))

lambda <- matrix (diff, 2, 2)

V12 <- as.matrix(covariates[, 3:4, drop = FALSE])
eta <- t(apply(V12, 1, function(v) as.numeric(south + lambda %*% v)))
colnames(eta) <- c("eta_high", "eta_low")

beta <- matrix(rtruncnorm(a=0, b=1, n=600, mean = eta, sd = 0.05), nrow = 300, ncol = 2)
colnames(beta) <- c("beta_high", "beta_low")

covariates <- cbind(covariates, eta, beta)

outcome <- rowSums(covariates[, 1:2] * covariates[, 7:8])

total <- rep(1,300)

synthetic_dataset <- as.data.frame(cbind(covariates, outcome, total))

#I try and run the regressions on our synthetic dataset

#The first idea is to run a linear regression

naive <- lm(outcome~high_income, data = synthetic_dataset)

summary(naive)

#Then I move to McCartan and Kuriwaki methodology

synthspec <- ei_spec(
  synthetic_dataset, 
  predictors = high_income:low_income,
  outcome = outcome,
  total = total,
  covariates = c(area_North,area_Centre)
)

print(synthspec)

m <- ei_ridge(synthspec)
rr <- ei_riesz(synthspec, penalty = m$penalty)

ei_est(regr = m, riesz = rr, data = synthspec, conf_level = 0.95)

#I compare with "true" values

high <- sum(synthetic_dataset$high_income*synthetic_dataset$beta_high)/sum(synthetic_dataset$high_income)

low <- sum(synthetic_dataset$low_income*synthetic_dataset$beta_low)/sum(synthetic_dataset$low_income)

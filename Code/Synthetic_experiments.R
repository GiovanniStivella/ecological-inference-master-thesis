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

#I firstly try without covariates

#synthetic_dataset_no_covariates <- ei_synthetic(n=300, n_x = 1, x = high_income)

#naive_no_covariates <- lm(y ~ x1, data = synthetic_dataset_no_covariates)

#summary(naive_no_covariates)

#I would like to add covariates
area_yes <- matrix(1,100)
area_no <- matrix(0,100)

area_North <- rbind(area_yes, area_no, area_no)
area_Centre <- rbind(area_no, area_yes, area_no)
#area_South <- rbind(area_no, area_no, area_yes)

area <- cbind(area_North, area_Centre)

covariates <- as.matrix(cbind(x, area))

x <- as.matrix(cbind(high_income = covariates[, 1], low_income = 1 - covariates[, 1]))

z <- as.matrix(covariates[, 2:3, drop = FALSE])

synthetic_dataset <- ei_synthetic(x = x, z = z)
synthetic_dataset[, c("z1", "z2")] <- z
total <- matrix(100,300)
synthetic_dataset <- cbind(synthetic_dataset, total)

#I try and run the regressions on our synthetic dataset

#The first idea is to run a linear regression

naive <- lm(y~x1, data = synthetic_dataset)

summary(naive)

#Then I move to McCartan and Kuriwaki methodology

synthspec <- ei_spec(
  synthetic_dataset, 
  predictors = x1:x2,
  outcome = y,
  total = total,
  covariates = c(z1,z2)
)

print(synthspec)

synthm <- ei_ridge(synthspec)
synthrr <- ei_riesz(synthspec, penalty = m$penalty)

ei_est(regr = synthm, riesz = synthrr, data = synthspec, conf_level = 0.95)
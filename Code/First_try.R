#remotes::install_github("CoryMcCartan/seine")

set.seed(1234)

library(seine)

#Here there is the experiment that seine runs with default data

data(elec_1968)

spec = ei_spec(
  elec_1968, 
  predictors = vap_white:vap_other,
  outcome = pres_dem_hum:pres_ind_wal, 
  total = pres_total,
  covariates = c(state, pop_city:pop_rural, farm:educ_coll, 
                 inc_00_03k:inc_25_99k)
)

print(spec)

m = ei_ridge(spec)
rr = ei_riesz(spec, penalty = m$penalty)

ei_est(regr = m, riesz = rr, data = spec, conf_level = 0.95)


#Now I try and have a look at how the results vary once we consider a synthetic dataset

high_income_North <- matrix(rnorm(100, mean = 0.7, sd = 0.1))

high_income_Centre <- matrix(rnorm(100, mean = 0.5, sd = 0.1))

high_income_South <- matrix(rnorm(100, mean = 0.3, sd = 0.1))

high_income <- as.data.frame(rbind(high_income_North, high_income_Centre, high_income_South))

#I firstly try without covariates

synthetic_dataset_no_covariates <- ei_synthetic(n=300, n_x = 1, x = high_income)

naive_no_covariates <- lm(y ~ x1, data = synthetic_dataset_no_covariates)

summary(naive_no_covariates)

#I would like to add covariates


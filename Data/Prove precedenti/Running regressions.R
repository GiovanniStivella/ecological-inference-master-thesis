library(seine)

data <- readRDS("merging.rds")

spec <- ei_spec(
  data, 
  predictors = ,
  outcome = ,
  total = vap,
  covariates = 
)

m <- ei_ridge(spec)
rr <- ei_riesz(spec, penalty = m$penalty)

ei_est(regr = m, riesz = rr, data = spec, conf_level = 0.95)
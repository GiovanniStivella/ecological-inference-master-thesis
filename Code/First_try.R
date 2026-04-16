#remotes::install_github("CoryMcCartan/seine")

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


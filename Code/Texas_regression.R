set.seed(123)

library(dplyr)

library(seine)

library(xtable)

data <- readRDS("/Users/giovannistivella/Documents/Università/UniPi/Magistrale/ecological-inference-master-thesis/Data/texas_with_covariates.rds")

#Let's run the following experiment:
#I want to estimate how voters voted in 2020 presidential election based on ethnicity
#But I may consider four categories: White, Black, Hispanic, Other
#As covariates to describe the precinct, I use the following:
#educational attainment of people over 25 (B15003): whites in electoral precincts with more graduates are generally different from whites in electoral precincts in rural areas with less graduates
#language (B16004): electoral precincts where people speak languages different from English may be either more urban (effect already accounted for) or near the border (not accounted yet)
#however, it risks being highly collinear with ethnicity
#employmentstatus(B23025): voters behave differently in electoral precincts with different unemployment rates

#However, as for educational attainment I do not want that detail: I just want to divide the population in people with (B15003_002:B15003_020) and without a college degree (B15003_021:B15003_025)

#I need to add a column with total votes in each election and then I have to change other columns as proportions

elec_2020 <- data %>%
  mutate(
    pres_total = pre_20_rep_tru + pre_20_dem_bid
  )

elec_2020 <- elec_2020 %>%
  mutate(
    no_college = rowSums(across(B15003_002:B15003_020)),
    college = rowSums(across(B15003_021:B15003_025))
  )

elec_2020 <- elec_2020 %>%
  mutate(
    other_ethnicity = rowSums(across(vap_aian:vap_two))
  )


elec_2020 <- ei_proportions(elec_2020, pre_20_rep_tru:pre_20_dem_bid, .total = pres_total)

#Apparently, there exist 200 precincts where neither Trump nor Biden got votes (in general, nearly no votes are reported in these precincts in no election); then, there are some other precincts with tiny rounding errors

elec_2020 <- ei_proportions(elec_2020, c(vap_hisp:vap_black, other_ethnicity), .total = vap)
elec_2020 <- ei_proportions(elec_2020, no_college:college, .total = B15003_001)
#elec_2020 <- ei_proportions(elec_2020, B15003_002:B15003_025, .total = B15003_001)
#elec_2020 <- ei_proportions(elec_2020, B16004_001:B16004_067, .total = B16004_001)
elec_2020 <- ei_proportions(elec_2020, B23025_004:B23025_007, .total = B23025_001)


experiment <- ei_spec(
  elec_2020, 
  predictors = c(vap_hisp:vap_black, other_ethnicity),
  outcome = pre_20_rep_tru:pre_20_dem_bid, 
  total = pres_total,
  covariates = no_college:college
)

m <- ei_ridge(experiment)
rr <- ei_riesz(experiment, penalty = m$penalty)

ei_estimates <- ei_est(regr = m, riesz = rr, data = experiment, conf_level = 0.95)

#Save results in .tex table
ei_estimates_df <- as.data.frame(ei_estimates)
ei_estimates_df$predictor <- gsub("_", "\\_", ei_estimates_df$predictor, fixed = TRUE)
ei_estimates_df$outcome <- gsub("_", "\\_", ei_estimates_df$outcome, fixed = TRUE)

results_table <- xtable(
  ei_estimates_df,
  caption = "Semiparametric ecological inference estimates for the 2020 presidential election in Texas",
  digits = 3,
  label = "tab:ei-estimates-texas")

print(results_table,
      file = "../Paper/Images/ei_estimates_texas_summary.tex",
      include.rownames = TRUE,
      sanitize.text.function = identity
)

####Other experiments###


#I also have one code for each county (there are 254 counties), we might add this as covariate but we might risk losing identifiability

experiment <- ei_spec(
  elec_2020, 
  predictors = vap_hisp:vap_two,
  outcome = pre_20_rep_tru:pre_20_dem_bid,
  total = pres_total,
  covariates = c(B15003_002:B15003_025, B23025_004:B23025_007, county)
)

m <- ei_ridge(experiment)
rr <- ei_riesz(experiment, penalty = m$penalty)

ei_est(regr = m, riesz = rr, data = experiment, conf_level = 0.95)

###

experiment <- ei_spec(
  elec_2020, 
  predictors = vap_hisp:vap_two,
  outcome = pre_20_rep_tru:pre_20_dem_bid, 
  total = pres_total,
  covariates = c(B15003_002:B15003_025, county)
)

m <- ei_ridge(experiment)
rr <- ei_riesz(experiment, penalty = m$penalty)

ei_est(regr = m, riesz = rr, data = experiment, conf_level = 0.95)

###


experiment <- ei_spec(
  elec_2020, 
  predictors = vap_hisp:vap_two,
  outcome = pre_20_rep_tru:pre_20_dem_bid, 
  total = pres_total,
  covariates = no_college:college
)

m <- ei_ridge(experiment)
rr <- ei_riesz(experiment, penalty = m$penalty)

ei_est(regr = m, riesz = rr, data = experiment, conf_level = 0.95)

###


experiment <- ei_spec(
  elec_2020, 
  predictors = college:no_college,
  outcome = pre_20_rep_tru:pre_20_dem_bid, 
  total = pres_total,
  covariates = c(vap_hisp:vap_two)
)

m <- ei_ridge(experiment)
rr <- ei_riesz(experiment, penalty = m$penalty)

ei_est(regr = m, riesz = rr, data = experiment, conf_level = 0.95)
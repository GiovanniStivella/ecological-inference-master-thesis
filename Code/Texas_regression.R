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


#I will drop all the precincts where the total number of votes is less than 10: these data are probably due to errors; even the ones which are not the result of errors will not affect the estimates too much;
#Moreover, I will also drop the precincts where reported voting age population is less than 10
#Finally, I will drop also the precincts where there are more reported votes than voting age population, which is obviously erroneous

elec_2020_prerefined <- elec_2020%>%filter(pres_total>10 & vap>10 & vap>pres_total)


paper_images_dir <- "/Users/giovannistivella/Documents/Università/UniPi/Magistrale/ecological-inference-master-thesis/Paper/Images"
dir.create(paper_images_dir, recursive = TRUE, showWarnings = FALSE)
pdf(file.path(paper_images_dir, "vap_vs_pres_total_prerefined.pdf"), width = 8, height = 6)
plot(elec_2020_prerefined$vap, elec_2020_prerefined$pres_total)
dev.off()


#We might note that we would expect a more linear relationship, while certain precincts where vap is much greater than pres_total draw suspicion; I will further restrict

elec_2020_refined <- elec_2020_prerefined%>%filter(pres_total>vap/5)

attach(elec_2020_refined)

pdf(file.path(paper_images_dir, "vap_vs_pres_total_refined.pdf"), width = 8, height = 6)
plot(vap, pres_total)
dev.off()

#Tests for bounded N
pdf(file.path(paper_images_dir, "pres_total_histogram.pdf"), width = 8, height = 6)
hist(elec_2020_refined$pres_total)
dev.off()
summary(elec_2020_refined$pres_total)
print(sort(elec_2020_refined$pres_total, decreasing = TRUE))

#Test for positivity assumption
pdf(file.path(paper_images_dir, "positivity_hispanic_college.pdf"), width = 8, height = 6)
plot(elec_2020_refined$vap_hisp, elec_2020_refined$college)
dev.off()

pdf(file.path(paper_images_dir, "positivity_white_college.pdf"), width = 8, height = 6)
plot(elec_2020_refined$vap_white, elec_2020_refined$college)
dev.off()

pdf(file.path(paper_images_dir, "positivity_black_college.pdf"), width = 8, height = 6)
plot(elec_2020_refined$vap_black, elec_2020_refined$college)
dev.off()

pdf(file.path(paper_images_dir, "positivity_other_ethnicity_college.pdf"), width = 8, height = 6)
plot(elec_2020_refined$other_ethnicity, elec_2020_refined$college)
dev.off()

#CAR is untestable

experiment <- ei_spec(
  elec_2020_refined, 
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
  caption = "Semiparametric estimates for the 2020 presidential election in Texas",
  digits = 3,
  label = "tab:ei-estimates-texas")

print(results_table,
      file = file.path(paper_images_dir, "ei_estimates_texas_summary.tex"),
      include.rownames = FALSE,
      sanitize.text.function = identity
)

####Other experiments###


#I also have one code for each county (there are 254 counties), we might add this as covariate but we might risk losing identifiability

experiment2 <- ei_spec(
  elec_2020_refined, 
  predictors = c(vap_hisp:vap_black, other_ethnicity),
  outcome = pre_20_rep_tru:pre_20_dem_bid,
  total = pres_total,
  covariates = c(B15003_002:B15003_025, B23025_004:B23025_007, county)
)

m <- ei_ridge(experiment2)
rr <- ei_riesz(experiment2, penalty = m$penalty)

ei_est(regr = m, riesz = rr, data = experiment2, conf_level = 0.95)

ei_estimates2 <- ei_est(regr = m, riesz = rr, data = experiment2, conf_level = 0.95)

###

experiment3 <- ei_spec(
  elec_2020_refined, 
  predictors = c(vap_hisp:vap_black, other_ethnicity),
  outcome = pre_20_rep_tru:pre_20_dem_bid, 
  total = pres_total,
  covariates = c(B15003_002:B15003_025, county)
)

m <- ei_ridge(experiment3)
rr <- ei_riesz(experiment3, penalty = m$penalty)

ei_est(regr = m, riesz = rr, data = experiment3, conf_level = 0.95)

ei_estimates3 <- ei_est(regr = m, riesz = rr, data = experiment3, conf_level = 0.95)


###


experiment4 <- ei_spec(
  elec_2020_refined, 
  predictors = college:no_college,
  outcome = pre_20_rep_tru:pre_20_dem_bid, 
  total = pres_total,
  covariates = c(vap_hisp:vap_black, other_ethnicity)
)

m <- ei_ridge(experiment4)
rr <- ei_riesz(experiment4, penalty = m$penalty)

ei_est(regr = m, riesz = rr, data = experiment4, conf_level = 0.95)

ei_estimates4 <- ei_est(regr = m, riesz = rr, data = experiment4, conf_level = 0.95)


###Let's move to linear estimations with covariates

###

linearexperiment <- lm(pre_20_rep_tru~(vap_hisp+vap_white+vap_black)*college, data = elec_2020_refined)
summary(linearexperiment)

lincoef <- summary(linearexperiment)$coefficients


rownames(lincoef) <- gsub("_", "\\_", rownames(lincoef), fixed = TRUE)

tab <- xtable(
  lincoef,
  caption = "Coefficients of regression with interactions with covariates for the 2020 presidential election in Texas (Republican share as outcome)",
  digits = 3,
  label = "tab:interaction-regression-texas"
)

print(
  tab,
  file = file.path(paper_images_dir, "interaction_regression_summary_texas.tex"),
  include.rownames = TRUE,
  sanitize.text.function = identity
)


beta_fitted_hisp <- lincoef[1]+lincoef[2]+(lincoef[5]+lincoef[6])*elec_2020_refined$college
beta_fitted_white <- lincoef[1]+lincoef[3]+(lincoef[5]+lincoef[7])*elec_2020_refined$college
beta_fitted_black <- lincoef[1]+lincoef[4]+(lincoef[5]+lincoef[8])*elec_2020_refined$college
beta_fitted_other <- lincoef[1]+lincoef[5]*elec_2020_refined$college

beta_hisp <- sum(elec_2020_refined$vap_hisp*elec_2020_refined$pres_total*beta_fitted_hisp)/sum(elec_2020_refined$vap_hisp*elec_2020_refined$pres_total)

beta_white <- sum(elec_2020_refined$vap_white*elec_2020_refined$pres_total*beta_fitted_white)/sum(elec_2020_refined$vap_white*elec_2020_refined$pres_total)

beta_black <- sum(elec_2020_refined$vap_black*elec_2020_refined$pres_total*beta_fitted_black)/sum(elec_2020_refined$vap_black*elec_2020_refined$pres_total)

beta_other <- sum(elec_2020_refined$other_ethnicity*elec_2020_refined$pres_total*beta_fitted_other)/sum(elec_2020_refined$other_ethnicity*elec_2020_refined$pres_total)


beta_texas <- rbind (beta_hisp, beta_white, beta_black, beta_other)
rownames(beta_texas) <- gsub("_", "\\_", rownames(beta_texas), fixed = TRUE)

inter <- xtable(
  beta_texas,
  caption = "Estimates of linear specification with interactions for the 2020 presidential election in Texas (Republican share as outcome)",
  digits = 3,
  label = "tab:int-tex-beta"
)

print(
  inter,
  file = file.path(paper_images_dir, "int_texas_beta.tex"),
  include.rownames = TRUE,
  include.colnames = FALSE,
  sanitize.text.function = identity
)


###


beta_fitted_college <- lincoef[1]+lincoef[5]+((lincoef[2]+lincoef[6])*elec_2020_refined$vap_hisp+(lincoef[3]+lincoef[7])*elec_2020_refined$vap_white+(lincoef[4]+lincoef[8])*elec_2020_refined$vap_black)
beta_fitted_no_college <- lincoef[1]+(lincoef[2]*elec_2020_refined$vap_hisp+lincoef[3]*elec_2020_refined$vap_white+lincoef[4]*elec_2020_refined$vap_black)


beta_college <- sum(elec_2020_refined$college*elec_2020_refined$pres_total*beta_fitted_college)/sum(elec_2020_refined$college*elec_2020_refined$pres_total)

beta_no_college <- sum(elec_2020_refined$no_college*elec_2020_refined$pres_total*beta_fitted_no_college)/sum(elec_2020_refined$no_college*elec_2020_refined$pres_total)
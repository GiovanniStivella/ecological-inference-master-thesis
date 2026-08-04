#Let's take Texas
texas <- read.table("https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-2020/tx_2020_vtd.csv",
                    header = TRUE,    # first row of file contains variable names
                    sep = ",",        # need to specify that the file is comma-separator
                    dec = ".")        # useless here (as it is the default), can be useful with Italian settings (dec = ",")
#There are 9007 VTDs

library(tidyr)
library(dplyr)
library(broom)

library(tigris)
library(ggplot2)

library(tidycensus)
library(tidyverse)

#census_api_key("4c7376779d3541cf0bac0eff12edea2ad63d4bb1", install = TRUE)

c <- load_variables(2020, "pl")   #P1_001N is a total

acs_variables <- load_variables(2020, "acs5")

#We can only use those variables which are collected at block group level (every higher unit of aggregation is too vast)
#Possibilities: sexbyage; race and other ethnic;geographic mobility; work mobility & geography (where you work, how you commute); family type (with lot of possible details); school enrollment (again, with lots of details); educational attainment (by sex, by type of bachelor's degree); age by language spoken; poverty status; income, earnings, wage, etc.; a lot of combinations of these variables; housing status (who owns it, its characteristics); health insurance; computer and internet; ALLOCATION OF CITIZENSHIP STATUS (I didn't get it);  

#Using get_decennial to have population at census block level (block data are not available in the ACS)
weights <- get_decennial(
  geography = "block",
  state = "TX",
  variables = "P1_001N",
  year = 2020
)

#There are 219672 blocks (out of 668757) without people 

#(https://www.arcgis.com/apps/mapviewer/index.html?layers=2f5e592494d243b0aa5c253e75e792a4)


#There are 465950/25=18638 block groups, which make for an average of 36 blocks per block group

#Now we open the table that links blocks and VTDs
block_codes <- read.table("BlockAssign_ST48_TX_VTD.txt",
                          header = TRUE,
                          sep = "|")
#The number of observations in block_codes coincide with the number of observations in weights 
#That is a good check of the total number of blocks

block_codes <- block_codes%>%mutate(BLOCKID = as.character(BLOCKID))

#Using COUNTYFP and DISTRICT I can recover the code of each VTD
#Then I can link each census block to its VTD
block_codes <- block_codes %>%
  mutate(VTDST20GEOID = paste0("48", sprintf("%03s", COUNTYFP), DISTRICT))

#Using the first 12 characters I can link each census block to its census block group
block_codes <- block_codes%>%mutate(block_group = substr(as.character(BLOCKID), 1, 12))

#Joining codes with population
block_codes_with_population <- block_codes%>%
  left_join(weights, by=c("BLOCKID"="GEOID"))

#Count how many blocks are there in each block group
block_groups <- block_codes %>%
  group_by(block_group) %>%
  summarise(n_rows = n(), .groups = "drop")

#Count how many blocks of a certain block group are there in each VTD
block_vtds_pair <- block_codes %>%
  group_by(block_group, VTDST20GEOID) %>%
  summarise(n_rows = n(), .groups = "drop")

#Collect all the block_groups which are not entirely in a VTD
differences <- block_groups%>%
  left_join(block_vtds_pair, by=c("block_group"="block_group"))%>%
  filter(n_rows.x != n_rows.y)

#Take every block that is not in a block group that coincides with a VTD (filter)
#For each block I consider its VTD, its block_group and its population (in block_codes_with_population)
differences_and_weights <- block_codes_with_population %>%
  filter(block_group %in% differences$block_group)

#Compute the population of each couple of VTD and block_group
differences_weighted <- differences_and_weights %>%
  group_by(VTDST20GEOID, block_group) %>%
  summarise(pop = sum(value))
#And then compute the population of each block_group
differences_weighted <- differences_weighted %>%
  group_by(block_group) %>%
  mutate(group_pop = sum(pop)) %>%
  ungroup()

#Now consider the block census groups which are contained in VTDs
easy_groups <- block_groups%>%
  left_join(block_vtds_pair, by=c("block_group"="block_group"))%>%
  filter(n_rows.x == n_rows.y)

#Educational attainment for people over 25
educational <- get_acs(geography = "cbg", #it returns estimates with moe (margin of error)
                     state = "Texas",
                     table = "B15003",
                     cache_table = TRUE,
                     year = 2020)

#For those block census groups and VTDs I take data in the first request of get_acs (education)
auto_easy <- easy_groups %>%
  split(.$VTDST20GEOID)%>%
  lapply(function(df) {
    educational %>%
      filter(GEOID %in% df$block_group) %>%
      group_by(variable) %>%
      summarise(easy_estimate = sum(estimate, na.rm = TRUE)) %>%
      mutate(VTDST20GEOID = unique(df$VTDST20GEOID))
  }) %>%
  bind_rows()

#Now I can consider block groups which are not entirely contained in VTDs
auto_tedious <- differences_weighted %>%
  split(.$VTDST20GEOID)%>%
  lapply(function(df) {
    educational %>%
      filter(GEOID %in% df$block_group) %>%
      group_by(variable) %>%
      summarise(tedious_estimate = sum(estimate*(df$pop/df$group_pop), na.rm = TRUE)) %>%
      mutate(VTDST20GEOID = unique(df$VTDST20GEOID))
  }) %>%
  bind_rows()

education <- auto_easy %>%
  full_join(auto_tedious, by = c("variable"="variable", "VTDST20GEOID"="VTDST20GEOID"))%>%
  mutate(total_estimate = coalesce(easy_estimate, 0) + coalesce(tedious_estimate, 0))%>%
  select("VTDST20GEOID", "variable", "total_estimate")

education_wide <- education %>%
  pivot_wider(names_from = variable, values_from = total_estimate)

merging <- education_wide%>%
  left_join(texas, by = c("VTDST20GEOID"="GEOID20"))

#Sex by age
sexbyage <- get_acs(geography = "cbg", #it returns estimates with moe (margin of error)
                    state = "Texas",
                    table = "B01001",
                    cache_table = TRUE,
                    year = 2020)

sexage_easy <- easy_groups %>%
  split(.$VTDST20GEOID)%>%
  lapply(function(df) {
    sexbyage %>%
      filter(GEOID %in% df$block_group) %>%
      group_by(variable) %>%
      summarise(easy_estimate = sum(estimate, na.rm = TRUE)) %>%
      mutate(VTDST20GEOID = unique(df$VTDST20GEOID))
  }) %>%
  bind_rows()

#Now I can consider block groups which are not entirely contained in VTDs
sexage_tedious <- differences_weighted %>%
  split(.$VTDST20GEOID)%>%
  lapply(function(df) {
    sexbyage %>%
      filter(GEOID %in% df$block_group) %>%
      group_by(variable) %>%
      summarise(tedious_estimate = sum(estimate*(df$pop/df$group_pop), na.rm = TRUE)) %>%
      mutate(VTDST20GEOID = unique(df$VTDST20GEOID))
  }) %>%
  bind_rows()

sexage <- sexage_easy %>%
  full_join(sexage_tedious, by = c("variable"="variable", "VTDST20GEOID"="VTDST20GEOID"))%>%
  mutate(total_estimate = coalesce(easy_estimate, 0) + coalesce(tedious_estimate, 0))%>%
  select("VTDST20GEOID", "variable", "total_estimate")

sexage_wide <- sexage %>%
  pivot_wider(names_from = variable, values_from = total_estimate)

#(I have verified that total populations coincide in sexage and education: the only difference is due to education accounting for only over 25)

#Race
racedata <- get_acs(geography = "cbg", #it returns estimates with moe (margin of error)
                    state = "Texas",
                    table = "B02001",
                    cache_table = TRUE,
                    year = 2020)

race_easy <- easy_groups %>%
  split(.$VTDST20GEOID)%>%
  lapply(function(df) {
    racedata %>%
      filter(GEOID %in% df$block_group) %>%
      group_by(variable) %>%
      summarise(easy_estimate = sum(estimate, na.rm = TRUE)) %>%
      mutate(VTDST20GEOID = unique(df$VTDST20GEOID))
  }) %>%
  bind_rows()

#Now I can consider block groups which are not entirely contained in VTDs
race_tedious <- differences_weighted %>%
  split(.$VTDST20GEOID)%>%
  lapply(function(df) {
    racedata %>%
      filter(GEOID %in% df$block_group) %>%
      group_by(variable) %>%
      summarise(tedious_estimate = sum(estimate*(df$pop/df$group_pop), na.rm = TRUE)) %>%
      mutate(VTDST20GEOID = unique(df$VTDST20GEOID))
  }) %>%
  bind_rows()

race <- race_easy %>%
  full_join(race_tedious, by = c("variable"="variable", "VTDST20GEOID"="VTDST20GEOID"))%>%
  mutate(total_estimate = coalesce(easy_estimate, 0) + coalesce(tedious_estimate, 0))%>%
  select("VTDST20GEOID", "variable", "total_estimate")

race_wide <- race %>%
  pivot_wider(names_from = variable, values_from = total_estimate)

confrontrace <- race_wide%>%
  left_join(texas, by = c("VTDST20GEOID"="GEOID20"))

#Results for race are a bit different from the one we have in ALARM: that can be explained by lower precision of our estimate
#In any case, we will not use our own estimates for race, so that this section could be possibly dropped

#Race
racedata <- get_acs(geography = "cbg", #it returns estimates with moe (margin of error)
                    state = "Texas",
                    table = "B02001",
                    cache_table = TRUE,
                    year = 2020)

race_easy <- easy_groups %>%
  split(.$VTDST20GEOID)%>%
  lapply(function(df) {
    racedata %>%
      filter(GEOID %in% df$block_group) %>%
      group_by(variable) %>%
      summarise(easy_estimate = sum(estimate, na.rm = TRUE)) %>%
      mutate(VTDST20GEOID = unique(df$VTDST20GEOID))
  }) %>%
  bind_rows()

#Now I can consider block groups which are not entirely contained in VTDs
race_tedious <- differences_weighted %>%
  split(.$VTDST20GEOID)%>%
  lapply(function(df) {
    racedata %>%
      filter(GEOID %in% df$block_group) %>%
      group_by(variable) %>%
      summarise(tedious_estimate = sum(estimate*(df$pop/df$group_pop), na.rm = TRUE)) %>%
      mutate(VTDST20GEOID = unique(df$VTDST20GEOID))
  }) %>%
  bind_rows()

race <- race_easy %>%
  full_join(race_tedious, by = c("variable"="variable", "VTDST20GEOID"="VTDST20GEOID"))%>%
  mutate(total_estimate = coalesce(easy_estimate, 0) + coalesce(tedious_estimate, 0))%>%
  select("VTDST20GEOID", "variable", "total_estimate")

race_wide <- race %>%
  pivot_wider(names_from = variable, values_from = total_estimate)

merging <- race_wide%>%
  left_join(texas, by = c("VTDST20GEOID"="GEOID20"))




saveRDS(race_wide, "final_wide_tx.rds")
saveRDS(merging, "merging.rds")
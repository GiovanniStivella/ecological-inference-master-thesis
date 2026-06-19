#Let's take Texas
texas <- read.table("https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-2020/tx_2020_vtd.csv",
                    header = TRUE,    # first row of file contains variable names
                    sep = ",",        # need to specify that the file is comma-separator
                    dec = ".")        # useless here (as it is the default), can be useful with Italian settings (dec = ",")

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

#Using get_decennial to have population at census block level (I have to check why I had not used get_acs, which would be more coherent)
weights <- get_decennial(
  geography = "block",
  state = "TX",
  variables = "P1_001N",
  year = 2020
)

#There are 219672 blocks without people 

#(https://www.arcgis.com/apps/mapviewer/index.html?layers=2f5e592494d243b0aa5c253e75e792a4)

#Educational attainment for people over 25
prova <- get_acs(geography = "cbg",
                 state = "Texas",
                 table = "B15003",
                 year = 2020)

#There are 465950 observations (instead of 668757), of which 238758 where estimate is equal to 0

block_codes <- read.table("BlockAssign_ST48_TX_VTD.txt",
                          header = TRUE,
                          sep = "|")
#The number of block_codes coincide with the number in weights, which is a good check

block_codes <- block_codes%>%mutate(BLOCKID = as.character(BLOCKID))

#This is to consider how each census block is encoded into an electoral precinct
block_codes <- block_codes %>%
  mutate(VTDST20GEOID = paste0("48", sprintf("%03s", COUNTYFP), DISTRICT))

#That is to consider how each census block is encoded into a block group
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
#For each block consider both which is its VTD and which is its block_group (information contained in block_codes)
differences_and_weights <- block_codes %>%
  filter(block_group %in% differences$block_group)

differences_weighted <- differences_and_weights %>%
  group_by(VTDST20GEOID, block_group) %>%
  summarise (pop = sum(value))
differences_weighted <- differences_weighted %>%
  group_by(block_group) %>%
  mutate(group_pop = sum(pop)) %>%
  ungroup()

easy_groups <- block_groups%>%
  left_join(block_vtds_pair, by=c("block_group"="block_group"))%>%
  filter(n_rows.x == n_rows.y)

auto_easy <- easy_groups %>%
  split(.$VTDST20GEOID)%>%
  lapply(function(df) {
    prova %>%
      filter(GEOID %in% df$block_group) %>%
      group_by(variable) %>%
      summarise(easy_estimate = sum(estimate, na.rm = TRUE)) %>%
      mutate(VTDST20GEOID = unique(df$VTDST20GEOID))
  }) %>%
  bind_rows()

auto_easy <- easy_groups %>%
  split(.$VTDST20GEOID)%>%
  lapply(function(df) {
    prova %>%
      filter(GEOID %in% df$block_group) %>%
      group_by(variable) %>%
      #summarise(easy_estimate = sum(estimate, na.rm = TRUE)) %>%
      mutate(VTDST20GEOID = unique(df$VTDST20GEOID))
  }) %>%
  bind_rows()

auto_tedious <- differences_weighted %>%
  split(.$VTDST20GEOID)%>%
  lapply(function(df) {
    prova %>%
      filter(GEOID %in% df$block_group) %>%
      group_by(variable) %>%
      summarise(tedious_estimate = sum(estimate*(df$pop/df$group_pop), na.rm = TRUE)) %>%
      mutate(VTDST20GEOID = unique(df$VTDST20GEOID))
  }) %>%
  bind_rows()

final <- auto_easy %>%
  full_join(auto_tedious, by = c("variable"="variable", "VTDST20GEOID"="VTDST20GEOID"))%>%
  mutate(total_estimate = coalesce(easy_estimate, 0) + coalesce(tedious_estimate, 0))%>%
  select("VTDST20GEOID", "variable", "total_estimate")

final_wide <- final %>%
  pivot_wider(names_from = variable, values_from = total_estimate)

write.csv(final_wide, "final_wide.csv")
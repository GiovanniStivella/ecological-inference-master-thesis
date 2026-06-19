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

help("get_acs")

c <- load_variables(2020, "pl")   #P1_001N is a total

help("get_decennial")

# Use state FIPS or USPS abbreviation for get_decennial.
# "Texas" can return an HTML error.
weights <- get_decennial(
  geography = "block",
  state = "TX",
  variables = "P1_001N",
  year = 2020
)
#I have verified that, at least when I sum blocks into block groups, the number coincides with the one presented here (https://www.arcgis.com/apps/mapviewer/index.html?layers=2f5e592494d243b0aa5c253e75e792a4)

prova <- get_acs(geography = "cbg",
                 state = "Texas",
                 table = "B15003",
                 year = 2020)

block_codes <- read.table('/Users/giovannistivella/Documents/Università/SSSUP/PSU/Data/Pennsylvania (first try)/BlockAssign_ST42_PA_VTD.txt',              # TXT data file indicated as string or full path to the file
                          header = TRUE,    # Whether to display the header (TRUE) or not (FALSE)
                          sep = "|")        # Separator of the columns of the file

block_codes <- block_codes%>%mutate(BLOCKID = as.character(BLOCKID))
block_codes <- block_codes %>%
  mutate(VTDST20GEOID = paste0("42", sprintf("%03s", COUNTYFP), DISTRICT))

block_codes <- block_codes%>%mutate(block_group = substr(as.character(BLOCKID), 1, 12))

block_codes <- block_codes%>%
  left_join(weights, by=c("BLOCKID"="GEOID"))

block_groups <- block_codes %>%
  group_by(block_group) %>%
  summarise(n_rows = n(), .groups = "drop") #how many blocks are there in each block group

block_vtds_pair <- block_codes %>%
  group_by(block_group, VTDST20GEOID) %>%
  summarise(n_rows = n(), .groups = "drop") #how many blocks of a certain block group are there in each VTD

differences <- block_groups%>%
  left_join(block_vtds_pair, by=c("block_group"="block_group"))%>%
  filter(n_rows.x != n_rows.y)

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
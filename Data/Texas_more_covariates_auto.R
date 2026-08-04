library(tidyverse)
library(tidycensus)

# Texas precinct-level covariates built from block-group ACS data.
# The core idea is to distribute each block-group estimate across VTDs
# using the population share of the blocks that belong to each VTD.

texas <- read.table(
  "https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-2020/tx_2020_vtd.csv",
  header = TRUE,
  sep = ",",
  dec = "."
)

weights <- get_decennial(
  geography = "block",
  state = "TX",
  variables = "P1_001N",
  year = 2020
)

block_codes <- read.table(
  "BlockAssign_ST48_TX_VTD.txt",
  header = TRUE,
  sep = "|",
  colClasses = "character"
)

block_codes <- block_codes %>%
  mutate(
    BLOCKID = as.character(BLOCKID),
    COUNTYFP = str_pad(COUNTYFP, width = 3, side = "left", pad = "0"),
    DISTRICT = as.character(DISTRICT),
    VTDST20GEOID = paste0("48", COUNTYFP, DISTRICT),
    block_group = substr(BLOCKID, 1, 12)
  )

block_codes_with_population <- block_codes %>%
  left_join(
    weights %>% transmute(BLOCKID = GEOID, population = value),
    by = "BLOCKID"
  )

stopifnot(!any(is.na(block_codes_with_population$population)))

block_shares <- block_codes_with_population %>%
  group_by(block_group, VTDST20GEOID) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
  group_by(block_group) %>%
  mutate(
    group_population = sum(population, na.rm = TRUE),
    share = if_else(group_population > 0, population / group_population, 0)
  ) %>%
  ungroup()

aggregate_cbg_to_vtd <- function(acs_table, block_shares) {
  acs_table %>%
    transmute(
      GEOID = as.character(GEOID),
      variable = as.character(variable),
      estimate = as.numeric(estimate)
    ) %>%
    inner_join(
      block_shares %>% select(block_group, VTDST20GEOID, share),
      by = c("GEOID" = "block_group")
    ) %>%
    mutate(weighted_estimate = estimate * share) %>%
    group_by(VTDST20GEOID, variable) %>%
    summarise(total_estimate = sum(weighted_estimate, na.rm = TRUE), .groups = "drop")
}

build_vtd_table <- function(acs_table) {
  aggregate_cbg_to_vtd(acs_table, block_shares) %>%
    pivot_wider(names_from = variable, values_from = total_estimate)
}

merge_with_texas <- function(vtd_table) {
  vtd_table %>%
    left_join(texas, by = c("VTDST20GEOID" = "GEOID20"))
}

acs_tables <- list(
  education = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B15003",
    cache_table = TRUE,
    year = 2020
  ),
  sexage = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B01001",
    cache_table = TRUE,
    year = 2020
  ),
  race = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B02001",
    cache_table = TRUE,
    year = 2020
  )
)

vtd_tables <- lapply(acs_tables, build_vtd_table)
merged_tables <- lapply(vtd_tables, merge_with_texas)

education_wide <- vtd_tables$education
sexage_wide <- vtd_tables$sexage
race_wide <- vtd_tables$race

education_merging <- merged_tables$education
sexage_merging <- merged_tables$sexage
race_merging <- merged_tables$race

saveRDS(education_wide, "education_wide_tx.rds")
saveRDS(sexage_wide, "sexage_wide_tx.rds")
saveRDS(race_wide, "race_wide_tx.rds")
saveRDS(education_merging, "education_merging_tx.rds")
saveRDS(sexage_merging, "sexage_merging_tx.rds")
saveRDS(race_merging, "race_merging_tx.rds")

look <- readRDS("merging.rds")
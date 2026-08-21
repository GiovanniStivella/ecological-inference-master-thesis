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

#Here we can look at which variables to use, bearing in mind that we can only use variables collected at census block group level
acs_variables <- load_variables(2020, "acs5")

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
  mobilitymetro = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B07201",
    cache_table = TRUE,
    year = 2020
  ),
  mobilitymicro = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B07202",
    cache_table = TRUE,
    year = 2020
  ),
  mobilityother = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B07203",
    cache_table = TRUE,
    year = 2020
  ),
  householdtype = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B09019",
    cache_table = TRUE,
    year = 2020
  ),
  language = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B16004",
    cache_table = TRUE,
    year = 2020
  ),
  householdincome = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B19001", #should we use household income or family income (B19101)? B19001 is the sum of family and nonfamily, which means that each household is categorised either as family or as nonfamily
    cache_table = TRUE,
    year = 2020
  ),
  averageincome = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B19301", #we can even have this estimate based on ethnicity
    cache_table = TRUE,
    year = 2020
  ),
  employmentstatus = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B23025",
    cache_table = TRUE,
    year = 2020
  ),
  agevap = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B29001",
    cache_table = TRUE,
    year = 2020
  ),
  educationvap = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B29002",
    cache_table = TRUE,
    year = 2020
  ),
  povertyvap = get_acs(
    geography = "cbg",
    state = "Texas",
    table = "B29003",
    cache_table = TRUE,
    year = 2020
  )
)

vtd_tables <- lapply(acs_tables, build_vtd_table)
merged_tables <- reduce(vtd_tables, ~ left_join(.x, .y, by = "VTDST20GEOID"))
merged_with_texas <- merge_with_texas(merged_tables)

saveRDS(merged_with_texas, "texas_with_covariates.rds")
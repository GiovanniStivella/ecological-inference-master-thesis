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

acs_variables <- load_variables(2020, "acs5") #B02001_001 should be a total

weights <- get_acs(
  geography = "block",
  state = "TX",
  table = "B02001",
  year = 2020
)
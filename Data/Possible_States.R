
library(tidyr)
library(dplyr)

# Load 2020 VTD data from ALARM
texas <- read.table("https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-2020/tx_2020_vtd.csv",
                           header = TRUE,    # first row of file contains variable names
                           sep = ",",        # need to specify that the file is comma-separator
                           dec = ".")        # useless here (as it is the default), can be useful with Italian settings (dec = ",")

pennsylvania <- read.table("https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-2020/pa_2020_vtd.csv",
                    header = TRUE,    # first row of file contains variable names
                    sep = ",",        # need to specify that the file is comma-separator
                    dec = ".")        # useless here (as it is the default), can be useful with Italian settings (dec = ",")

california <- read.table("https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-2020/ca_2020_block.csv",
                           header = TRUE,    # first row of file contains variable names
                           sep = ",",        # need to specify that the file is comma-separator
                           dec = ".")        # useless here (as it is the default), can be useful with Italian settings (dec = ",")

ny <- read.table("https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-2020/ny_2020_vtd.csv",
                         header = TRUE,    # first row of file contains variable names
                         sep = ",",        # need to specify that the file is comma-separator
                         dec = ".")        # useless here (as it is the default), can be useful with Italian settings (dec = ",")

florida <- read.table("https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-2020/fl_2020_vtd.csv",
                 header = TRUE,    # first row of file contains variable names
                 sep = ",",        # need to specify that the file is comma-separator
                 dec = ".")        # useless here (as it is the default), can be useful with Italian settings (dec = ",")
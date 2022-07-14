# Pipelines and Pathways Community Metrics
# Get Cville Region PUMS data
# Updated: 2022-06-14
# Authors: Michele Claibourn
# ..............................................

# Resources ----

# A nice intro to using PUMS in R
# https://walker-data.com/tidycensus/articles/pums-data.html

# Data dictionary for 2016-2020 ACS (also in folder)
# https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2016-2020.pdf

# Proposal outlining goals
# https://docs.google.com/document/d/1FX5uApvSgkbkXeRObXgI1wuEbl5kqeTq36kEFTc4Hxo/edit?usp=sharing


# ..............................................
# 0. Set up ----
library(tidyverse)
library(tidycensus)


# ..............................................
# 1a. Download with tidycensus ----
# # Download VA, 2020, ACS 5 (ACS 1 not available in 2020)
# 
# va_pums5_2020 <- get_pums(
#   state = "VA",
#   puma = c(51089, 50190),
#   survey = "acs5",
#   variables = c("PUMA", "SEX", "AGEP", "SCHL"),
#   year = 2020
# )
# 
# # doesn't work with puma set to multiple; works to download entire state with no pumas specified


# ..............................................
# 1b. Download ACS 2016-2020 PUMS from site ----
# https://www2.census.gov/programs-surveys/acs/data/pums/2020/5-Year/

p_url <- "https://www2.census.gov/programs-surveys/acs/data/pums/2020/5-Year/csv_pva.zip"
h_url <- "https://www2.census.gov/programs-surveys/acs/data/pums/2020/5-Year/csv_hva.zip"

download.file(p_url, "pumsdata/csv_pva.zip")
download.file(h_url, "pumsdata/csv_hva.zip")

unzip("pumsdata/csv_pva.zip", exdir = "pumsdata/")
unzip("pumsdata/csv_hva.zip", exdir = "pumsdata/")


# ..............................................
# 2. Read in data ----
pva <- read_csv("pumsdata/psam_p51.csv")
hva <- read_csv("pumsdata/psam_h51.csv")


# ..............................................
# 3. Initial exploration ----
pva %>% 
  filter(PUMA %in% c("51089", "51090")) %>% 
  count(PUMA, wt = PWGTP)

# Just Cville region
pva_cvl <- pva %>% 
  filter(PUMA %in% c("51089", "51090")) 
hva_cvl <- hva %>% 
  filter(PUMA %in% c("51089", "51090")) 

# Regional Population
sum(pva_cvl$PWGTP)


# ..............................................
# 5. Save ----
saveRDS(pva_cvl, "pumsdata/cville_pums_personal.RDS")
saveRDS(hva_cvl, "pumsdata/cville_pums_household.RDS")

# Manually Compute ADI with domains ----


# packages ----
library(dplyr)
library(tidyr)
library(tidycensus)

# get variable names from sociome ----
sociome::acs_vars %>%
  filter(
    set1==TRUE
  ) %>%
  select(
    variable
  ) %>%
  pull() -> varz

# get data from ACS/Tidycensus----
test <- tidycensus::get_acs(geography = "county",
                            variables = varz, 
                            year = 2019)
# clean and pivot, needed for calculate_adi ----
test %>%
  select(-moe) %>%
  tidyr::pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  ) -> test2

# calculate ADI ----
test3 <- calculate_adi(test2, keep_indicators = T)

# Look at counties with missing data----
## GEOID of counties with missing data----
gid <- pull(test2[!complete.cases(test2),"GEOID"])
## population of counties with missing data----
tidycensus::get_estimates(geography = "county", product = "population") %>%
  filter(
    GEOID %in% gid,
    variable == "POP"
  ) 

### https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2018.html
### vars_all <- load_variables(year = 2019, dataset = "acs5")
### https://api.census.gov/data/2019/acs/acs5/profile/variables.html
### https://api.census.gov/data/2019/acs/acs5/subject/variables.html

library(dplyr)
library(sociome)
library(tidycensus)



x <- dep_internal$request_vars$svi19
x <- x[-5]
x <- unique(unlist(x))

y <- sociome::acs_vars
y <- dplyr::filter(y, set1 == TRUE) %>% pull(variable)

df <- get_acs(year = 2020, geography = "county", variables = y, state = "MO", output = "wide")
df <- select(df, GEOID, NAME, ends_with("E"))

x <- names(df)
x <- gsub("E$", "", x)
x <- ifelse(x == "NAM", "NAME", x)
names(df) <- x

rm(x)

df <- calculate_adi(df)

# DP02_0001 and DP04_0076 are equivalent


devtools::load_all()

dep_build_varlist(geography = "county", variables = "svi", year = 2020, survey = "acs5", output = "vector")


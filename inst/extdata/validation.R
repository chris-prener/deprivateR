# Validation of deprivateR Against CDC 2018 County-level Data

# dependencies ####
devtools::load_all()

library(dplyr)
library(readr)

# load data ####
## pull data with deprivateR
dep_18 <- dep_get_index(geography = "county", index = "svi", year = 2018, territory = NULL,
                        svi_round = FALSE, keep_subscales = TRUE)

## load CDC data for comparison
cdc_18 <- read_csv("data-raw/cdc_svi_county_2018.csv") %>%
  #select(STATE, ST_ABBR, COUNTY, FIPS, RPL_THEME1,
  #       RPL_THEME2, RPL_THEME3,
  #       RPL_THEME4, RPL_THEMES) %>%
  #mutate(RPL_THEME1 = RPL_THEME1*100,
  #       RPL_THEME2 = RPL_THEME2*100,
  #       RPL_THEME3 = RPL_THEME3*100,
  #       RPL_THEME4 = RPL_THEME4*100,
  #       RPL_THEMES = RPL_THEMES*100) %>%
  mutate(across(where(is.numeric), ~ifelse(.x == -999, NA, .x))) %>%
  arrange(FIPS)

## primary
all(cdc_18$E_TOTPOP == dep_18$E_TOTPOP)
all(cdc_18$E_HU == dep_18$E_HU)
all(cdc_18$E_HH == dep_18$E_HH)

## ses
all(cdc_18$E_POV == dep_18$E_POV, na.rm = TRUE)
all(cdc_18$EP_POV == dep_18$EP_POV, na.rm = TRUE)
all(cdc_18$EPL_POV == dep_18$EPL_POV, na.rm = TRUE)
all(cdc_18$E_UNEMP == dep_18$E_UNEMP, na.rm = TRUE)
all(cdc_18$EP_UNEMP == dep_18$EP_UNEMP, na.rm = TRUE)
all(cdc_18$EPL_UNEMP == dep_18$EPL_UNEMP, na.rm = TRUE)
all(cdc_18$E_PCI == dep_18$E_PCI, na.rm = TRUE)
all(cdc_18$EPL_PCI == dep_18$EPL_PCI, na.rm = TRUE)
all(cdc_18$E_NOHSDP == dep_18$E_NOHSDP, na.rm = TRUE)
all(cdc_18$EP_NOHSDP == dep_18$EP_NOHSDP, na.rm = TRUE)
all(cdc_18$EPL_NOHSDP == dep_18$EPL_NOHSDP, na.rm = TRUE)

## hcd
all(cdc_18$E_AGE17 == dep_18$E_AGE17, na.rm = TRUE)
all(cdc_18$EP_AGE17 == dep_18$EP_AGE17, na.rm = TRUE)
all(cdc_18$E_AGE65 == dep_18$E_AGE65, na.rm = TRUE)
all(cdc_18$EP_AGE65 == dep_18$EP_AGE65, na.rm = TRUE)
all(cdc_18$E_DISABL == dep_18$E_DISABL, na.rm = TRUE)
all(cdc_18$EP_DISABL == dep_18$EP_DISABL, na.rm = TRUE)
all(cdc_18$E_SNGPNT == dep_18$E_SNGPNT, na.rm = TRUE)
all(cdc_18$EP_SNGPNT == dep_18$EP_SNGPNT, na.rm = TRUE)

# msl
all(cdc_18$E_LIMENG == dep_18$E_LIMENG, na.rm = TRUE)
all(cdc_18$EP_LIMENG == dep_18$EP_LIMENG, na.rm = TRUE)
all(cdc_18$E_MINRTY == dep_18$E_MINRTY, na.rm = TRUE)
all(cdc_18$EP_MINRTY == dep_18$EP_MINRTY, na.rm = TRUE) # false - one value doesn't match b/c of R rounding

# htt
all(cdc_18$E_MUNIT == dep_18$E_MUNIT, na.rm = TRUE)
all(cdc_18$EP_MUNIT == dep_18$EP_MUNIT, na.rm = TRUE) # false - one value doesn't match b/c of R rounding
all(cdc_18$E_MOBILE == dep_18$E_MOBILE, na.rm = TRUE)
all(cdc_18$EP_MOBILE == dep_18$EP_MOBILE, na.rm = TRUE)
all(cdc_18$E_CROWD == dep_18$E_CROWD, na.rm = TRUE)
all(cdc_18$EP_CROWD == dep_18$EP_CROWD, na.rm = TRUE)
all(cdc_18$E_NOVEH == dep_18$E_NOVEH, na.rm = TRUE)
all(cdc_18$EP_NOVEH == dep_18$EP_NOVEH, na.rm = TRUE)
all(cdc_18$E_GROUPQ == dep_18$E_GROUPQ, na.rm = TRUE)
all(cdc_18$EP_GROUPQ == dep_18$EP_GROUPQ, na.rm = TRUE)


x <- select(cdc_18, GEOID = FIPS, EP_POV_cdc = EP_POV)
x1 <- select(dep_18, GEOID, EP_POV_dep = EP_POV)
x <- left_join(x, x1, by = "GEOID")

x <- select(cdc_18, GEOID = FIPS, SVI_cdc = RPL_THEMES)
x1 <- select(dep_18, GEOID, SVI_dep = SVI)
x <- left_join(x, x1, by = "GEOID")


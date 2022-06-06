# create vectors of variables for measures ####

## Gini ####
gini10 <- "B19083"

## SVI ####
### primary variables to download
pri_vars <- c("S0601_C01_001", # total population
              "DP04_0001", # total housing units
              "DP02_0001") # total households

### theme 1 - socioeconomic status
ses_vars <- c("B17001_001", "B17001_002", # poverty status
              "DP03_0001", "DP03_0005", # employment status
              "B06011_001", # median income
              "B19301_001", # per capita income
              "B06009_001", "B06009_002") # less than a HS education

### theme 2 - household composition and disability
hhd_vars <- c("S0101_C01_001", "S0101_C01_030", "S0101_C01_022", # selected age categories
              "DP02_0071", "DP02_0072", # civilian non institutionalized population with disability
              "DP02_0001", "DP02_0007", "DP02_0011") # single parent households

### theme 3 - minority status and language
msl_vars <- c("S0601_C01_001", # total population
              "B01001H_001", # white, non-hispanic population
              "B16004_001")
eng_table <- "B16004"
eng_vars <- c("B16004_007", "B16004_008", "B16004_012", "B16004_013", "B16004_017", "B16004_018", "B16004_022", "B16004_023",
              "B16004_029", "B16004_030", "B16004_034", "B16004_035", "B16004_039", "B16004_040", "B16004_044", "B16004_045",
              "B16004_051", "B16004_052", "B16004_056", "B16004_057", "B16004_061", "B16004_062", "B16004_066", "B16004_067")

### theme 4 - housing type and transportation
htt_vars <- c("DP04_0006", # total housing units for structure type
              "DP04_0012", "DP04_0013", # 10 or more units
              "DP04_0014", # mobile home units
              "DP04_0076", "DP04_0078", "DP04_0079", # crowding
              "DP04_0057", "DP04_0058", # vehicles available
              "B26001_001", # group quarters population
              "DP04_0001", # total housing units
              "S0601_C01_001E", # total population
              "DP04_0002E") # occupied housing units

### combine
svi19 <- list(
  pri_vars = pri_vars,
  ses_vars = ses_vars,
  hhd_vars = hhd_vars,
  msl_vars = msl_vars,
  eng_table = eng_table,
  eng_vars = eng_vars,
  htt_vars = htt_vars
)

rm(pri_vars, ses_vars, hhd_vars, msl_vars, eng_table, eng_vars, htt_vars)

## Extras ####
extras19 <- c("B13015_001", "B13015_002", # recent births
               "DP03_0051", # income and benefits households
               "DP03_0066", # households with social security
               "DP03_0070", # households with SSI
               "DP03_0072", # households with TANF
               "DP03_0074", # households with SNAP
               "DP03_0095", # civilian non institutionalized population
               "DP03_0098", # public health insurance coverage
               "DP03_0099") # no health insurance coverage

## Put Data Together ####
request_vars <- list(
  gini10 = gini10,
  svi19 = svi19,
  extras19 = extras19
)

rm(gini10, svi19, extras19)

# Create Test Data ####
## Dependencies ####
library(tidycensus)
library(sociome)

## Pull Gini Data ####
test_gini_df_wide <- get_acs(geography = "county", table = request_vars$gini1,
                        output = "wide", year = 2019, state = 29,
                        geometry = FALSE, keep_geo_vars = FALSE)

test_gini_df_tidy <- get_acs(geography = "county", table = request_vars$gini1,
                             output = "tidy", year = 2019, state = 29,
                             geometry = FALSE, keep_geo_vars = FALSE)

test_gini_sf <- get_acs(geography = "county", table = request_vars$gini1,
                        output = "wide", year = 2019, state = 29,
                        geometry = TRUE, keep_geo_vars = FALSE)

test_gini_sf_geo <- get_acs(geography = "county", table = request_vars$gini1,
                        output = "wide", year = 2019, state = 29,
                        geometry = TRUE, keep_geo_vars = TRUE)

test_gini <- list(
  test_gini_df_wide = test_gini_df_wide,
  test_gini_df_tidy = test_gini_df_tidy,
  test_gini_sf = test_gini_sf,
  test_gini_sf_geo = test_gini_sf_geo
)

rm(test_gini_df_wide, test_gini_df_tidy, test_gini_sf, test_gini_sf_geo)

## Pull SVI Data ####
test_svi_pri <- get_acs(geography = "county", state = 29,
                        variables = request_vars$svi19$pri_vars,
                        output = "wide", year = 2019)

test_svi_ses <- get_acs(geography = "county", state = 29,
                        variables = request_vars$svi19$ses_vars,
                        output = "wide", year = 2019)

test_svi_hhd <- get_acs(geography = "county", state = 29,
                        variables = request_vars$svi19$hhd_vars,
                        output = "wide", year = 2019)

test_svi_msl <- get_acs(geography = "county", state = 29,
                        variables = request_vars$svi19$msl_vars,
                        output = "wide", year = 2019)

test_svi_eng <- get_acs(geography = "county", state = 29,
                        table = request_vars$svi19$eng_table,
                        output = "tidy", year = 2019)

test_svi_htt <- get_acs(geography = "county", state = 29,
                        variables = request_vars$svi19$htt_vars,
                        output = "wide", year = 2019)

test_svi <- list(
  test_svi_pri = test_svi_pri,
  test_svi_ses = test_svi_ses,
  test_svi_hhd = test_svi_hhd,
  test_svi_msl = test_svi_msl,
  test_svi_eng = test_svi_eng,
  test_svi_htt = test_svi_htt
)

rm(test_svi_pri, test_svi_ses, test_svi_hhd, test_svi_msl, test_svi_eng, test_svi_htt)

## Pull ADI Data ####
test_adi <- get_adi(geography = "county", state = "MO", year = 2019, dataset = "acs5")

# Create Internal Data Object ####
dep_internal <- list(
  request_vars = request_vars,
  test_data = list(
    test_gini = test_gini,
    test_svi = test_svi,
    test_adi = test_adi
  )
)

rm(request_vars, test_gini, test_svi)

# Save Internal Data Object ####
save(dep_internal, file = "R/sysdata.rda", version = 2, compress = "xz")

# create vectors of variables for measures ####

## Gini ####
gini10 <- "B19083_001"

## SVI ####
### primary variables to download
pri_vars <- c("S0601_C01_001", # total population
              "DP04_0001", # total housing units
              "DP02_0001") # total households (equivalent to B11005_001?)

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
eng_vars <- c("B16004_007", "B16004_008", "B16004_012", "B16004_013", "B16004_017", "B16004_018", "B16004_022", "B16004_023",
              "B16004_029", "B16004_030", "B16004_034", "B16004_035", "B16004_039", "B16004_040", "B16004_044", "B16004_045",
              "B16004_051", "B16004_052", "B16004_056", "B16004_057", "B16004_061", "B16004_062", "B16004_066", "B16004_067")

### theme 4 - housing type and transportation
htt_vars <- c("DP02_0001", # total households
              "DP04_0001", # total housing units
              "DP04_0002", # occupied housing units
              "DP04_0006", # total housing units for structure type
              "DP04_0012", "DP04_0013", # 10 or more units
              "DP04_0014", # mobile home units
              "DP04_0078", "DP04_0079", # crowding - switched out "DP04_0076" for DP02_0001
              "DP04_0057", "DP04_0058", # vehicles available
              "B26001_001", # group quarters population
              "S0601_C01_001" # total population
              )

### combine
svi19 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(ses_vars),
  hhd_vars = sort(hhd_vars),
  msl_vars = sort(msl_vars),
  eng_vars = sort(eng_vars),
  htt_vars = sort(htt_vars)
)

rm(pri_vars, ses_vars, hhd_vars, msl_vars, eng_vars, htt_vars)

## Put Data Together ####
request_vars <- list(
  gini10 = gini10,
  svi19 = svi19
)

rm(gini10, svi19)

# States Vector ####
library(tigris)

states <- states(cb = TRUE)
states <- sort(states$STUSPS)

# Save Internal Data Object ####
save(request_vars, states, file = "R/sysdata.rda", version = 2, compress = "xz")

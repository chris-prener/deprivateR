# create vectors of variables for measures ####

## Gini ####
gini10 <- "B19083_001"

## SVI ####
### primary variables to download
pri_vars <- c("DP02_0001", # total households (2018, 2019, 2020)
              "DP04_0001", # total housing units (2018, 2019, 2020)
              "S0601_C01_001") # total population (2018, 2019, 2020)

# DP02_0001 (equivalent to B11005_001?)

### theme 1 - socioeconomic status
ses_vars <- c("B06009_001", # place of birth by educational attainment, total (2018, 2019, 2020)
              "B06009_002", # place of birth by educational attainment, less than a high school education (2018, 2019, 2020)
              "B17001_001", # poverty status in the last 12 months, total (2018, 2019, 2020)
              "B17001_002", # poverty status in the last 12 months, below poverty level (2018, 2019, 2020)
              "B19301_001", # per capita income in inflation-adjusted dollars (per year of call) (2018, 2019, 2020)
              "DP03_0005", # unemployed (>= 16 civilians) (2018, 2019, 2020)
              "DP03_0009P") # unemployment rate (>= 16 civilians) (2018, 2019, 2020)

### theme 2 - household composition and disability
hhd_vars18 <- c("DP02_0001", # total households, confirmed in pri_vars
                "DP02_0007", # male householder, no spouse, with children < 18 (2018)
                "DP02_0009", # female householder, no spouse, with children < 18 (2018)
                "DP02_0070", # total civilian noninstitutionalized population (2018)
                "DP02_0071", # total civilian noninstitutionalized population with disability (2018)
                "S0101_C01_001", # total population, confirmed in pri_vars
                "S0101_C01_022", # population by age (< 18 years) (2018, 2019, 2020)
                "S0101_C01_030") # population by age (>= 65 years) (2018, 2019, 2020)

hhd_vars19 <- c("B11012_001", # total households (2019, 2020)
                "B11012_010", # female householder, no spouse, with children < 18 (2019, 2020)
                "B11012_015", # male householder, no spouse, with children < 18 (2019, 2020)
                "DP02_0071", # total civilian noninstitutionalized population (2019, 2022)
                "DP02_0072", # total civilian noninstitutionalized population with disability (2019, 2022)
                "S0101_C01_001", # total population, confirmed in pri_vars
                "S0101_C01_022", # population by age (< 18 years) (2018, 2019, 2020)
                "S0101_C01_030") # population by age (>= 65 years) (2018, 2019, 2020)

### theme 3 - minority status and language
#### core variables
msl_vars <- c("B01001H_001", # sex by age, white, non-hispanic (2018, 2019, 2020)
              "B16004_001", # age by language spoken at home by ability to speak english (>= 5 years) (2018, 2019, 2020)
              "S0601_C01_001") # total population, confirmed in pri_vars)

#### english langauge variables (2018, 2019, 2020)
eng_vars <- c("B16004_007", # Spanish speaker, speaks English not well (5-17 years)
              "B16004_008", # Spanish speaker, speaks English not at all (5-17 years)
              "B16004_012", # Speaks other Indo-European languages, speaks English not well (5-17 years)
              "B16004_013", # Speaks other Indo-European languages, speaks English not at all (5-17 years)
              "B16004_017", # Speaks Asian and Pacific Islander languages, speaks English not well (5-17 years)
              "B16004_018", # Speaks Asian and Pacific Islander languages, speaks English not at all (5-17 years)
              "B16004_022", # Speaks other languages, speaks English not well (5-17 years)
              "B16004_023", # Speaks other languages, speaks English not at all (5-17 years)
              "B16004_029", # Spanish speaker, speaks English not well (18-64 years)
              "B16004_030", # Spanish speaker, speaks English not at all (18-64 years)
              "B16004_034", # Speaks other Indo-European languages, speaks English not well (18-64 years)
              "B16004_035", # Speaks other Indo-European languages, speaks English not at all (18-64 years)
              "B16004_039", # Speaks Asian and Pacific Islander languages, speaks English not well (18-64 years)
              "B16004_040", # Speaks Asian and Pacific Islander languages, speaks English not at all (18-64 years)
              "B16004_044", # Speaks other languages, speaks English not well (18-64 years)
              "B16004_045", # Speaks other languages, speaks English not at all (18-64 years)
              "B16004_051", # Spanish speaker, speaks English not well (>= 65 years)
              "B16004_052", # Spanish speaker, speaks English not at all (>= 65 years)
              "B16004_056", # Speaks other Indo-European languages, speaks English not well (>= 65 years)
              "B16004_057", # Speaks other Indo-European languages, speaks English not at all (>= 65 years)
              "B16004_061", # Speaks Asian and Pacific Islander languages, speaks English not well (>= 65 years)
              "B16004_062", # Speaks Asian and Pacific Islander languages, speaks English not at all (>= 65 years)
              "B16004_066", # Speaks other languages, speaks English not well (>= 65 years)
              "B16004_067") # Speaks other languages, speaks English not at all (>= 65 years)

### theme 4 - housing type and transportation
htt_vars <- c("B26001_001", # group quarters population (2018, 2019, 2020)
              "DP02_0001", # total households, confirmed in pri_vars
              "DP04_0001", # total housing units, confirmed in pri_vars
              "DP04_0002", # total occupied housing units (2018, 2019, 2020)
              "DP04_0006", # total housing units in structure (2018, 2019, 2020)
              "DP04_0012", # 10 to 19 units in structure (2018, 2019, 2020)
              "DP04_0013", # 20 or more units in structure (2018, 2019, 2020)
              "DP04_0014", # mobile home units (2018, 2019, 2020)
              "DP04_0057", # total occupied housing units (2018, 2019, 2020)
              "DP04_0058", # no vehicles available, occupied housing units (2018, 2019, 2020)
              "DP04_0076", # total occupied housing units (2018, 2019, 2020)
              "DP04_0078", # occupants per room (1.01 to 1.5) (2018, 2019, 2020)
              "DP04_0079", # occupants per room (>= 1.51) (2018, 2019, 2020)
              "S0601_C01_001") # total population, confirmed in pri_vars

# are DP04_0076 and DP04_0057 equivalent?

### combine
svi18 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(ses_vars),
  hhd_vars = sort(hhd_vars18),
  msl_vars = sort(msl_vars),
  eng_vars = sort(eng_vars),
  htt_vars = sort(htt_vars)
)

svi19 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(ses_vars),
  hhd_vars = sort(hhd_vars19),
  msl_vars = sort(msl_vars),
  eng_vars = sort(eng_vars),
  htt_vars = sort(htt_vars)
)

rm(pri_vars, ses_vars, hhd_vars18, hhd_vars19, msl_vars, eng_vars, htt_vars)

## Put Data Together ####
request_vars <- list(
  gini10 = gini10,
  svi18 = svi18,
  svi19 = svi19
)

rm(gini10, svi18, svi19)

# States Vector ####
library(tigris)

states <- states(cb = TRUE)
states <- sort(states$STUSPS)

# Save Internal Data Object ####
save(request_vars, states, file = "R/sysdata.rda", version = 2, compress = "xz")

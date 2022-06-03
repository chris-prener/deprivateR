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
              "B16004_001", # age by language spoken at home
              "B02001_001", "B02001_002", "B02001_003", "B02001_004", # racial identity
              "B02001_005", "B02001_006", "B02001_007", "B02001_008",
              "B03003_001", "B03003_003") # hispanic/latino origin

eng_vars <- c("B16004_007", "B16004_008", "B16004_012", "B16004_013", "B16004_017", "B16004_018", "B16004_022", "B16004_023",
              "B16004_029", "B16004_030", "B16004_034", "B16004_035", "B16004_039", "B16004_040", "B16004_044", "B16004_045",
              "B16004_051", "B16004_052", "B16004_056", "B16004_057", "B16004_061", "B16004_062", "B16004_066", "B16004_067")

### theme 4 - housing type and transportation
htt_vars <- c("DP04_0006", # total housing units for structure type
              "DP04_0012", "DP04_0013", # 10 or more units
              "DP04_0014", # mobile home units
              "DP04_0003", # vacant housing units
              "DP04_0045", # occupied housing units
              "DP04_0047", # renter occupied housing units
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
  eng_vars = eng_vars,
  htt_vars = htt_vars
)

rm(pri_vars, ses_vars, hhd_vars, msl_vars, eng_vars, htt_vars)

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

## Save Internal Data Object ####
save(request_vars, file = "R/sysdata.rda", version = 2)

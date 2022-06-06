devtools::load_all()

# gini
get_deprivation(geography = "county", variables = "gini", state = "MO", year = 2019, debug = TRUE)

# svi
get_deprivation(geography = "county", variables = "svi", state = "MO", year = 2019, debug = TRUE)

# gini and SVI
get_deprivation(geography = "county", variables = c("gini", "svi"), state = "MO", year = 2019, debug = TRUE)

# geometry
get_deprivation(geography = "county", variables = "svi", state = "MO", year = 2019, output = "wide", geometry = TRUE, debug = TRUE)
get_deprivation(geography = "county", variables = c("gini", "svi"), state = "MO", year = 2019, output = "wide", geometry = TRUE, debug = TRUE)

get_adi(geography = "county", state = "MO", year = 2019, dataset = "acs5")
get_adi(geography = "county", state = c("IL", "MO"), year = 2020, dataset = "acs5")

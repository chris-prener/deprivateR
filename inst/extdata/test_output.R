devtools::load_all()

# gini
get_deprivation(geography = "county", variables = "gini", output = "tidy", state = "MO", year = 2019, debug = "test")

# svi
get_deprivation(geography = "county", variables = "svi", output = "tidy", state = "MO", year = 2019, debug = "test")

# adi
get_deprivation(geography = "county", variables = "adi", output = "tidy", state = "MO", year = 2019, debug = "test")

# adi3
get_deprivation(geography = "county", variables = "adi3", output = "tidy", state = "MO", year = 2019, debug = "test")

# gini and SVI
get_deprivation(geography = "county", variables = c("gini", "svi"), output = "wide", state = "MO", year = 2019,
                keep_subscales = TRUE, debug = "test")

# gini and adi
get_deprivation(geography = "county", variables = c("gini", "adi"), output = "wide", state = "MO", year = 2019,
                keep_subscales = TRUE, debug = "test")

# gini, svi, adi
get_deprivation(geography = "county", variables = c("gini", "svi", "adi"), output = "wide", state = "MO", year = 2019,
                keep_subscales = FALSE, debug = "test")

# gini, svi, adi, and adi3
get_deprivation(geography = "county", variables = c("gini", "svi", "adi", "adi3"),
                output = "wide", state = "MO", year = 2019,
                keep_subscales = FALSE, debug = "test")

# geometry
get_deprivation(geography = "county", variables = "svi", state = "MO", year = 2019, output = "wide", geometry = TRUE, debug = "test")
get_deprivation(geography = "county", variables = c("gini", "svi"), state = "MO", year = 2019, output = "wide", geometry = TRUE, debug = "test")

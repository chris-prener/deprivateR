# Get Gini coefficients
get_gini <- function(geography, output, year, state, county,
                     geometry = FALSE, keep_geo_vars = FALSE, shift_geo = FALSE){

  # call tidycensus
  out <- tidycensus::get_acs(geography = geography, table = request_vars$gini10,
                             year = year, state = state, county = county,
                             geometry = geometry, keep_geo_vars = keep_geo_vars,
                             shift_geo = FALSE)

  # structure output
  if (output == "tidy"){
    out$variable <- "gini"
  } else if (output == "wide"){
    names(out)[names(out) == "B19083_001E"] <- "gini_e"
    names(out)[names(out) == "B19083_001M"] <- "gini_m"
  }

  # return output
  return(out)

}

# Get SVI

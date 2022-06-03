#' Calculate Deprivation Measures
#'
#' @description Calculates various measures of deprivation.
#'
#' @usage get_deprivation(geography, variables, keep_subscales = FALSE,
#'     keep_components = FALSE, output = "tidy", year, state = NULL,
#'     county = NULL, geometry = FALSE, keep_geo_vars = FALSE, shift_geo = FALSE)
#'
#'
#' @export
get_deprivation <- function(geography, variables,
                            keep_subscales = FALSE, keep_components = FALSE,
                            output = "tidy", year, state = NULL, county = NULL,
                            geometry = FALSE, keep_geo_vars = FALSE, shift_geo = FALSE){

  # global bindings
  # x = NULL

  # check inputs
  if (missing(geography)) {
    stop("A level of geography must be provided. Please choose one of: 'state', 'county', or 'tract'.")
  }

  if (geography %in% c("state", "county", "tract") == FALSE){
    stop("Invalid level of geography provided. Please choose one of: 'state', 'county', or 'tract'.")
  }

  # call subfunctions
  if ("gini" %in% variables == TRUE){
    out <- get_gini(geography = geography, output = output, year = year, state = state,
                    county = county)
  }

  # return output
  return(out)

}


# Get Gini coefficients
get_gini <- function(geography, output, year, state, county){

  # call tidycensus
  out <- tidycensus::get_acs(geography = geography, table = "B19083",
                             year = year, state = state, county = county)

  # structure output
  if (output == "tidy"){
    out$variable <- "gini"
  } else if (output == "wide"){
    names(my_data)[names(my_data) == "B19083_001E"] <- "gini_e"
    names(my_data)[names(my_data) == "B19083_001M"] <- "gini_m"
  }

  # return output
  return(out)

}

# Get SVI




#' Calculate Deprivation Measures
#'
#' @description Calculates various measures of deprivation.
#'
#' @usage get_deprivation(geography, variables, keep_subscales = FALSE,
#'     keep_components = FALSE, output = "tidy", year, state = NULL,
#'     county = NULL, geometry = FALSE, keep_geo_vars = FALSE, shift_geo = FALSE)
#'
#' @param geography A character scalar; one of \code{"state"}, \code{"county"}, or
#'     \code{"tract"}
#' @param variables A character scalar or vector listing deprivation measures
#'     to return. These include the gini coefficient (\code{gini})...
#' @param keep_subscales A logical scalar; if \code{FALSE} (defualt), subscales
#'     for SVI, SVIp, and CCVI will not be returned. If \code{TRUE}, these components
#'     will be returned.
#' @param keep_components A logical salar; if \code{FALSE} (defualt), only the
#'     main output measures will be returned. If \code{TRUE}, all individual
#'     Census variables for each requested measure of deprivation will be
#'     returned. Be aware that this can create very large output tables.
#' @param output A character scalar; one of \code{"tidy"} (long output) or
#'     \code{"wide"} depending on the type of data format you want.
#' @param year A numeric between 2010 and 2020
#' @param state A state name or FIPS code (may be either character or numeric
#'     as well as scalar or vector)
#' @param county A county FIPS code or vector of FIPS codes
#' @param geometry A logical salar; if \code{FALSE} (defualt), tabular data are
#'     returned. If \code{TRUE}, a \code{sf} class object will be returned.
#'     This will only work when \code{output = "wide"}.
#' @param keep_geo_vars A logical salar; if \code{FALSE} (defualt), a minimum
#'     number of spatial variables are returned. If \code{TRUE}, a full set of
#'     TIGER/Line variables are returned. This is only relevant if
#'     \code{geometry = TRUE}.
#' @param shift_geo A logical salar; if \code{FALSE} (defualt), Alaska and
#'     Hawaii will appear in their cartographically correct locations. If
#'     \code{TRUE}, Alaska and Hawaii will be shifted south to facilitate
#'     more straightforward national mapping.
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




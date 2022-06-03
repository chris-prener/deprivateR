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
#' @param shift_geo A logical scalar; if \code{FALSE} (defualt), Alaska and
#'     Hawaii will appear in their cartographically correct locations. If
#'     \code{TRUE}, Alaska and Hawaii will be shifted south to facilitate
#'     more straightforward national mapping.
#' @param units A character scalar; either \code{"mi"} (miles) or \code{"km"}
#'     (kilometers)
#'
#' @export
get_deprivation <- function(geography, variables,
                            keep_subscales = FALSE, keep_components = FALSE,
                            output = "tidy", year, state = NULL, county = NULL,
                            geometry = FALSE, keep_geo_vars = FALSE, shift_geo = FALSE,
                            units){

  # global bindings
  # x = NULL

  # check inputs
  if (missing(geography)) {
    stop("A level of geography must be provided. Please choose one of: 'state', 'county', or 'tract'.")
  }

  if (geography %in% c("state", "county", "tract") == FALSE){
    stop("Invalid level of geography provided. Please choose one of: 'state', 'county', or 'tract'.")
  }

  # optionally add geometry, otherwise only return gini if requested
  if (geometry == TRUE){

    ## download data
    out_gini <- get_gini(geography = geography, output = output, year = year, state = state,
                         county = county, geometry = TRUE,
                         keep_geo_vars = keep_geo_vars, shift_geo = shift_geo)

    ## remove gini variables if necessary
    if ("gini" %in% variables == FALSE){
      if (output == "tidy"){
        out_gini <- subset(out_gini, select = -c(variable, estimate, moe))
      } else if (output == "wide"){
        out_gini <- subset(out_gini, select = -c(gini_e, gini_m))
      }
    }

    ## tidy up geo vars
    if (keep_geo_vars == TRUE){

      ### land and water area
      if (units == "mi"){
        out_gini$ALAND_SQMI <- measurements::conv_unit(out_gini$ALAND, from = "m2", to = "mi2")
        out_gini$AWATER_SQMI <- measurements::conv_unit(out_gini$AWATER, from = "m2", to = "mi2")
      } else if (units == "km"){
        out_gini$ALAND_SQKM <- measurements::conv_unit(out_gini$ALAND, from = "m2", to = "km2")
        out_gini$AWATER_SQKM <- measurements::conv_unit(out_gini$AWATER, from = "m2", to = "km2")
      }

      out_gini <- subset(out_gini, select = -c(ALAND, AWATER))

      ### fix variable names
      if (geography == "state"){
        names(out_gini)[names(out_gini) == "NAME.x"] <- "NAME"
        out_gini <- subset(out_gini, select = -NAME.y)
      }

      ### move geometry to the end
      names <- names(out_gini)
      names <- names[! names %in% "geometry"]
      names <- c(names, "geometry")
      out_gini <- out_gini[, names]

    }

  } else if (geometry == FALSE & "gini" %in% variables == TRUE){
    out_gini <- get_gini(geography = geography, output = output, year = year, state = state,
                         county = county)
  }

  # if ("svi" %in% variables == TRUE)

  # combine output
  out <- out_gini

  # return output
  return(out)

}

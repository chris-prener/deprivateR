#' Calculate Deprivation Measures
#'
#' @description Calculates various measures of deprivation.
#'
#' @usage get_deprivation(geography, variables, keep_subscales = FALSE,
#'     keep_components = FALSE, output=, year, state = NULL,
#'     county = NULL, geometry = FALSE, keep_geo_vars = FALSE, shift_geo = FALSE,
#'     units, debug = NULL)
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
#' @param units A character scalar; either \code{"mi"} (default; miles) or
#'     \code{"km"} (kilometers)
#' @param debug Additional parameters for debugging and testing
#'     \code{deprivateR}. The two styles are \code{"messages"} (will print
#'     all messages from \code{tidycensus} and \code{sociome}) and
#'     \code{"test"} (will use built-in data instead of making live API
#'     calls).
#'
#' @export
get_deprivation <- function(geography, variables,
                            keep_subscales = FALSE, keep_components = FALSE,
                            output, year, state = NULL, county = NULL,
                            geometry = FALSE, keep_geo_vars = FALSE, shift_geo = FALSE,
                            units, debug = NULL){

  # global bindings
  ALAND = AWATER = GEOID = E_GINI = M_GINI = NAME = NAME.x = NAME.y = NULL

  # check inputs
  if (missing(geography)) {
    stop("A level of geography must be provided. Please choose one of: 'state', 'county', or 'tract'.")
  }

  if (geography %in% c("state", "county", "tract") == FALSE){
    stop("Invalid level of geography provided. Please choose one of: 'state', 'county', or 'tract'.")
  }

  if (is.null(debug)){
    debug <- "live"
  }

  # optionally add geometry, otherwise only return gini if requested
  if (geometry == TRUE){

    ## check output
    if (output == "tidy"){
      stop("Please set 'output' to 'wide' if you would like geometric data.")
    }

    ## download data
    out <- get_gini(geography = geography, output = output, year = year,
                         state = state, county = county, geometry = TRUE,
                         keep_geo_vars = keep_geo_vars, shift_geo = shift_geo,
                         debug = debug)

    ## remove gini variables if necessary
    if ("gini" %in% variables == FALSE){
      out <- subset(out, select = -c(E_GINI, M_GINI))
    }

    ## tidy up geo vars
    if (keep_geo_vars == TRUE){

      ### land and water area
      if (units == "mi"){
        out$ALAND_SQMI <- measurements::conv_unit(out$ALAND, from = "m2", to = "mi2")
        out$AWATER_SQMI <- measurements::conv_unit(out$AWATER, from = "m2", to = "mi2")
      } else if (units == "km"){
        out$ALAND_SQKM <- measurements::conv_unit(out$ALAND, from = "m2", to = "km")
        out$AWATER_SQKM <- measurements::conv_unit(out$AWATER, from = "m2", to = "km")
      }

      out <- subset(out, select = -c(ALAND, AWATER))

      ### fix variable names
      if (geography == "state"){
        names(out)[names(out) == "NAME.x"] <- "NAME"
        out <- subset(out, select = -NAME.y)
      } else if (geography == "county"){
        names(out)[names(out) == "NAME.y"] <- "NAME"
        out <- subset(out, select = -NAME.x)
      }

    }

  } else if (geometry == FALSE & "gini" %in% variables == TRUE){
    out <- get_gini(geography = geography, output = output, year = year,
                         state = state, county = county, debug = debug)
  }

  ## get SVI scores
  if ("svi" %in% variables == TRUE){
    ## pull data
    out_svi <- get_svi(geography = geography, output = output, year = year,
                       state = state, county = county,
                       keep_subscales = keep_subscales,
                       keep_components = keep_components, debug = debug)

    ## combine
    if (geometry == TRUE){
      out_svi <- subset(out_svi, select = -NAME)
      out <- merge(x = out, y = out_svi, by = "GEOID", all.x = TRUE)
    } else if (geometry == FALSE & "gini" %in% variables == TRUE){
      if (output == "tidy"){
        out_svi$moe <- NA
        out <- rbind(out, out_svi)
      } else if (output == "wide"){
        out_svi <- subset(out_svi, select = -NAME)
        out <- merge(x = out, y = out_svi, by = "GEOID", all.x = TRUE)
      }
    } else {
      out <- out_svi
    }
  }

  ## get ADI scores
  if ("adi" %in% variables == TRUE){
    ## pull data
    out_adi <- get_adi(geography = geography, output = output, year = year,
                       state = state, county = county, debug = debug)

    ## combine
    prior_vars <- "gini" %in% variables == TRUE | "svi" %in% variables == TRUE

    if (geometry == TRUE){
      out_adi <- subset(out_adi, select = -NAME)
      out <- merge(x = out, y = out_adi, by = "GEOID", all.x = TRUE)
    } else if (geometry == FALSE & prior_vars == TRUE){
      if (output == "tidy"){
        out_adi$moe <- NA
        out <- rbind(out, out_adi)
      } else if (output == "wide"){
        out_adi <- subset(out_adi, select = -NAME)
        out <- merge(x = out, y = out_adi, by = "GEOID", all.x = TRUE)
      }
    } else {
      out <- out_adi
    }

  }

  ## get ADI3 scores
  if ("adi3" %in% variables == TRUE){
    ## pull data
    out_adi3 <- get_adi3(geography = geography, output = output, year = year,
                       state = state, county = county, debug = debug)

    ## combine
    prior_vars <- "gini" %in% variables == TRUE | "svi" %in% variables == TRUE | "adi" %in% variables == TRUE

    if (geometry == TRUE){
      out_adi3 <- subset(out_adi3, select = -NAME)
      out <- merge(x = out, y = out_adi3, by = "GEOID", all.x = TRUE)
    } else if (geometry == FALSE & prior_vars == TRUE){
      if (output == "tidy"){
        out_adi3$moe <- NA
        out <- rbind(out, out_adi3)
      } else if (output == "wide"){
        out_adi3 <- subset(out_adi3, select = -NAME)
        out <- merge(x = out, y = out_adi3, by = "GEOID", all.x = TRUE)
      }
    } else {
      out <- out_adi3
    }

  }

  # move geometry to the end
  if (geometry == TRUE){
    out <- out[c(setdiff(names(out), "geometry"), "geometry")]
  } else {
    out <- tibble::as_tibble(out)
  }

  # order output
  #out <- dplyr::arrange(out, GEOID)
 
  out <- out[order(out$GEOID, decreasing = T),]
  # return output
  return(out)

}

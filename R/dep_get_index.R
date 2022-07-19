#' Calculate Deprivation Measures
#'
#' @description Calculates various measures of deprivation, including a range
#'     of options for structuring output. The three included measures include
#'     the area deprivation index, gini coefficient, and the social vulnerability
#'     index.
#'
#' @usage dep_get_index(geography, index, svi_round = FALSE, adi_percent = TRUE,
#'     keep_subscales = FALSE, keep_components = FALSE, output = "tidy", year, survey,
#'     state = NULL, county = NULL, territory = c("AS", "GU", "MP", "PR", "VI"),
#'     zcta = NULL, zcta3_method = NULL, geometry = FALSE, cb = FALSE,
#'     keep_geo_vars = FALSE, shift_geo = FALSE, units, key, debug = NULL)
#'
#' @param geography A character scalar; one of \code{"state"}, \code{"county"}, or
#'     \code{"tract"}
#' @param index A character scalar or vector listing deprivation measures
#'     to return. These include the area deprivation index (\code{"adi"}),
#'     gini coefficient (\code{"gini"}), and the social vulnerability index
#'     (\code{"svi"}). See Details.
#' @param svi_round A logical scalar; if \code{FALSE} (default), SVI will be
#'     calculated with the greatest possible precision. If \code{TRUE}, rounding
#'     will be introduced into the SVI algorithm in order to approximate the
#'     scores published by the CDC. This will not yield exact scores since,
#'     as the CDC warns in their documentation, reproducing them is not possible.
#'     With rounding applied, users can expect a majority of units to match
#'     CDC scores while a small number of units will have SVI scores that differ
#'     in the thousandths or ten-thousandths place.
#' @param adi_percent A logical scalar; if \code{TRUE} (default), ADI and (if
#'     requested) its subscales (see Details) will be returned as percentiles. If
#'     \code{FALSE}, raw ADI scores will be returned.
#' @param keep_subscales A logical scalar; if \code{FALSE} (default), only the
#'     full ADI and/or SVI scores (depending on what is passed to the \code{index}
#'     argument) will be returned. If \code{TRUE} and \code{"svi"} is listed for
#'     the \code{index} argument, the four SVI "themes" (see Details) will be
#'     returned along with the full SVI score. Similarly, if \code{"adi"} is
#'     listed for the \code{index} argument, the three ADI subscales (see Details)
#'     will be returned.
#' @param keep_components A logical scalar; if \code{FALSE} (default), none of
#'     the components used to calculate ADI and/or SVI scores (depending on what
#'     is passed to the \code{index} argument) will be returned. If \code{TRUE},
#'     all of the demographic variables used to calculate ADI and/or SVI will
#'     be returned.
#' @param year A numeric scalar between 2010 and 2020
#' @param survey A character scalar representing the Census product. It can
#'     be any American Community Survey product (either \code{"acs1"},
#'     \code{"acs3"}, or \code{"acs5"}). Note that \code{"acs3"} was
#'     discontinued after 2013.
#'
#' @export
dep_get_index <- function(geography, index, svi_round = FALSE, adi_percent = TRUE,
                          keep_subscales = FALSE, keep_components = FALSE,
                          output = "tidy", year, survey = "acs5",
                          state = NULL, county = NULL,
                          territory = c("AS", "GU", "MP", "PR", "VI"),
                          zcta = NULL, zcta3_method = NULL,
                          geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                          shift_geo = FALSE, units, key, debug = NULL){

  # global bindings

  # check inputs
  if (missing(geography)) {
    stop("A level of geography must be provided. Please choose one of: 'state' or 'county'.")
  }

  # if (geography %in% c("state", "county", "tract") == FALSE){
  #  stop("Invalid level of geography provided. Please choose one of: 'state' or 'county'.")
  # }

  if (is.null(debug)){
    debug <- "live"
  }

  # create vector of variables to pull
  varlist <- dep_build_varlist(geography = geography, index = index,
                               year = year, survey = survey, output = "vector")

  # create demo and geo tables
  demo <- dep_get_data(geography = geography, varlist = varlist, year = year,
                       survey = survey, state = state, county = county,
                       territory = territory,
                       zcta = zcta, zcta3_method = zcta3_method,
                       geometry = geometry, cb = cb, keep_geo_vars = keep_geo_vars,
                       shift_geo = shift_geo, key = key, debug = debug)

  geo <- demo$geo
  demo <- demo$demo

  # create gini output
  if ("gini" %in% index == TRUE){
    out <- dep_process_gini(demo, geography = geography, year = year, survey = survey)
    out <- merge(x = geo, y = out, by = "GEOID", all.x = TRUE)
  } else if ("gini" %in% index == FALSE){
    out <- geo
  }

  # remove geo
  rm(geo)

  # create svi output
  if ("svi" %in% index == TRUE){
    svi <- dep_process_svi(demo, geography = geography, year = year, survey = survey,
                           keep_subscales = keep_subscales,
                           keep_components = keep_components,
                           svi_round = svi_round)
    out <- merge(x = out, y = svi, by = "GEOID", all.x = TRUE)
  }

  # create adi output
  if (any(c("adi", "adi3") %in% index) == TRUE){
    adi <- dep_process_adi(demo, geography = geography, year = year, survey = survey,
                           adi = "adi" %in% index,
                           adi3 = "adi3" %in% index,
                           keep_components = keep_components)
    out <- merge(x = out, y = adi, by = "GEOID", all.x = TRUE)
  }

  # prep output
  out <- dplyr::arrange(out, GEOID)
 #  out <- tibble::as_tibble(out)

  if (geometry == FALSE){
    out <- tibble::as_tibble(out)
  }

  # fix id if zcta3
  if (geography == "zcta3"){
    names(out)[names(out) == "GEOID"] <- "ZCTA3"
  }

  # return output
  return(out)

}

#' Calculate Deprivation Measures
#'
#' @description Calculates various measures of deprivation.
#'
#' @usage dep_get_index(geography, variables, keep_subscales = FALSE,
#'     keep_components = FALSE, output, year, survey, state = NULL,
#'     county = NULL, territory = c("AS", "GU", "MP", "PR", "VI"),
#'     zcta = NULL, zcta3_method = NULL, geometry = FALSE,
#'     keep_geo_vars = FALSE, shift_geo = FALSE, units, debug = NULL)
#'
#' @export
dep_get_index <- function(geography, index, keep_subscales = FALSE,
                          keep_components = FALSE, output, year, survey = "acs5",
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
                           keep_components = keep_components)
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

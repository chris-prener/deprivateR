# Create Demographic Data
#
# This is an internal function that returns a list with two objects, one
# that corresponds to geometric data (or simply lists GEOID and NAME) and
# one that contains all of the demographic data.
#
# For all geometries other than ZCTA3, the length of both objects should be
# identical. For ZCTA3, the demographic data will be for ZCTA5 while the
# geometric data (or simply the ZCTA3 id) will be for ZCTA3.
#
dep_get_data <- function(geography, varlist, year, survey, state, county,
                         territory, zcta, zcta3_method, geometry, cb, keep_geo_vars,
                         shift_geo, key, debug){

  ## prep for ZCTA3
  if (geography == "zcta5"){

    ## call zippeR for demographics
    if (debug == "live"){
      demo <- zippeR::zi_get_demographics(year = year,
                                         variables = varlist,
                                         survey = survey,
                                         output = "wide",
                                         key = key)
    } else if (debug %in% c("messages", "call")){
      demo <- zippeR::zi_get_demographics(year = year,
                                         variables = varlist,
                                         survey = survey,
                                         output = "wide",
                                         key = key,
                                         debug = debug)
    } else if (debug == "test"){
      stop("testing debug mode not enabled yet!")
    }

    ## optionally filter based on zcta
    if (is.null(zcta) == FALSE){
        demo <- dplyr::filter(demo, GEOID %in% zcta == TRUE)
    }

    ## manage territories
    if (is.null(territory) == TRUE){

      ## all territories not including American Samoa
      demo <- dplyr::filter(demo, substr(GEOID, 1,3) %in% c("006", "007", "008", "009", "969") == FALSE)

      ## American Samoa
      demo <- dplyr::filter(demo, GEOID != "96799")

    } else if (is.null(territory) == FALSE){

      ## territory vector
      territory_vec <- c("AS", "GU", "MP", "PR", "VI")

      if (all(territory == territory_vec) == FALSE){

        ## construct list
        territory_vec <- territory_vec[territory_vec %in% territory == FALSE]

        ## create vector
        zcta_vec <- zippeR::zi_list_zctas(year = 2019, state = territory_vec, method = "intersect")

        ## exclude
        demo <- dplyr::filter(demo, GEOID %in% zcta_vec == FALSE)

      }
    }

    ## set year since 2020 uses the older geometries
    if (year == 2020){
      year_revise <- 2019
    } else if (year < 2020){
      year_revise <- year
    }

    ## optionally add geometry
    if (geometry == TRUE){

      ## set return
      if (keep_geo_vars == TRUE){
        return_type <- "full"
      } else if (keep_geo_vars == FALSE){
        return_type <- "id"
      }

      ## call zippeR
      geo <- zippeR::zi_get_geometry(year = year_revise,
                                     style = geography,
                                     return = return_type,
                                     class = "sf",
                                     territory = territory,
                                     cb = cb,
                                     shift_geo = shift_geo)

      ## optionally filter based on zcta
      if (is.null(zcta) == FALSE){
        geo <- dplyr::filter(geo, GEOID %in% zcta == TRUE)
      }

    } else if (geometry == FALSE){
      geo <- tibble::as_tibble(data.frame(GEOID = demo$GEOID))
    }

  } else if (geography == "zcta3"){

    ## call zippeR for demographics
    if (debug == "live"){
      demo <- zippeR::zi_get_demographics(year = year,
                                          variables = varlist,
                                          survey = survey,
                                          output = "tidy",
                                          key = key)
    } else if (debug %in% c("messages", "call")){
      demo <- zippeR::zi_get_demographics(year = year,
                                          variables = varlist,
                                          survey = survey,
                                          output = "tidy",
                                          key = key,
                                          debug = debug)
    } else if (debug == "test"){
      stop("testing debug mode not enabled yet!")
    }

    ## optionally filter based on zcta
    if (is.null(zcta) == FALSE){
      demo <- dplyr::filter(demo, GEOID %in% zcta == TRUE)
    }

    ## manage territories
    if (is.null(territory) == TRUE){

      ## all territories not including American Samoa
      demo <- dplyr::filter(demo, substr(GEOID, 1,3) %in% c("006", "007", "008", "009", "969") == FALSE)

      ## American Samoa
      demo <- dplyr::filter(demo, GEOID != "96799")

    } else if (is.null(territory) == FALSE){

      ## territory vector
      territory_vec <- c("AS", "GU", "MP", "PR", "VI")

      if (all(territory == territory_vec) == FALSE){

        ## construct list
        territory_vec <- territory_vec[territory_vec %in% territory == FALSE]

        ## create vector
        zcta_vec <- zippeR::zi_list_zctas(year = 2019, state = territory_vec, method = "intersect")

        ## exclude
        demo <- dplyr::filter(demo, GEOID %in% zcta_vec == FALSE)

      }
    }

    ## aggregate
    ### make lists of variables
    varlist_mod <- dep_zcta3_varlist(varlist = varlist)

    ### aggregate
    if (debug == "live"){
      demo <- zippeR::zi_aggregate(demo, year = year,
                                   extensive = varlist_mod$extensive,
                                   intensive = varlist_mod$intensive,
                                   intensive_method = zcta3_method,
                                   survey = survey, output = "wide",
                                   zcta = zcta, key = key)
    } else if (debug %in% c("messages", "call")){
      demo <- zippeR::zi_aggregate(demo, year = year,
                                   extensive = varlist_mod$extensive,
                                   intensive = varlist_mod$intensive,
                                   intensive_method = zcta3_method,
                                   survey = survey, output = "wide",
                                   zcta = zcta, key = key,
                                   debug = debug)
    } else if (debug == "test"){
      stop("testing debug mode not enabled yet!")
    }

    ### rename id
    demo <- dplyr::rename(demo, GEOID = ZCTA3)

    ## set year since 2020 uses the older geometries
    if (year == 2020){
      year_revise <- 2019
    } else if (year < 2020){
      year_revise <- year
    }

    ## optionally add geometry
    if (geometry == TRUE){

      ## call zippeR
      geo <- zippeR::zi_get_geometry(year = year_revise,
                                     style = geography,
                                     return = "id",
                                     class = "sf",
                                     territory = territory,
                                     shift_geo = shift_geo)

      ## rename id
      geo <- dplyr::rename(geo, GEOID = ZCTA3)

      ## optionally filter based on zcta
      if (is.null(zcta) == FALSE){
        geo <- dplyr::filter(geo, GEOID %in% zcta == TRUE)
      }

    } else if (geometry == FALSE){
      geo <- tibble::as_tibble(data.frame(GEOID = demo$GEOID))
    }

  } else {

    ## call tidycensus
    if (debug == "live"){
      out <- suppressMessages(tidycensus::get_acs(geography = geography,
                                                  state = state,
                                                  county = county,
                                                  variables = varlist,
                                                  output = "wide",
                                                  year = year,
                                                  survey = survey,
                                                  geometry = geometry,
                                                  keep_geo_vars = keep_geo_vars,
                                                  key = key))
    } else if (debug == "messages"){
      out <- tidycensus::get_acs(geography = geography,
                                 state = state,
                                 county = county,
                                 variables = varlist,
                                 output = "wide",
                                 year = year,
                                 survey = survey,
                                 geometry = geometry,
                                 keep_geo_vars = keep_geo_vars,
                                 key = key)
    } else if (debug == "call"){
      out <- tidycensus::get_acs(geography = geography,
                                 state = state,
                                 county = county,
                                 variables = varlist,
                                 output = "wide",
                                 year = year,
                                 survey = survey,
                                 geometry = geometry,
                                 keep_geo_vars = keep_geo_vars,
                                 show_call = TRUE,
                                 key = key)
    } else if (debug == "test"){
      stop("testing debug mode not enabled yet!")
    }

    # manage territories
    if (is.null(territory) == TRUE){

      out <- dplyr::filter(out, substr(GEOID, 1,2) %in% c("60", "66", "69", "72", "78") == FALSE)

    } else if (is.null(territory) == FALSE){

      ## territory vector
      territory_vec <- c("AS", "GU", "MP", "PR", "VI")

      if (all(territory == territory_vec) == FALSE){

        ## construct list
        territory_vec <- territory_vec[territory_vec %in% territory == FALSE]

        ## create data.frame
        territory_df <- data.frame(territory = c("AS", "GU", "MP", "PR", "VI"),
                                   GEOID = c("60", "66", "69", "72", "78"))
        territory_df <- territory_df[territory_df$territory %in% territory == FALSE,]
        territory_id <- territory_df$GEOID

        ## exclude
        out <- dplyr::filter(out, substr(GEOID, 1,2) %in% territory_id == FALSE)

      }
    }

    ## optionally handle geometric data
    if (geometry == TRUE){

      ## break apart geo and demo data
      if (keep_geo_vars == TRUE){
        geo <- dplyr::select(out, -c(varlist))
        demo <- dplyr::select(out, c())
      } else if (keep_geo_vars == FALSE){
        geo <- dplyr::select(out, GEOID, NAME)
        demo <- dplyr::select(out, -NAME)
        sf::st_geometry(demo) <- NULL
      }

      ## shift geometry
      if (shift_geo == TRUE){
        geo <- tigris::shift_geometry(geo, position = "below")
      }

    } else if (geometry == FALSE){
      geo <- dplyr::select(out, GEOID, NAME)
      demo <- dplyr::select(out, -NAME)
    }


  }

  ## construct output
  out <- list(
    geo = geo,
    demo = demo
  )

  ## return output
  return(out)

}

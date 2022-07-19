#' Create Variable Lists
#'
#' @description This function creates a vector or \code{tibble} containing
#'     variables included in particular calls.
#'
#' @usage dep_build_varlist(geography, year, index, survey, output = "vector")
#'
#' @param geography A character scalar; one of \code{"state"}, \code{"county"}, or
#'     \code{"tract"}
#' @param index A character scalar or vector listing deprivation measures
#'     to return. These include the area deprivation index (\code{"adi"}),
#'     gini coefficient (\code{"gini"}), and the social vulnerability index
#'     (\code{"svi"}).
#' @param year A numeric scalar between 2010 and 2020
#' @param survey A character scalar representing the Census product. It can
#'     be any American Community Survey product (either \code{"acs1"},
#'     \code{"acs3"}, or \code{"acs5"}). Note that \code{"acs3"} was
#'     discontinued after 2013.
#' @param output A character scalar; either \code{"vector"} (default) or
#'     \code{tibble}. See Return below.
#'
#' @return A vector of variable names or a \code{tibble} containing both
#'     variable names, labels, and the measure(s) they are associated with.
#'
#' @export
dep_build_varlist <- function(geography, index, year, survey, output = "vector"){

  ## gini
  if ("gini" %in% index == TRUE){
    a <- request_vars$gini10
  } else {
    a <- NULL
  }

  ## svi
  if ("svi" %in% index == TRUE){

    if (year == 2018){
      b <- unlist(request_vars$svi18)
    } else if (year %in% c(2019, 2020)){
      b <- unlist(request_vars$svi19)
    }

    svi <- TRUE

  } else {
    b <- NULL
    svi <- FALSE
  }

  ## adi
  if ("adi" %in% index == TRUE){
    c <- build_adi_varlist(geography, year = year, survey = survey, output = output)
  } else {
    c <- NULL
  }

  ## create output
  if (output == "vector"){
    out <- sort(unique(c(a,b,c)))
  } else if (output == "tibble"){
    out <- c
  }

  ## modify to collapse shared cols
  # if (){
  #
  # }

  ## return output
  return(out)

}

# sub function for ADI specific variable lists
build_adi_varlist <- function(geography, year, survey, output){

  ## create data object
  out <- sociome::acs_vars

  ## subset based on year+survey combination
  if (year == 2010){

    if (survey == "acs5"){
      out <- dplyr::filter(out, set5 == TRUE)
      out <- dplyr::select(out, variable, description, adi = set5)
    } else if (survey %in% c("acs1", "acs3")){
      out <- dplyr::filter(out, set4 == TRUE)
      out <- dplyr::select(out, variable, description, adi = set4)
    }

  } else if (year == 2011){

    if (survey == "acs5"){
      out <- dplyr::filter(out, set3 == TRUE)
      out <- dplyr::select(out, variable, description, adi = set3)
    } else if (survey %in% c("acs1", "acs3")){
      out <- dplyr::filter(out, set1 == TRUE)
      out <- dplyr::select(out, variable, description, adi = set1)
    }

  } else if (year %in% c(2012:2014)){
    out <- dplyr::filter(out, set1 == TRUE)
    out <- dplyr::select(out, variable, description, adi = set1)
  } else if (year %in% c(2015:2016)){

    if (geography == "block group"){
      out <- dplyr::filter(out, set2 == TRUE)
      out <- dplyr::select(out, variable, description, adi = set2)
    } else {
      out <- dplyr::filter(out, set1 == TRUE)
      out <- dplyr::select(out, variable, description, adi = set1)
    }

  } else if (year %in% c(2016:2020)){
    out <- dplyr::filter(out, set1 == TRUE)
    out <- dplyr::select(out, variable, description, adi = set1)
  }

  ## create output
  if (output == "vector"){
    out <- out$variable
  }

  ## return output
  return(out)

}

# sub function for adding appropriate suffix values
dep_expand_varlist <- function(geography, index, year, survey,
                               estimates_only = FALSE, moe_only = FALSE){

  # create initial variable list
  if (index %in% c("gini", "adi", "adi3", "svi")){
    out <- dep_build_varlist(geography = geography, index = index,
                             year = year, survey = survey)
  } else if (index == "svi, msl all") {

    if (year == 2018){
      out <- c(request_vars$svi18$msl_vars, request_vars$svi18$eng_vars)
    } else if (year %in% c(2019, 2020)){
      out <- c(request_vars$svi19$msl_vars, request_vars$svi19$eng_vars)
    }

  } else if (index %in% c("svi, pri", "svi, ses", "svi, hhd", "svi, msl", "svi, eng", "svi, htt")){

    req <- paste0(stringr::word(index, 2),"_vars")

    if (year == 2018){
      out <- request_vars$svi18[[req]]
    } else if (year %in% c(2019, 2020)){
      out <- request_vars$svi19[[req]]
    }

  }

  # expand and combine
  if (estimates_only == FALSE & moe_only == FALSE){
    out <- sort(c(paste0(out, "E"), paste0(out, "M")))
  } else if (estimates_only == TRUE & moe_only == FALSE){
    out <- sort(paste0(out, "E"))
  } else if (estimates_only == FALSE & moe_only == TRUE){
    out <- sort(paste0(out, "M"))
  }

  # return
  return(out)

}

# sub function for zcta3 aggregation
dep_zcta3_varlist <- function(varlist){

  ## intensive variables in scales
  intensive_vars <- c("B19083_001", # gini coefficient
                      "B06011_001", # median income
                      "B19301_001", # per capita income
                      "B19013_001", # median household income
                      "B19113_001", # median family income
                      "B25064_001", # median gross rent
                      "B25077_001", # median value of owner-occupied housing
                      "B25088_002") # median monthly costs for housing units w/ mortgage

  ## create output
  out <- list(
    extensive = varlist[varlist %in% intensive_vars == FALSE],
    intensive = varlist[varlist %in% intensive_vars == TRUE]
  )

  ## return output
  return(out)

}


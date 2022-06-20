#' Export Variable Lists
#'
#' @description This function exports a vector or \code{tibble} containing
#'     variables included in particular calls.
#'
#' @usage dep_build_varlist(year, variables, survey, output = "vector")
#'
#' @param geography A character scalar; one of \code{"state"}, \code{"county"}, or
#'     \code{"tract"}
#' @param variables A character scalar or vector listing deprivation measures
#'     to return. These include the gini coefficient (\code{gini})...
#' @param year A numeric between 2010 and 2020
#' @param survey A character scalar representing the Census product. It can
#'     be any American Community Survey product (either \code{"acs1"},
#'     \code{"acs3"}, or \code{"acs5"}). Note that \code{"acs3"} was
#'     discontinued after 2013.
#' @param output A character scalar; either \code{"vector"} (default) or
#'     \code{tibble}. See Returns below.
#'
#' @return A vector of variable names or a \code{tibble} containing both
#'     variable names, labels, and the measure(s) they are associated with.
#'
#' @export
dep_build_varlist <- function(geography, variables, year, survey, output = "vector"){

  ## gini
  if ("gini" %in% variables == TRUE){
    a <- dep_internal$request_vars$gini10
  } else {
    a <- NULL
  }

  ## svi
  if ("svi" %in% variables == TRUE){
    b <- unlist(dep_internal$request_vars$svi19)
    svi <- TRUE
  } else {
    b <- NULL
    svi <- FALSE
  }

  ## adi
  if ("adi" %in% variables == TRUE){
    c <- build_adi_varlist(geography, year = year, survey = survey, svi = svi, output = output)
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

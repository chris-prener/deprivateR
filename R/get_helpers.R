# Get Gini coefficients ####
get_gini <- function(geography, output, year, state, county,
                     geometry = FALSE, keep_geo_vars = FALSE,
                     shift_geo = FALSE, debug){

  # call tidycensus
  if (debug == "live"){
    out <- suppressMessages(tidycensus::get_acs(geography = geography,
                                                table = dep_internal$request_vars$gini10,
                                                output = output, year = year,
                                                state = state, county = county, survey = "acs5",
                                                geometry = geometry, keep_geo_vars = keep_geo_vars,
                                                shift_geo = FALSE))
  } else if (debug == "messages"){
    out <- tidycensus::get_acs(geography = geography,
                               table = dep_internal$request_vars$gini10,
                               output = output, year = year,
                               state = state, county = county, survey = "acs5",
                               geometry = geometry, keep_geo_vars = keep_geo_vars,
                               shift_geo = FALSE)
  } else if (debug == "test"){
    if (geometry == TRUE & keep_geo_vars == FALSE){
      out <- dep_internal$test_data$test_gini$test_gini_sf
    } else if (geometry == TRUE & keep_geo_vars == TRUE) {
      out <- dep_internal$test_data$test_gini$test_gini_sf_geo
    } else if (geometry == FALSE & output == "wide"){
      out <- dep_internal$test_data$test_gini$test_gini_df_wide
    } else if (geometry == FALSE & output == "tidy"){
      out <- dep_internal$test_data$test_gini$test_gini_df_tidy
    }
  }

  # structure output
  if (output == "tidy"){
    out$variable <- "gini"
  } else if (output == "wide"){
    names(out)[names(out) == "B19083_001E"] <- "E_GINI"
    names(out)[names(out) == "B19083_001M"] <- "M_GINI"
  }

  # return output
  return(out)

}

# Get ADI ####
get_adi <- function(geography, output, year, state, county, debug){

  # global variables
  GEOID = NAME = variable = estimate = Financial_Strength =
    Economic_Hardship_and_Inequality = Educational_Attainment = NULL

  # call sociome
  if (debug == "live"){
    out <- suppressMessages(sociome::get_adi(geography = geography,
                                             state = state,
                                             county = county,
                                             year = year,
                                             dataset = "acs5"))
  } else if (debug == "messages"){
    out <- sociome::get_adi(geography = geography,
                            state = state,
                            county = county,
                            year = year,
                            dataset = "acs5")
  } else if (debug == "test"){
    out <- dep_internal$test_data$test_adi
  }

  # remove ADI3
  out <- subset(out, select = -c(Financial_Strength,
                                 Economic_Hardship_and_Inequality,
                                 Educational_Attainment))

  # structure output
  if (output == "tidy"){
    out$variable <- "adi"
    names(out)[names(out) == "ADI"] <- "estimate"
    out <- subset(out, select = c(GEOID, NAME, variable, estimate))
  }

  # return output
  return(out)

}

# Get ADI3 ####
get_adi3 <- function(geography, output, year, state, county, debug){

  # global variables
  ADI = NULL

  # call sociome
  if (debug == "live"){
    out <- suppressMessages(sociome::get_adi(geography = geography,
                                             state = state,
                                             county = county,
                                             year = year,
                                             dataset = "acs5"))
  } else if (debug == "messages"){
    out <- sociome::get_adi(geography = geography,
                            state = state,
                            county = county,
                            year = year,
                            dataset = "acs5")
  } else if (debug == "test"){
    out <- dep_internal$test_data$test_adi
  }

  # remove ADI3
  out <- subset(out, select = -ADI)

  # rename columns
  names(out)[names(out) == "Financial_Strength"] <- "ADI3_FINANCE"
  names(out)[names(out) == "Economic_Hardship_and_Inequality"] <- "ADI3_ECON"
  names(out)[names(out) == "Educational_Attainment"] <- "ADI3_EDU"

  # structure output
  if (output == "tidy"){
    names <- setdiff(names(out), c("GEOID", "NAME"))
    out <- tidyr::pivot_longer(out, cols = names, names_to = "variable",
                               values_to = "estimate")

    out$variable <- tolower(out$variable)
    out$variable <- sub(pattern = "_", " - ", out$variable)
  }

  # return output
  return(out)

}

# Get SVI ####
get_svi <- function(geography, year, output, state, county, keep_subscales,
                    keep_components, debug){

  # global bindings
  SPL_THEME1 = SPL_THEME2 = SPL_THEME3 = SPL_THEME4 = SVI_RPL_THEME1 = SVI_RPL_THEME2 =
    SVI_RPL_THEME3 = SVI_RPL_THEME4 = SPL_THEMES = GEOID = SVI = NAME = NULL

  ## download variables
  pri <- get_svi_pri(geography = geography, year = year, state = state, county = county,
                     debug = debug)

  if (keep_components == FALSE){
    pri <- subset(pri, select = c(GEOID, NAME))
  }

  theme1 <- get_svi_ses(geography = geography, year = year, state = state, county = county,
                        keep_components = keep_components, debug = debug)
  theme2 <- get_svi_hhd(geography = geography, year = year, state = state, county = county,
                        keep_components = keep_components, debug = debug)
  theme3 <- get_svi_msl(geography = geography, year = year, state = state, county = county,
                        keep_components = keep_components, debug = debug)
  theme4 <- get_svi_htt(geography = geography, year = year, state = state, county = county,
                        keep_components = keep_components, debug = debug)

  ## combine
  out <- merge(x = pri, y = theme1, by = "GEOID", all.x = TRUE)
  out <- merge(x = out, y = theme2, by = "GEOID", all.x = TRUE)
  out <- merge(x = out, y = theme3, by = "GEOID", all.x = TRUE)
  out <- merge(x = out, y = theme4, by = "GEOID", all.x = TRUE)

  ## calculate svi
  out <- dplyr::mutate(out, SPL_THEMES = SPL_THEME1 + SPL_THEME2 + SPL_THEME3 + SPL_THEME4)
  # out <-
  out <- dplyr::mutate(out, SVI = dplyr::percent_rank(SPL_THEMES)*100)
  out <- dplyr::mutate(out, SVI_RPL_THEME1 = SVI_RPL_THEME1*100,
                       SVI_RPL_THEME2 = SVI_RPL_THEME2*100,
                       SVI_RPL_THEME3 = SVI_RPL_THEME3*100,
                       SVI_RPL_THEME4 = SVI_RPL_THEME4*100)

  ## keep components / keep subscales
  if (keep_components == FALSE){
    if (keep_subscales == FALSE){
      out <- subset(out, select = c(GEOID, NAME, SVI))
    } else if (keep_subscales == TRUE){
      out <- subset(out, select = c(GEOID, NAME, SVI, SVI_RPL_THEME1, SVI_RPL_THEME2, SVI_RPL_THEME3, SVI_RPL_THEME4))
    }

    names <- setdiff(names(out), c("GEOID", "NAME"))
  }

  ## convert to long optionally
  if (output == "tidy"){
    out <- tidyr::pivot_longer(out, cols = names, names_to = "variable",
                               values_to = "estimate")

    out$variable <- tolower(out$variable)
    out$variable <- sub(pattern = "_", " - ", out$variable)
  }

  ## return output
  return(out)

}

## primary variables ####
get_svi_pri <- function(geography, year, state, county, debug){

  ### global bindings
  GEOID = S0601_C01_001E = DP04_0001E = DP02_0001E = S0601_C01_001M =
    DP04_0001M = DP02_0001M = NAME = NULL

  ## download
  if (debug == "live"){
    out <- suppressMessages(tidycensus::get_acs(geography = geography,
                            state = state, county = county,
                            variables = dep_internal$request_vars$svi19$pri_vars,
                            output = "wide", year = year, suvey = "acs5"))
  } else if (debug == "messages"){
    out <- tidycensus::get_acs(geography = geography,
                              state = state, county = county,
                              variables = dep_internal$request_vars$svi19$pri_vars,
                              output = "wide", year = year, suvey = "acs5")
  } else if (debug == "test"){
    out <- dep_internal$test_data$test_svi$test_svi_pri
  }

  ## process components
  out <- dplyr::select(out,
                       GEOID,
                       NAME,
                       E_TOTPOP = S0601_C01_001E,
                       M_TOTPOP = S0601_C01_001M,
                       E_HU = DP04_0001E,
                       M_HU = DP04_0001M,
                       E_HH = DP02_0001E,
                       M_HH = DP02_0001M)

  ## return output
  return(out)

}

## ses variables ####
get_svi_ses <- function(geography, year, state, county, keep_components, debug){

  ## global bindings
  GEOID = B17001_001E = B17001_001M = B17001_002E = DP03_0001E = DP03_0001M =
    DP03_0005E = DP03_0005M = B19301_001E = B19301_001M = B06009_001E =
    B06009_001M = B06009_002E = B06009_002M = E_POV = D_POV = EP_POV =
    M_POV = E_UNEMP = D_UNEMP = EP_UNEMP = M_UNEMP = E_PCI = M_PCI =
    E_NOHSDP = D_NOHSDP = EP_NOHSDP = M_NOHSDP = EPL_POV = EPL_UNEMP =
    EPL_PCI = EPL_NOHSDP = SPL_THEME1 = SVI_RPL_THEME1 = NULL

  ## download
  if (debug == "live"){
    out <- suppressMessages(tidycensus::get_acs(geography = geography,
                                                state = state, county = county,
                                                variables = dep_internal$request_vars$svi19$ses_vars,
                                                output = "wide", year = year, suvey = "acs5"))
  } else if (debug == "messages"){
    out <- tidycensus::get_acs(geography = geography,
                        state = state, county = county,
                        variables = dep_internal$request_vars$svi19$ses_vars,
                        output = "wide", year = year, suvey = "acs5")
  } else if (debug == "test"){
    out <- dep_internal$test_data$test_svi$test_svi_ses
  }

  ## process components
  ### subset
  out <- dplyr::select(out,
    GEOID,
    D_POV = B17001_001E,
    DM_POV = B17001_001M,
    E_POV = B17001_002E,
    M_POV = B17001_001M,
    D_UNEMP = DP03_0001E,
    DM_UNEMP = DP03_0001M,
    E_UNEMP = DP03_0005E,
    M_UNEMP = DP03_0005M,
    E_PCI = B19301_001E,
    M_PCI = B19301_001M,
    D_NOHSDP = B06009_001E,
    DM_NOHSDP = B06009_001M,
    E_NOHSDP = B06009_002E,
    M_NOHSDP = B06009_002M
  )

  ### calculate theme 1
  out <- dplyr::mutate(out, EP_POV = E_POV/D_POV*100,
                       EPL_POV = dplyr::percent_rank(EP_POV),
                       .after = M_POV)
  out <- dplyr::mutate(out, EP_UNEMP = E_UNEMP/D_UNEMP*100,
                       EPL_UNEMP = dplyr::percent_rank(EP_UNEMP),
                       .after = M_UNEMP)
  out <- dplyr::mutate(out, EPL_PCI = dplyr::percent_rank(E_PCI),
                       .after = M_PCI)
  out <- dplyr::mutate(out, EP_NOHSDP = E_NOHSDP/D_NOHSDP*100,
                       EPL_NOHSDP = dplyr::percent_rank(EP_NOHSDP),
                       .after = M_NOHSDP)
  out <- dplyr::mutate(out, SPL_THEME1 = EPL_POV + EPL_UNEMP + EPL_PCI + EPL_NOHSDP,
                       SVI_RPL_THEME1 = dplyr::percent_rank(SPL_THEME1))

  ## optionally drop components
  if (keep_components == FALSE){
    out <- dplyr::select(out, GEOID, SPL_THEME1, SVI_RPL_THEME1)
  }

  ## return output
  return(out)

}

## hhd variables ####
get_svi_hhd <- function(geography, year, state, county, keep_components, debug){

  ## global bindings
  DP02_0007E = DP02_0011E = DP02_0011M = GEOID = S0101_C01_001E =
    S0101_C01_001M = S0101_C01_030E = S0101_C01_030M = S0101_C01_022E =
    S0101_C01_022M = DP02_0071E = DP02_0071M = DP02_0072E = DP02_0072M =
    DP02_0001E = DP02_0001M = E_SNGPNT = M_SNGPNT = E_AGE65 = D_AGE = EP_AGE65 =
    M_AGE65 = E_AGE17 = EP_AGE17 = M_AGE17 = E_DISABL = D_DISABL = EP_DISABL =
    M_DISABL = D_SNGPNT = EP_SNGPNT = EPL_AGE65 = EPL_AGE17 = EPL_DISABL =
    EPL_SNGPNT = SPL_THEME2 = SVI_RPL_THEME2 = NULL

  ## download
  if (debug == "live"){
    out <- suppressMessages(tidycensus::get_acs(geography = geography,
                                                state = state, county = county,
                                                variables = dep_internal$request_vars$svi19$hhd_vars,
                                                output = "wide", year = year, suvey = "acs5"))
  } else if (debug == "messages"){
    out <- tidycensus::get_acs(geography = geography,
                        state = state, county = county,
                        variables = dep_internal$request_vars$svi19$hhd_vars,
                        output = "wide", year = year, suvey = "acs5")
  } else if (debug == "test"){
    out <- dep_internal$test_data$test_svi$test_svi_hhd
  }

  ## process components
  ### pre-process single parent vars
  out <- dplyr::mutate(out,
                       E_SNGPNT = DP02_0007E+DP02_0011E,
                       M_SNGPNT = sqrt(DP02_0007E^2+DP02_0011M^2)
  )

  ### subset
  out <- dplyr::select(out,
    GEOID,
    D_AGE = S0101_C01_001E,
    DM_AGE = S0101_C01_001M,
    E_AGE65 = S0101_C01_030E,
    M_AGE65 = S0101_C01_030M,
    E_AGE17 = S0101_C01_022E,
    M_AGE17 = S0101_C01_022M,
    D_DISABL = DP02_0071E,
    DM_DISABL = DP02_0071M,
    E_DISABL = DP02_0072E,
    M_DISABL = DP02_0072M,
    D_SNGPNT = DP02_0001E,
    DM_SNGPNT = DP02_0001M,
    E_SNGPNT, M_SNGPNT
  )

  ### calculate theme 2
  out <- dplyr::mutate(out, EP_AGE65 = E_AGE65/D_AGE*100,
                       EPL_AGE65 = dplyr::percent_rank(EP_AGE65),
                       .after = M_AGE65)

  out <- dplyr::mutate(out, EP_AGE17 = E_AGE17/D_AGE*100,
                       EPL_AGE17 = dplyr::percent_rank(EP_AGE17),
                       .after = M_AGE17)

  out <- dplyr::mutate(out, EP_DISABL = E_DISABL/D_DISABL*100,
                       EPL_DISABL = dplyr::percent_rank(EP_DISABL),
                       .after = M_DISABL)

  out <- dplyr::mutate(out, EP_SNGPNT = E_SNGPNT/D_SNGPNT*100,
                       EPL_SNGPNT = dplyr::percent_rank(EP_SNGPNT),
                       .after = M_DISABL)

  out <- dplyr::mutate(out, SPL_THEME2 = EPL_AGE65 + EPL_AGE17 + EPL_DISABL + EPL_SNGPNT,
                       SVI_RPL_THEME2 = dplyr::percent_rank(SPL_THEME2))

  ## optionally drop components
  if (keep_components == FALSE){
    out <- dplyr::select(out, GEOID, SPL_THEME2, SVI_RPL_THEME2)
  }

  ## return output
  return(out)

}

## msl variables ####
get_svi_msl <- function(geography, year, state, county, keep_components, debug){

  ## global bindings
  variable = eng_vars = GEOID = estimate = S0601_C01_001E = B01001H_001E =
    S0601_C01_001M = B01001H_001M = E_MINRTY = M_MINRTY = E_LIMENG =
    B16004_001E = EP_MINRTY = EP_LIMENG = ELP_MINRTY = EPL_LIMENG =
    SPL_THEME3 = SVI_RPL_THEME3 = NULL

  ## download
  if (debug == "live"){
    m <- suppressMessages(tidycensus::get_acs(geography = geography,
                                                state = state, county = county,
                                                variables = dep_internal$request_vars$svi19$msl_vars,
                                                output = "wide", year = year, suvey = "acs5"))
    e <- suppressMessages(tidycensus::get_acs(geography = geography,
                                              state = state, county = county,
                                              table = dep_internal$request_vars$svi19$eng_table,
                                              output = "tidy", year = year, suvey = "acs5"))
  } else if (debug == "messages"){
    m <- tidycensus::get_acs(geography = geography,
                             state = state, county = county,
                             variables = dep_internal$request_vars$svi19$msl_vars,
                             output = "wide", year = year, suvey = "acs5")
    e <- tidycensus::get_acs(geography = geography,
                             state = state, county = county,
                             table = dep_internal$request_vars$svi19$eng_table,
                             output = "tidy", year = year, suvey = "acs5")
  } else if (debug == "test"){
    m <- dep_internal$test_data$test_svi$test_svi_msl
    e <- dep_internal$test_data$test_svi$test_svi_eng
  }

  ## process components
  ### subset e2
  e <- dplyr::filter(e, variable %in% dep_internal$request_vars$svi19$eng_vars)
  e <- dplyr::group_by(e, GEOID)
  e <- dplyr::summarise(e, E_LIMENG = sum(estimate))

  # note that no margin of error for M_LIMENG is calculated right now

  ### join e1 and e2
  out <- dplyr::left_join(m, e, by = "GEOID")

  ### pre-process minority vars
  out <- dplyr::mutate(out, E_MINRTY = S0601_C01_001E-B01001H_001E,
                       M_MINRTY = sqrt(S0601_C01_001M^2+B01001H_001M^2))

  ### subset
  out <- dplyr::select(out, GEOID, E_MINRTY, M_MINRTY, E_LIMENG, S0601_C01_001E,
                       B16004_001E)

  ### calculate theme 3
  out <- dplyr::mutate(out, EP_MINRTY = E_MINRTY/S0601_C01_001E*100,
                       ELP_MINRTY = dplyr::percent_rank(EP_MINRTY),
                       .after = M_MINRTY)

  out <- dplyr::mutate(out, EP_LIMENG = E_LIMENG/B16004_001E*100,
                       EPL_LIMENG = dplyr::percent_rank(EP_LIMENG),
                       .after = E_LIMENG)

  out <- dplyr::select(out, -S0601_C01_001E, -B16004_001E)

  out <- dplyr::mutate(out, SPL_THEME3 = ELP_MINRTY + EPL_LIMENG,
                       SVI_RPL_THEME3 = dplyr::percent_rank(SPL_THEME3))

  ## optionally drop components
  if (keep_components == FALSE){
    out <- dplyr::select(out, GEOID, SPL_THEME3, SVI_RPL_THEME3)
  }

  ## return output
  return(out)

}

## hhd variables ####
get_svi_htt <- function(geography, year, state, county, keep_components, debug){

  ## global bindings
  DP04_0012E = DP04_0013E = DP04_0012M = DP04_0013M = DP04_0078E = DP04_0079E =
    DP04_0078M = DP04_0079M = GEOID = DP04_0006E = DP04_0006M = E_MUNIT =
    M_MUNIT = DP04_0014E = DP04_0014M = DP04_0002E = DP04_0002M = E_CROWD =
    M_CROWD = DP04_0057E = DP04_0057M = DP04_0058E = DP04_0058M =
    B26001_001E = B26001_001M = S0601_C01_001E = D_HOUSE = EP_MUNIT = E_MOBILE =
    EP_MOBILE = M_MOBILE = D_CROWD = EP_CROWD =
    E_NOVEH = D_NOVEH = EP_NOVEH = M_NOVEH = E_GROUPQ = EP_GROUPQ =
    M_GROUPQ = DP04_0001E = EPL_MUNIT = EPL_MOBILE = EPL_CROWD = EPL_NOVEH =
    EPL_GROUPQ = SPL_THEME4 = SVI_RPL_THEME4 = NULL

  ## download
  if (debug == "live"){
    out <- suppressMessages(tidycensus::get_acs(geography = geography,
                                                state = state, county = county,
                                                variables = dep_internal$request_vars$svi19$htt_vars,
                                                output = "wide", year = year, suvey = "acs5"))
  } else if (debug == "messages"){
    out <- tidycensus::get_acs(geography = geography,
                               state = state, county = county,
                               variables = dep_internal$request_vars$svi19$htt_vars,
                               output = "wide", year = year, suvey = "acs5")
  } else if (debug == "test"){
    out <- dep_internal$test_data$test_svi$test_svi_htt
  }

  ## process components
  ### pre-process crowding vars
  out <- dplyr::mutate(out, E_MUNIT = DP04_0012E+DP04_0013E,
                       M_MUNIT = sqrt(DP04_0012M^2+DP04_0013M^2),
                       E_CROWD = DP04_0078E+DP04_0079E,
                       M_CROWD = sqrt(DP04_0078M^2+DP04_0079M^2))

  ### subset
  out <- dplyr::select(out,
    GEOID,
    D_HOUSE = DP04_0006E,
    M_HOUSE = DP04_0006M,
    E_MUNIT, M_MUNIT,
    E_MOBILE = DP04_0014E,
    M_MOBILE = DP04_0014M,
    D_CROWD = DP04_0002E,
    DM_CROWD = DP04_0002M,
    E_CROWD, M_CROWD,
    D_NOVEH = DP04_0057E,
    DM_NOVEH = DP04_0057M,
    E_NOVEH = DP04_0058E,
    M_NOVEH = DP04_0058M,
    E_GROUPQ = B26001_001E,
    M_GROUPQ = B26001_001M,
    S0601_C01_001E
  )

  ### calculat theme 3
  out <- dplyr::mutate(out, EP_MUNIT = E_MUNIT/D_HOUSE*100,
                       EPL_MUNIT = dplyr::percent_rank(EP_MUNIT),
                       .after = M_MUNIT)

  out <- dplyr::mutate(out, EP_MOBILE = E_MOBILE/D_HOUSE*100,
                       EPL_MOBILE = dplyr::percent_rank(EP_MOBILE),
                       .after = M_MOBILE)

  out <- dplyr::mutate(out, EP_CROWD = E_CROWD/D_CROWD*100,
                       EPL_CROWD = dplyr::percent_rank(EP_CROWD),
                       .after = M_CROWD)

  out <- dplyr::mutate(out, EP_NOVEH = E_NOVEH/D_NOVEH*100,
                       EPL_NOVEH = dplyr::percent_rank(EP_NOVEH),
                       .after = M_NOVEH)

  out <- dplyr::mutate(out, EP_GROUPQ = E_GROUPQ/S0601_C01_001E*100,
                       EPL_GROUPQ = dplyr::percent_rank(EP_GROUPQ),
                       .after = M_GROUPQ)

  out <- dplyr::select(out, -DP04_0001E, -S0601_C01_001E)

  out <- dplyr::mutate(out, SPL_THEME4 = EPL_MUNIT + EPL_MOBILE + EPL_CROWD + EPL_NOVEH + EPL_GROUPQ,
                       SVI_RPL_THEME4 = dplyr::percent_rank(SPL_THEME4))

  ## optionally drop components
  if (keep_components == FALSE){
    out <- dplyr::select(out, GEOID, SPL_THEME4, SVI_RPL_THEME4)
  }

  ## return output
  return(out)

}

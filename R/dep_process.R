# Processing Functions for Gini, SVI, ADI, and ADI3

## gini coefficient
dep_process_gini <- function(.data, geography, year, survey){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "gini",
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## rename
  names(.data)[names(.data) == "B19083_001E"] <- "E_GINI"
  names(.data)[names(.data) == "B19083_001M"] <- "M_GINI"

  ## return output
  return(.data)

}

## adi
dep_process_adi <- function(.data, geography, year, survey,
                            adi, adi3, keep_components){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "adi",
                                year = year, survey = survey,
                                estimates_only = TRUE)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## prep names
  x <- names(.data)
  x <- gsub("E$", "", x)
  names(.data) <- x

  ## call sociome
  .data <- sociome::calculate_adi(.data, keep_indicators = keep_components)

  ## clean-up
  .data <- subset(.data, select = -B11005_001)

  ## prep output
  if (adi == FALSE){
    .data <- subset(.data, select = -ADI)
  }

  if (adi3 == FALSE){
    .data <- subset(.data, select = -c(Financial_Strength,
                                   Economic_Hardship_and_Inequality,
                                   Educational_Attainment))
  } else if (adi3 == TRUE){
    names(.data)[names(.data) == "Financial_Strength"] <- "ADI3_FINANCE"
    names(.data)[names(.data) == "Economic_Hardship_and_Inequality"] <- "ADI3_ECON"
    names(.data)[names(.data) == "Educational_Attainment"] <- "ADI3_EDU"
  }

  ## return output
  return(.data)

}

## svi
dep_process_svi <- function(.data, geography, year, survey, keep_subscales,
                             keep_components, svi_round){

  ## subscale creation
  if (keep_components == TRUE){
    pri <- dep_process_svi_pri(.data, geography = geography, year = year,
                               survey = survey)
  }

  theme1 <- dep_process_svi_ses(.data, geography = geography, year = year,
                             survey = survey, keep_components = keep_components,
                             svi_round = svi_round)
  theme2 <- dep_process_svi_hhd(.data, geography = geography, year = year,
                                survey = survey, keep_components = keep_components,
                                svi_round = svi_round)
  theme3 <- dep_process_svi_msl(.data, geography = geography, year = year,
                                survey = survey, keep_components = keep_components,
                                svi_round = svi_round)
  theme4 <- dep_process_svi_htt(.data, geography = geography, year = year,
                                survey = survey, keep_components = keep_components,
                                svi_round = svi_round)

  ## combine subscales
  if (keep_components == TRUE){
    out <- merge(x = pri, y = theme1, by = "GEOID", all.x = TRUE)
    out <- merge(x = out, y = theme2, by = "GEOID", all.x = TRUE)
  } else if (keep_components == FALSE){
    out <- merge(x = theme1, y = theme2, by = "GEOID", all.x = TRUE)
  }

  out <- merge(x = out, y = theme3, by = "GEOID", all.x = TRUE)
  out <- merge(x = out, y = theme4, by = "GEOID", all.x = TRUE)

  ## calculate svi
  out$SPL_THEMES <- out$SPL_THEME1 + out$SPL_THEME2 + out$SPL_THEME3 + out$SPL_THEME4

  ## optionally round
  if (svi_round == TRUE){
    out$SPL_THEMES <- round(out$SPL_THEMES, digits = 4)
  }

  ## calculate svi percentile
  out$SVI <- dep_percent_rank(out$SPL_THEMES)

  ## optionally round
  if (svi_round == TRUE){
    out$SVI <- round(out$SVI, digits = 4)
  }

  ## format output
  final_vars <- c("GEOID", "SVI", "SVI_RPL_THEME1", "SVI_RPL_THEME2", "SVI_RPL_THEME3", "SVI_RPL_THEME4")

  if (keep_components == FALSE){
    if (keep_subscales == FALSE){
      out <- subset(out, select = c("GEOID", "SVI"))
    } else if (keep_subscales == TRUE){
      out <- subset(out, select = final_vars)
    }
  } else if (keep_components == TRUE){

    ## store other variable names in vector
    other_vars <- names(out)[names(out) %in% final_vars == FALSE]

    ## combine with final variable vector
    final_vars <- c(final_vars, other_vars)

    ## subset
    out <- subset(out, select = final_vars)

  }

  ## return output
  return(out)

}

### svi, primary variables
dep_process_svi_pri <- function(.data, geography, year, survey, svi_round){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "svi, pri",
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## rename components
  names(.data)[names(.data) == "DP02_0001E"] <- "E_HH"
  names(.data)[names(.data) == "DP02_0001M"] <- "M_HH"
  names(.data)[names(.data) == "DP04_0001E"] <- "E_HU"
  names(.data)[names(.data) == "DP04_0001M"] <- "M_HU"
  names(.data)[names(.data) == "S0601_C01_001E"] <- "E_TOTPOP"
  names(.data)[names(.data) == "S0601_C01_001M"] <- "M_TOTPOP"

  ## update output order
  .data <- subset(.data, select = c(GEOID, E_TOTPOP, M_TOTPOP,
                                    E_HU, M_HU, E_HH, M_HH))

  ## return output
  return(.data)

}

### svi, ses variables
dep_process_svi_ses <- function(.data, geography, year, survey, keep_components, svi_round){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "svi, ses",
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## rename components
  names(.data)[names(.data) == "B06009_001E"] <- "D_NOHSDP"
  names(.data)[names(.data) == "B06009_001M"] <- "DM_NOHSDP"
  names(.data)[names(.data) == "B06009_002E"] <- "E_NOHSDP"
  names(.data)[names(.data) == "B06009_002M"] <- "M_NOHSDP"
  names(.data)[names(.data) == "B17001_001E"] <- "D_POV"
  names(.data)[names(.data) == "B17001_001M"] <- "DM_POV"
  names(.data)[names(.data) == "B17001_002E"] <- "E_POV"
  names(.data)[names(.data) == "B17001_002M"] <- "M_POV"
  names(.data)[names(.data) == "B19301_001E"] <- "E_PCI"
  names(.data)[names(.data) == "B19301_001M"] <- "M_PCI"
  # names(.data)[names(.data) == "DP03_0001E"] <- "D_UNEMP"
  # names(.data)[names(.data) == "DP03_0001M"] <- "DM_UNEMP"
  names(.data)[names(.data) == "DP03_0005E"] <- "E_UNEMP"
  names(.data)[names(.data) == "DP03_0005M"] <- "M_UNEMP"
  names(.data)[names(.data) == "DP03_0009PE"] <- "EP_UNEMP"
  names(.data)[names(.data) == "DP03_0009PM"] <- "MP_UNEMP"

  ## calculate metrics
  .data$EP_POV <- .data$E_POV/.data$D_POV*100
  .data$MP_POV <- ((sqrt(.data$M_POV^2-((.data$EP_POV/100)^2*.data$DM_POV^2)))/.data$DM_POV)*100

  .data$EP_NOHSDP <- .data$E_NOHSDP/.data$D_NOHSDP*100
  .data$MP_NOHSDP <- ((sqrt(.data$M_NOHSDP^2-((.data$EP_NOHSDP/100)^2*.data$DM_NOHSDP^2)))/.data$DM_NOHSDP)*100

  ## optionally round
  if (svi_round == TRUE){
    .data$EP_UNEMP <- round(.data$EP_UNEMP, digits = 1)
    .data$MP_UNEMP <- round(.data$MP_UNEMP, digits = 1)
    .data$EP_POV <- round(.data$EP_POV, digits = 1)
    .data$MP_POV <- round(.data$MP_POV, digits = 1)
    .data$EP_NOHSDP <- round(.data$EP_NOHSDP, digits = 1)
    .data$MP_NOHSDP <- round(.data$MP_NOHSDP, digits = 1)
  }

  ## calculate percentiles
  .data$EPL_UNEMP <- dep_percent_rank(.data$EP_UNEMP)
  .data$EPL_POV <- dep_percent_rank(.data$EP_POV)
  .data$EPL_PCI <- 1-dep_percent_rank(.data$E_PCI)
  .data$EPL_NOHSDP <- dep_percent_rank(.data$EP_NOHSDP)

  ## calculate theme
  .data$SPL_THEME1 <- .data$EPL_POV + .data$EPL_UNEMP + .data$EPL_PCI + .data$EPL_NOHSDP

  ## optionally round
  if (svi_round == TRUE){
    .data$SPL_THEME1 <- round(.data$SPL_THEME1, digits = 4)
  }

  ## calculate theme percentile
  .data$SVI_RPL_THEME1 <- dep_percent_rank(.data$SPL_THEME1)

  ## optionally round
  if (svi_round == TRUE){
    .data$SVI_RPL_THEME1 <- round(.data$SVI_RPL_THEME1, digits = 4)
  }

  ## update output order
  if (keep_components == TRUE){
    .data <- subset(.data, select = c(GEOID,
                                      D_POV, DM_POV, E_POV, M_POV, EP_POV, MP_POV, EPL_POV,
                                      E_UNEMP, M_UNEMP, EP_UNEMP, MP_UNEMP, EPL_UNEMP, # D_UNEMP, DM_UNEMP,
                                      E_PCI, M_PCI, EPL_PCI,
                                      D_NOHSDP, DM_NOHSDP, E_NOHSDP, M_NOHSDP, EP_NOHSDP, MP_NOHSDP, EPL_NOHSDP,
                                      SPL_THEME1, SVI_RPL_THEME1))
  } else if (keep_components == FALSE){
    .data <- subset(.data, select = c(GEOID, SPL_THEME1, SVI_RPL_THEME1))
  }

  ## return output
  return(.data)

}

### svi, ses variables
dep_process_svi_hhd <- function(.data, geography, year, survey, keep_components, svi_round){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "svi, hhd",
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## rename components
  if (year == 2018){
    names(.data)[names(.data) == "DP02_0001E"] <- "D_SNGPNT"
    names(.data)[names(.data) == "DP02_0001M"] <- "DM_SNGPNT"
    names(.data)[names(.data) == "DP02_0070E"] <- "D_DISABL"
    names(.data)[names(.data) == "DP02_0070M"] <- "DM_DISABL"
    names(.data)[names(.data) == "DP02_0071E"] <- "E_DISABL"
    names(.data)[names(.data) == "DP02_0071M"] <- "M_DISABL"
  } else if (year %in% c(2019, 2020)){
    names(.data)[names(.data) == "B11012_001E"] <- "D_SNGPNT"
    names(.data)[names(.data) == "B11012_001M"] <- "DM_SNGPNT"
    names(.data)[names(.data) == "DP02_0071E"] <- "D_DISABL"
    names(.data)[names(.data) == "DP02_0071M"] <- "DM_DISABL"
    names(.data)[names(.data) == "DP02_0072E"] <- "E_DISABL"
    names(.data)[names(.data) == "DP02_0072M"] <- "M_DISABL"
  }

  names(.data)[names(.data) == "S0101_C01_001E"] <- "D_AGE"
  names(.data)[names(.data) == "S0101_C01_001M"] <- "DM_AGE"
  names(.data)[names(.data) == "S0101_C01_022E"] <- "E_AGE17"
  names(.data)[names(.data) == "S0101_C01_022M"] <- "M_AGE17"
  names(.data)[names(.data) == "S0101_C01_030E"] <- "E_AGE65"
  names(.data)[names(.data) == "S0101_C01_030M"] <- "M_AGE65"

  ## calculate components
  if (year == 2018){
    .data$E_SNGPNT <- .data$DP02_0007E+.data$DP02_0009E
    .data$M_SNGPNT <- sqrt(.data$DP02_0007M^2+.data$DP02_0009M^2)
  } else if (year %in% c(2019, 2020)){
    .data$E_SNGPNT <- .data$B11012_010E+.data$B11012_015E
    .data$M_SNGPNT <- sqrt(.data$B11012_010M^2+.data$B11012_015M^2)
  }

  ## calculate metrics
  .data$EP_AGE17 <- .data$E_AGE17/.data$D_AGE*100
  .data$MP_AGE17 <- ((sqrt(.data$M_AGE17^2-((.data$EP_AGE17/100)^2*.data$DM_AGE^2)))/.data$DM_AGE)*100

  .data$EP_AGE65 <- .data$E_AGE65/.data$D_AGE*100
  .data$MP_AGE65 <- ((sqrt(.data$M_AGE65^2-((.data$EP_AGE65/100)^2*.data$DM_AGE^2)))/.data$DM_AGE)*100

  .data$EP_DISABL <- .data$E_DISABL/.data$D_DISABL*100
  .data$MP_DISABL <- ((sqrt(.data$M_DISABL^2-((.data$EP_DISABL/100)^2*.data$DM_DISABL^2)))/.data$DM_DISABL)*100

  .data$EP_SNGPNT <- .data$E_SNGPNT/.data$D_SNGPNT*100
  .data$MP_SNGPNT <- ((sqrt(.data$M_SNGPNT^2-((.data$EP_SNGPNT/100)^2*.data$DM_SNGPNT^2)))/.data$DM_SNGPNT)*100

  ## optionally round
  if (svi_round == TRUE){
    .data$EP_AGE17 <- round(.data$EP_AGE17, digits = 1)
    .data$MP_AGE17 <- round(.data$MP_AGE17, digits = 1)
    .data$EP_AGE65 <- round(.data$EP_AGE65, digits = 1)
    .data$MP_AGE65 <- round(.data$MP_AGE65, digits = 1)
    .data$EP_DISABL <- round(.data$EP_DISABL, digits = 1)
    .data$MP_DISABL <- round(.data$MP_DISABL, digits = 1)
    .data$EP_SNGPNT <- round(.data$EP_SNGPNT, digits = 1)
    .data$MP_SNGPNT <- round(.data$MP_SNGPNT, digits = 1)
  }

  ## calculate percentiles
  .data$EPL_AGE17 <- dep_percent_rank(.data$EP_AGE17)
  .data$EPL_AGE65 <- dep_percent_rank(.data$EP_AGE65)
  .data$EPL_DISABL <- dep_percent_rank(.data$EP_DISABL)
  .data$EPL_SNGPNT <- dep_percent_rank(.data$EP_SNGPNT)

  ## calculate theme
  .data$SPL_THEME2 <- .data$EPL_AGE17 + .data$EPL_AGE65 + .data$EPL_DISABL + .data$EPL_SNGPNT

  ## optionally round
  if (svi_round == TRUE){
    .data$SPL_THEME2 <- round(.data$SPL_THEME2, digits = 4)
  }

  ## calculate theme percentile
  .data$SVI_RPL_THEME2 <- dep_percent_rank(.data$SPL_THEME2)

  ## optionally round
  if (svi_round == TRUE){
    .data$SVI_RPL_THEME2 <- round(.data$SVI_RPL_THEME2, digits = 4)
  }

  ## update output order
  if (keep_components == TRUE){
    .data <- subset(.data, select = c(GEOID, D_AGE, DM_AGE, E_AGE17, M_AGE17, EP_AGE17, MP_AGE17, EPL_AGE17,
                                      E_AGE65, M_AGE65, EP_AGE65, MP_AGE65, EPL_AGE65,
                                      D_DISABL, DM_DISABL, E_DISABL, M_DISABL, EP_DISABL, MP_DISABL, EPL_DISABL,
                                      D_SNGPNT, DM_SNGPNT, E_SNGPNT, M_SNGPNT, EP_SNGPNT, MP_SNGPNT, EPL_SNGPNT,
                                      SPL_THEME2, SVI_RPL_THEME2))
  } else if (keep_components == FALSE){
    .data <- subset(.data, select = c(GEOID, SPL_THEME2, SVI_RPL_THEME2))
  }

  ## return output
  return(.data)

}

### svi, msl variables
dep_process_svi_msl <- function(.data, geography, year, survey, keep_components, svi_round){

  ## English variables
  ### create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "svi, eng",
                                year = year, survey = survey,
                                estimates_only = TRUE)
  varlist_subset <- c("GEOID", varlist)

  ### subset estimates
  eng <- subset(.data, select = varlist_subset)

  ### pivot estimates
  eng <- tidyr::pivot_longer(eng, cols = varlist, names_to = "variable", values_to = "estimate")
  eng <- dplyr::group_by(eng, GEOID)
  eng <- dplyr::summarise(eng, E_LIMENG = sum(estimate))

  ### create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "svi, eng",
                                year = year, survey = survey,
                                moe_only = TRUE)
  varlist_subset <- c("GEOID", varlist)

  ### subset estimates
  eng_moe <- subset(.data, select = varlist_subset)

  ### pivot estimates
  eng_moe <- tidyr::pivot_longer(eng_moe, cols = varlist, names_to = "variable", values_to = "moe")
  eng_moe$moe <- eng_moe$moe^2
  eng_moe <- dplyr::group_by(eng_moe, GEOID)
  eng_moe <- dplyr::summarise(eng_moe, M_LIMENG = sum(moe))
  eng_moe$M_LIMENG <- sqrt(eng_moe$M_LIMENG)

  ### combine
  eng <- merge(x = eng, y = eng_moe, by = "GEOID", all.x = TRUE)

  ## Remaining Variables
  ### create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "svi, msl",
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ### subset
  .data <- subset(.data, select = varlist)

  ### combine with english data
  .data <- merge(x = .data, y = eng, by = "GEOID", all.x = TRUE)

  ## Final Steps
  ### rename components
  names(.data)[names(.data) == "B16004_001E"] <- "D_LIMENG"
  names(.data)[names(.data) == "B16004_001M"] <- "DM_LIMENG"

  ### calculate components
  .data$E_MINRTY <- .data$S0601_C01_001E-.data$B01001H_001E
  .data$M_MINRTY <- sqrt(.data$S0601_C01_001M^2+.data$B01001H_001M^2)

  ### calculate metrics
  .data$EP_MINRTY <- .data$E_MINRTY/.data$S0601_C01_001E*100
  .data$MP_MINRTY <- ((sqrt(.data$M_MINRTY^2-((.data$EP_MINRTY/100)^2*.data$S0601_C01_001M^2)))/.data$S0601_C01_001M)*100

  .data$EP_LIMENG <- .data$E_LIMENG/.data$D_LIMENG*100
  .data$MP_LIMENG <- ((sqrt(.data$M_LIMENG^2-((.data$EP_LIMENG/100)^2*.data$DM_LIMENG^2)))/.data$DM_LIMENG)*100

  ## optionally round
  if (svi_round == TRUE){
    .data$EP_MINRTY <- round(.data$EP_MINRTY, digits = 1)
    .data$MP_MINRTY <- round(.data$MP_MINRTY, digits = 1)
    .data$EP_LIMENG <- round(.data$EP_LIMENG, digits = 1)
    .data$MP_LIMENG <- round(.data$MP_LIMENG, digits = 1)
  }

  ## calculate percentiles
  .data$EPL_MINRTY <- dep_percent_rank(.data$EP_MINRTY)
  .data$EPL_LIMENG <- dep_percent_rank(.data$EP_LIMENG)

  ### calculate theme
  .data$SPL_THEME3 <- .data$EPL_MINRTY + .data$EPL_LIMENG

  ## optionally round
  if (svi_round == TRUE){
    .data$SPL_THEME3 <- round(.data$SPL_THEME3, digits = 4)
  }

  ## calculate theme percentile
  .data$SVI_RPL_THEME3 <- dep_percent_rank(.data$SPL_THEME3)

  ## optionally round
  if (svi_round == TRUE){
    .data$SVI_RPL_THEME3 <- round(.data$SVI_RPL_THEME3, digits = 4)
  }

  ### update output order
  if (keep_components == TRUE){
    .data <- subset(.data, select = c(GEOID, E_MINRTY, M_MINRTY, EP_MINRTY, MP_MINRTY, EPL_MINRTY,
                                      D_LIMENG, DM_LIMENG, E_LIMENG, M_LIMENG, EP_LIMENG, MP_LIMENG, EPL_LIMENG,
                                      SPL_THEME3, SVI_RPL_THEME3))
  } else if (keep_components == FALSE){
    .data <- subset(.data, select = c(GEOID, SPL_THEME3, SVI_RPL_THEME3))
  }

  ## return output
  return(.data)

}

### svi, htt variables
dep_process_svi_htt <- function(.data, geography, year, survey, keep_components, svi_round){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "svi, htt",
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## rename components
  names(.data)[names(.data) == "B26001_001E"] <- "E_GROUPQ"
  names(.data)[names(.data) == "B26001_001M"] <- "M_GROUPQ"
  names(.data)[names(.data) == "DP04_0002E"] <- "D_CROWD"
  names(.data)[names(.data) == "DP04_0002M"] <- "DM_CROWD"
  names(.data)[names(.data) == "DP04_0006E"] <- "D_HOUSE"
  names(.data)[names(.data) == "DP04_0006M"] <- "DM_HOUSE"
  names(.data)[names(.data) == "DP04_0014E"] <- "E_MOBILE"
  names(.data)[names(.data) == "DP04_0014M"] <- "M_MOBILE"
  names(.data)[names(.data) == "DP04_0057E"] <- "D_NOVEH"
  names(.data)[names(.data) == "DP04_0057M"] <- "DM_NOVEH"
  names(.data)[names(.data) == "DP04_0058E"] <- "E_NOVEH"
  names(.data)[names(.data) == "DP04_0058M"] <- "M_NOVEH"

  ## calculate components
  .data$E_MUNIT <- .data$DP04_0012E+.data$DP04_0013E
  .data$M_MUNIT <- sqrt(.data$DP04_0012M^2+.data$DP04_0013M^2)
  .data$E_CROWD <- .data$DP04_0078E+.data$DP04_0079E
  .data$M_CROWD <- sqrt(.data$DP04_0078M^2+.data$DP04_0079M^2)

  ## calculate metrics
  .data$EP_MUNIT <- .data$E_MUNIT/.data$D_HOUSE*100
  .data$MP_MUNIT <- ((sqrt(.data$M_MUNIT^2-((.data$EP_MUNIT/100)^2*.data$DM_HOUSE^2)))/.data$DM_HOUSE)*100

  .data$EP_MOBILE <- .data$E_MOBILE/.data$D_HOUSE*100
  .data$MP_MOBILE <- ((sqrt(.data$M_MOBILE^2-((.data$EP_MOBILE/100)^2*.data$DM_HOUSE^2)))/.data$DM_HOUSE)*100

  .data$EP_CROWD <- .data$E_CROWD/.data$D_CROWD*100
  .data$MP_CROWD <- ((sqrt(.data$M_CROWD^2-((.data$EP_CROWD/100)^2*.data$DM_CROWD^2)))/.data$DM_CROWD)*100

  .data$EP_NOVEH <- .data$E_NOVEH/.data$D_NOVEH*100
  .data$MP_NOVEH <- ((sqrt(.data$M_NOVEH^2-((.data$EP_NOVEH/100)^2*.data$DM_NOVEH^2)))/.data$DM_NOVEH)*100

  .data$EP_GROUPQ <- .data$E_GROUPQ/.data$S0601_C01_001E*100
  .data$MP_GROUPQ <- ((sqrt(.data$M_GROUPQ^2-((.data$EP_GROUPQ/100)^2*.data$S0601_C01_001M^2)))/.data$S0601_C01_001M)*100

  ## optionally round
  if (svi_round == TRUE){
    .data$EP_MUNIT <- round(.data$EP_MUNIT, digits = 1)
    .data$MP_MUNIT <- round(.data$MP_MUNIT, digits = 1)
    .data$EP_MOBILE <- round(.data$EP_MOBILE, digits = 1)
    .data$MP_MOBILE <- round(.data$MP_MOBILE, digits = 1)
    .data$EP_CROWD <- round(.data$EP_CROWD, digits = 1)
    .data$MP_CROWD <- round(.data$MP_CROWD, digits = 1)
    .data$EP_NOVEH <- round(.data$EP_NOVEH, digits = 1)
    .data$MP_NOVEH <- round(.data$MP_NOVEH, digits = 1)
    .data$EP_GROUPQ <- round(.data$EP_GROUPQ, digits = 1)
    .data$MP_GROUPQ <- round(.data$MP_GROUPQ, digits = 1)
  }

  ## calculate percentiles
  .data$EPL_MUNIT <- dep_percent_rank(.data$EP_MUNIT)
  .data$EPL_MOBILE <- dep_percent_rank(.data$EP_MOBILE)
  .data$EPL_CROWD <- dep_percent_rank(.data$EP_CROWD)
  .data$EPL_NOVEH <- dep_percent_rank(.data$EP_NOVEH)
  .data$EPL_GROUPQ <- dep_percent_rank(.data$EP_GROUPQ)

  ## calculate theme
  .data$SPL_THEME4 <- .data$EPL_MUNIT + .data$EPL_MOBILE + .data$EPL_CROWD + .data$EPL_NOVEH + .data$EPL_GROUPQ

  ## optionally round
  if (svi_round == TRUE){
    .data$SPL_THEME4 <- round(.data$SPL_THEME4, digits = 4)
  }

  ## calculate theme percentile
  .data$SVI_RPL_THEME4 <- dep_percent_rank(.data$SPL_THEME4)

  ## optionally round
  if (svi_round == TRUE){
    .data$SVI_RPL_THEME4 <- round(.data$SVI_RPL_THEME4, digits = 4)
  }

  ## update output order
  if (keep_components == TRUE){
    .data <- subset(.data, select = c(GEOID, D_HOUSE, DM_HOUSE, E_MUNIT, M_MUNIT, EP_MUNIT, MP_MUNIT, EPL_MUNIT,
                                      E_MOBILE, M_MOBILE, EP_MOBILE, MP_MOBILE, EPL_MOBILE,
                                      D_CROWD, DM_CROWD, E_CROWD, M_CROWD, EP_CROWD, MP_CROWD, EPL_CROWD,
                                      D_NOVEH, DM_NOVEH, E_NOVEH, M_NOVEH, EP_NOVEH, MP_NOVEH, EPL_NOVEH,
                                      E_GROUPQ, M_GROUPQ, EP_GROUPQ, MP_GROUPQ, EPL_GROUPQ,
                                      SPL_THEME4, SVI_RPL_THEME4))
  } else if (keep_components == FALSE){
    .data <- subset(.data, select = c(GEOID, SPL_THEME4, SVI_RPL_THEME4))
  }

  ## return output
  return(.data)

}

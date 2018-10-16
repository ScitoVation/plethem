#' Calcuate MPPGL and CPPGL based on age
#' @description Calculates the Microsomal Protein per Gram liver (MPPGL) and Cytosolic Protein Per Gram Liver(CPPGL)
#'   for humans based on the age of the person in years. The equations for MPPGL and CPPGL were developed internally at
#'   Scitovation
#' @param age age of the human in years
#' @return list containing the "MPPGL" and "CPPGL" values for the
#' @export
calcMPCPPGL<- function(age){
  #calculate mppgl and cppgl from age

  #calcualte MPPGL
  mppgl <- 10^((1.407) + (0.0158 *age) -
                 (0.00038 * age^2) + (0.0000024 * age^3))
  mp_cp_ratio <- 0.495594485

  cppgl <- mppgl/mp_cp_ratio

  return(list("MPPGL" = mppgl,"CPPGL" = cppgl))
}

#' get data for human cyp fraction by age
#' @description The function returns the expression of cyps at each age as a fraction of the adult (25 years).
#'   The number needs to be multiplied by the cyp abundance information in the database to get the nominal expression
#'   value for the cyp.
#' @param age the age of the person in years
#' @return dataframe containing the fractional expression for all the cyps
#' @export
getAllCypData<- function(age){
  cyp_df <- data.frame("Enzymes" = c("CYP1A2","CYP2C9","CYP2C19",
                                     "CYP3A4","CYP2B6","CES1M",
                                     "CES1C","CES2M","CES2C","CYP3A5"),
                       "Ontogeny"=c(calc_cyp1a2(age),calc_cyp2c9(age),
                                     calc_cyp2c19(age),calc_cyp3a4(age),
                                     calc_cyp2b6(age),calc_ces1m(age),
                                     calc_ces1c(age),calc_ces2m(age),
                                     calc_ces2c(age),calc_cyp3a4(age)),
                       stringsAsFactors = F
  )
  return(cyp_df)

}
# Calculate cyp1a2 fraction
# @description get expression for cyp1a2 as a fraction of adult
# @param age age of the human in years
calc_cyp1a2 <- function(age){

  # convert age to weeks assuming 40 week pregnancy
  weeks <- (age*52)+40
  result <- 1*exp(log(0.0153/1)*exp(-0.01423*weeks))
  return(result)

}
# Calculate cyp2c9 fraction
# @description get expression for cyp2c9 as a fraction of adult
# @param age age of the human in years
calc_cyp2c9 <- function(age){

  # convert age to weeks assuming 40 week pregnancy
  weeks <- (age*52)+40
  result <- 1*exp(log((1.65*10^-8)/1)*exp(-0.09376*weeks))
  return(result)

}
# Calculate cyp2c19 fraction
# @description get expression for cyp2c19 as a fraction of adult
# @param age age of the human in years
calc_cyp2c19 <- function(age){

  # convert age to weeks assuming 40 week pregnancy
  weeks <- (age*52)+40
  result <- 0.9998*exp(log(0.02271*0.9998)*exp(-0.03287*weeks))
  return(result)

}
# Calculate cyp3a4 fraction
# @description get expression for cyp3a4 as a fraction of adult
# @param age age of the human in years
calc_cyp3a4 <- function(age){

  # convert age to weeks assuming 40 week pregnancy
  weeks <- (age*52)+40
  result <- 1.026*weeks/(33.72+weeks)
  return(result)

}
#' Calculate cyp2b6 fraction
#' @description get expression for cyp2b6 as a fraction of adult
#' @param age age of the human in years
calc_cyp2b6 <- function(age){

  # convert age to weeks assuming 40 week pregnancy
  weeks <- (age*52)+40
  result <- 1.014*weeks/(19.6 + weeks)
  return(result)

}
# Calculate ces1m fraction
# @description get expression for ces1m as a fraction of adult
# @param age age of the human in years
calc_ces1m <- function(age){

  result <- 0.9998*exp(log(0.2316*0.9998)*exp((-39.97*age)))
  return(result)

}
# Calculate ces1c fraction
# @description get expression for ces1c as a fraction of adult
# @param age age of the human in years
calc_ces1c <- function(age){

  numerator <- 1.024*(age^0.5801)
  denominator <- (0.03959^0.5801)+(age^0.5801)
  result <- numerator/denominator
  return(result)
}
# Calculate ces2m fraction
# @description get expression for ces2m as a fraction of adult
# @param age age of the human in years
calc_ces2m <- function(age){

  numerator <- ((age^0.2656)*(1.457-0.3781))
  denominator <- (age^0.2656)+(7.831^0.2656)
  result <- 0.3781+(numerator/denominator)
  return(result)
}
# Calculate ces2c fraction
# @description get expression for ces2c as a fraction of adult
# @param age age of the human in years
calc_ces2c <- function(age){

  numerator <- (age^0.6546)*(1.003-0.2191)
  denominator <- age^0.6546+0.005482^0.6546
  result <- 0.2191+(numerator/denominator)
  return(result)
}

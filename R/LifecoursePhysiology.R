
# Small utility function for linear interpolation
# Used internally
interp2 <- function(x1, x2, y1, y2, x)
{
  m = (y2 - y1)/(x2 - x1)
  y = y1 + m*(x - x1)
  return(y)
}

#' Get volumes for tissues provided on the basis of age and gender
#'
#' @param age Age of the organism in years
#' @param gender The gender of the organism "M" for Male or "F" for Female
#' @param perf_frct fraction of perfused tissue. default to 0.85 
#' @param tissues List of tissues for which the volumes are needed. The tissues can be one of the following
#' liver,
#' blood,
#' fat,
#' bone,
#' brain,
#' gonad,
#' heart,
#' intestine,
#' kidney,
#' lung,
#' pancreas,
#' skin,
#' spleen,
#' stomach,
#' thymus,
#' remaining,
#' muscle
#' @return
#' list containing volumes for age, gender and tissues.
#' @export
#'
getLifecourseTissueVolumes<- function(age = 25, gender = "M",perf_frct = 0.85,
                                      tissues=list()){
  tissue_vol_list <- NULL
  available_tissues <- c("blood","fat","skin","muscle","bone","brain","lung",
                         "heart","gi","kidney","liver","rpf","spf")
  spf_tissues <- c("fat","skin","muscle","bone")
  rpf_tissues <- available_tissues[!(available_tissues %in% c(spf_tissues,"rpf","spf"))]
  inactive_tissues <- dplyr::setdiff(available_tissues,tissues)
  rpf_inactive_tissues <- inactive_tissues[inactive_tissues %in% rpf_tissues]
  spf_inactive_tissues <- inactive_tissues[inactive_tissues %in% spf_tissues]

  if(is.na(age)) return

  tissue_vol_list[["gi"]]<-getLifecourseGutVolume(age,gender)
  tissue_vol_list[["liver"]]<-getLifecourseLiverVolume(age,gender)
  tissue_vol_list[["blood"]]<-getLifecourseBloodVolume(age,gender)
  tissue_vol_list[["fat"]]<-getLifecourseAdiposeVolume(age,gender)
  tissue_vol_list[["bone"]]<-getLifecourseBoneVolume(age,gender)
  tissue_vol_list[["brain"]]<-getLifecourseBrainVolume(age,gender)
  tissue_vol_list[["heart"]]<-getLifecourseHeartVolume(age,gender)
  tissue_vol_list[["kidney"]]<-getLifecourseKidneyVolume(age,gender)
  tissue_vol_list[["lung"]]<-getLifecourseLungVolume(age,gender)
  tissue_vol_list[["skin"]]<-getLifecourseSkinVolume(age,gender)
  tissue_vol_list[["muscle"]]<-getLifecourseMuscleVolume(age,gender)
  # for richly perfused get value
  #from equation and add inactive tissue volumes
  vol <- getLifecourseRapidlyPerfusedVolume(age,gender)
  
  rpf_inact_vol <- sum(unlist(tissue_vol_list[rpf_inactive_tissues]))
  tissue_vol_list[["rpf"]]<-vol+rpf_inact_vol

  # for slowly perfused get value
  #from equation and add inactive tissue volumes
  vol <- getLifecourseSlowlyPerfusedVolume(age,gender,perf_frct)
  spf_inact_vol <- sum(unlist(tissue_vol_list[spf_inactive_tissues]))
  tissue_vol_list[["spf"]]<-vol+spf_inact_vol

  #print(tissue_vol_list[tissues])
  return(tissue_vol_list[tissues])

}

#' Get perfusion for tissues provided on the basis of age and gender
#'
#' @param age Age of the organism in years
#' @param gender The gender of the organism "M" for Male or "F" for Female
#' @param source source of the equation. defaults to "sciv" for scitovation
#' @param tissues List of tissues for which blood perfusion is needed. The
#'   tissues can be any of the following
#'   liver,
#'   fat,
#'   bone,
#'   brain,
#'   gonad,
#'   heart,
#'   intestine,
#'   kidney,
#'   lung,
#'   pancreas,
#'   skin,
#'   spleen,
#'   stomach,
#'   thymus,
#'   muscle,
#'   remaining.
#'  
#' @return list containing blood perfusion of tissues for the given age and
#' gender.
#' @export
getLifecourseTissuePerfusion<- function(age = 25, gender = "M", tissues = list(),source = "sciv"){

  if(is.na(age)) return(NA)
  if (source == "sciv"){
    #get scaling factor for cardiac output calculated as sum(qc) or from SCIV equation
    qc_scaling <- 1
  }else{
    qc_scaling <- 1.164749
  }


  tissue_perf_list <- NULL
  available_tissues <- c("fat","skin","muscle","bone","brain","lung",
                         "heart","gi","kidney","liver","rpf","spf")
  spf_tissues <- c("fat","skin","muscle")
  rpf_tissues <- available_tissues[!(available_tissues %in% c(spf_tissues,"rpf","spf"))]
  inactive_tissues <- dplyr::setdiff(available_tissues,tissues)
  rpf_inactive_tissues <- inactive_tissues[inactive_tissues %in% rpf_tissues]
  spf_inactive_tissues <- inactive_tissues[inactive_tissues %in% spf_tissues]

  if ("gi" %in% tissues){

    tissue_perf_list[["gi"]] <- (getLifecourseGutPerfusion(age,gender)) * qc_scaling
    tissue_perf_list[["liver_art"]]<- getLifecourseLiverArterialPerfusion(age,gender) * qc_scaling
    tissue_perf_list[["liver_ven"]]<- (tissue_perf_list[["liver_art"]] +
                                         tissue_perf_list[["gi"]])
  }else{
    tissue_perf_list[["liver_art"]]<- (getLifecourseLiverArterialPerfusion(age,gender)+
                                         getLifecourseGutPerfusion(age,gender)) * qc_scaling
    tissue_perf_list[["liver_ven"]]<- tissue_perf_list[["liver_art"]]
  }

  if (!("liver" %in% tissues)){
    rpf_inactive_tissues <- c(inactive_tissues,"liver")
    tissue_perf_list[["liver"]]<- getLifecourseLiverArterialPerfusion(age,gender)* qc_scaling
  }

  tissue_perf_list[["fat"]]<- getLifecourseAdiposePerfusion(age,gender)* qc_scaling
  tissue_perf_list[["bone"]]<- getLifecourseBonePerfusion(age,gender)* qc_scaling
  tissue_perf_list[["brain"]]<-getLifecourseBrainPerfusion(age,gender)* qc_scaling
  # tissue_perf_list[["gonad"]]<- getLifecourseGonadPerfusion(age,gender)* qc_scaling
  tissue_perf_list[["heart"]]<- getLifecourseHeartPerfusion(age,gender)* qc_scaling
  tissue_perf_list[["kidney"]]<-getLifecourseKidneyPerfusion(age,gender)* qc_scaling
  tissue_perf_list[["lung"]]<- getLifecourseLungPerfusion(age,gender)* qc_scaling
  # tissue_perf_list[["pancreas"]]<- getLifecoursePancreasPerfusion(age,gender)* qc_scaling
  tissue_perf_list[["skin"]]<- getLifecourseSkinPerfusion(age,gender)* qc_scaling
  # tissue_perf_list[["spleen"]]<- getLifecourseSpleenPerfusion(age,gender)* qc_scaling
  # tissue_perf_list[["thymus"]]<- getLifecourseThymusPerfusion(age,gender)* qc_scaling
  tissue_perf_list[["muscle"]]<- getLifecourseMusclePerfusion(age,gender)* qc_scaling

  # for rapidly perfused tissue flow
  rpf_perf <-  getLifecourseRichlyPerfusedTissuePerfusion(age,gender)* qc_scaling
  inact_perf <- sum(unlist(tissue_perf_list[rpf_inactive_tissues]))
  tissue_perf_list[["rpf"]]<- rpf_perf+inact_perf
  # for slowly perfused tissue flow
  spf_perf <-  getLifecourseSlowlyPerfusedTissuePerfusion(age,gender)* qc_scaling
  inact_perf <- sum(unlist(tissue_perf_list[spf_inactive_tissues]))
  tissue_perf_list[["spf"]]<- spf_perf+inact_perf

  if("liver" %in% tissues){
    tissues <- tissues[tissues != "liver"]
    tissues <- c(tissues,"liver_art","liver_ven")

  }
  return(tissue_perf_list[tissues])

}

#' Get average body weight using the life course equation
#' @description The function is used to calculate the average body weight in kgs for humans based on age and gender.
#'  The function uses life course equations developed by Scitovation.
#' @param age age in years
#' @param gender Either "M" for male or "F" for female
#' @return Body Weight in kgs
#' @export
getLifecourseBodyWeight <- function(age, gender){
  if(is.na(age)) return(NA)
  if(gender == "M") {
    return(getLifecourseBodyWeightMale(age))
  }else if(gender == "F") {
    return(getLifecourseBodyWeightFemale(age))
  }
}

#' Calculate body weight in kg of average human male using the lifecourse
#' equation
#' @description This function is not called directly by the user. See
#'   \code{getLifeCourseBodyWeight}
#' @param age age in years
#' @return Body weight in kg
getLifecourseBodyWeightMale <- function(age){
  mwbirth = 4
  mwchild = 28
  mwadult = 56
  half = 6
  k = 70
  theta = 14.85
  diff_bh_bw = -1.46
  mbw25 = 81.208

  wa = diff_bh_bw + theta
  lambda = log(k**2) / (2.0 * wa * mwadult)

  bw = NA

  if(age >= 0 && age < 26) {
    bw = mwbirth + (mwchild*age)/(half + age) + (mwadult/(1.0 + k*exp(-lambda*mwadult*age)))
  }else if(age >= 26) {
    months = age*12
    bw = ((2.19e-08 * months**3.0) - (1.22e-04 * months**2.0) + (0.120*months) + 50.824)*mbw25/76.6
  }

  return(bw)
}

#' Calculate body weight in kg of average human female using the lifecourse
#' equation
#' @description This function is not called directly by the user. See
#' \code{getLifeCourseBodyWeight}
#' @param age age in years
#' @return Body weight in kg
getLifecourseBodyWeightFemale <- function(age){
  mwbirth = 3.4
  mwchild = 22
  mwadult = 42.854
  wadult = 42.668
  half = 3
  mbwadult = 65.236
  k = 142
  lambda = 0.010279
  theta = 11.5
  h1 = 163

  bw = NA

  if(age >= 0 && age < 22.3333) {
    bw = 3.4 +(mwchild*age)/(3.0 + age) + (mwadult/(1.0 + 142.11536*exp(-0.01028*mwadult*age)))
  }else if(age >= 22.3333) {
    months = age*12
    bw = ((6.17*10^-09*months^3)-(9.68*10^-5*months^2)+(0.109*months)+43.790)*mbwadult/66.17
  }

  return(bw)
}

#' Get average body height using the life course equation
#' @description The function is used to calculate the average body height for humans based on age and gender.
#'  The function uses life course equations developed by Scitovation.
#' @param age age in years
#' @param gender Either "M" for male or "F" for female
#' @return Body Height
#' @export
getLifecourseBodyHeight <- function(age, gender){
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseBodyHeightMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseBodyHeightFemale(age))
  }
}

#' Calculate body height of average human male using the lifecourse
#' equation
#' @description This function is not called directly by the user.
#' @param age age in years
#' @return Body height
getLifecourseBodyHeightMale <- function(age){
  if(is.na(age)) return(NA)

  h1 = 177
  theta = 14.85
  s0 = 0.135
  s1 = 1.5
  c1 = 0.9
  d1 = 0.5
  mht = 168.6
  mbht3 = h1 - 2.0*(h1 - mht)/(exp(s0*(age - theta)) + exp(s1*(age - theta)))
  mbhtj3 = h1*(1.0 - 1.0/(1.0 + ((age + 0.75)/d1)^c1))

  bh = NA

  if(age >= 0 && age < 3) {
    bh = h1*(1.0 - 1.0/(1.0 + ((age + 0.75)/d1)^c1))*mbht3/mbhtj3
  } else if(age >= 3) {
    bh = 177.0 - 2.0*(177.0 - mht)/(exp(s0*(age - theta)) + exp(s1*(age - theta)))
  }

  return(bh)
}

#====================================================================================================
getLifecourseBodyHeightFemale <- function(age)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  h1 = 162.15
  theta = 11.2536
  s0 = 0.135
  s1 = 1.27
  mht = 151
  mbht = 94.2
  mbhtj = 94.81

  bh = NA

  if(age >= 0 && age <= 3) {
    bh = h1*(1 - 1/(1 + ((age + 0.75)/2.3)^0.7))*mbht/mbhtj
  } else if(age >= 3) {
    bh = h1 - 2*(h1 - mht)/(exp(s0*(age - theta)) + exp(s1*(age - theta)))
  }

  return(bh)
}

#====================================================================================================
getLifecourseBodySurfaceArea <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseBodySurfaceAreaMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseBodySurfaceAreaFemale(age))
  }
}

#====================================================================================================
getLifecourseBodySurfaceAreaMale <- function(age)
  #====================================================================================================
{
  mbw = getLifecourseBodyWeightMale(age)
  mbh = getLifecourseBodyHeightMale(age)

  bsa = exp(-3.751 + 0.422*log(mbh) + 0.515*log(mbw))

  return(bsa)
}

#====================================================================================================
getLifecourseBodySurfaceAreaFemale <- function(age)
  #====================================================================================================
{
  mbw = getLifecourseBodyWeightFemale(age)
  mbh = getLifecourseBodyHeightFemale(age)

  bsa = exp(-3.751 + 0.422*log(mbh) + 0.515*log(mbw))

  return(bsa)
}

#====================================================================================================
getLifecourseBodyMassIndex <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseBodyMassIndexMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseBodyMassIndexFemale(age))
  }
}

#====================================================================================================
getLifecourseBodyMassIndexMale <- function(age)
  #====================================================================================================
{
  mbw = getLifecourseBodyWeightMale(age)
  mbh = getLifecourseBodyHeightMale(age)

  bmi = mbw/((mbh/100)**2)

  return(bmi)
}

#====================================================================================================
getLifecourseBodyMassIndexFemale <- function(age)
  #====================================================================================================
{
  mbw = getLifecourseBodyWeightFemale(age)
  mbh = getLifecourseBodyHeightFemale(age)

  bmi = mbw/((mbh/100)**2)

  return(bmi)
}

#====================================================================================================
getLifecourseHematocrit <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseHematocritMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseHematocritFemale(age))
  }
}

#====================================================================================================
getLifecourseHematocritMale <- function(age)
  #====================================================================================================
{
  hct = NA;

  if(age >= 0 && age < 2) {
    hct = 0.359
  } else if(age >= 2) {
    hct = (((1.12815e-06)*(age**3.0)) - (1.72362e-04*(age**2.0)) + (8.15264e-03*age) + 0.327363)
  }

  return(hct)
}

#====================================================================================================
getLifecourseHematocritFemale <- function(age)
  #====================================================================================================
{
  hct = NA;

  # Note: this is same as male equation
  if(age >= 0 && age < 2) {
    hct = 0.359
  } else if(age >= 2) {
    hct = (((1.12815e-06)*(age**3.0)) - (1.72362e-04*(age**2.0)) + (8.15264e-03*age) + 0.327363)
  }

  return(hct)
}

#====================================================================================================
getLifecourseBloodVolume <- function(age, gender, source = "sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if (source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseBloodVolumeMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseBloodVolumeFemale(age))
    }
  }else if(source == "sciv"){
    if(gender == "M") {
      return(getLifecourseBloodVolumeMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseBloodVolumeFemaleSciV(age))
    }
  }


}

#====================================================================================================
getLifecourseBloodVolumeMale <- function(age)
  #====================================================================================================
{
  bsa = getLifecourseBodySurfaceAreaMale(age)
  vblood = 3.33*bsa - 0.81

  return(vblood)
}

getLifecourseBloodVolumeMaleSciV<- function(age){
  mbsa <- getLifecourseBodySurfaceAreaMale(age)
  hct <- getLifecourseHematocritMale(age)
  bw <- getLifecourseBodyWeightMale(age)
  #constants:
  a <- 10.0
  b <- 1.2082
  c <- 3.2869
  d <- 0.0
  e <- 1000.0
  vpls <-  (a^(b *log10(mbsa) + c)) * (1 + d) * ((1 - hct) / e)
  vbldc <- (vpls/bw)/(1-hct)
  vblood <- vbldc*bw
  return(vblood)
}

getLifecourseBloodVolumeFemaleSciV<- function(age){
  mbsa <- getLifecourseBodySurfaceAreaFemale(age)
  hct <- getLifecourseHematocritFemale(age)
  bw <- getLifecourseBodyWeightFemale(age)
  #constants:
  a <- 10.0
  b <- 1.2082
  c <- 3.2869
  e <- 1000.0
  #equations
  vpls <-  (a^(b *log10(mbsa) + c)) *((1 - hct) / e)
  vbldc <- (vpls/bw)/(1-hct)
  vblood <- vbldc*bw
  return(vblood)
}

#====================================================================================================
getLifecourseBloodVolumeFemale <- function(age)
  #====================================================================================================
{
  bsa = getLifecourseBodySurfaceAreaFemale(age)
  vblood = 2.66*bsa - 0.46

  return(vblood)
}

#====================================================================================================
getLifecourseAdiposeVolume <- function(age, gender, source = "sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(source=="bosgra"){
    if(gender == "M") {
      return(getLifecourseAdiposeVolumeMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseAdiposeVolumeFemaleSciV(age))
    }
  }else if(source == "sciv"){
    if(gender == "M") {
      return(getLifecourseAdiposeVolumeMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseAdiposeVolumeFemaleSciV(age))
    }
  }


}

#====================================================================================================
getLifecourseAdiposeVolumeMaleSciV <- function(age)
  #====================================================================================================
{
  vadipose = NA

  # Note, these are not the Bosgra equations.  Those eqns apparently have some errors,
  # so I'm using the Scitovation eqns here.
  mbw25 = 81.21
  mbmi25 = 25.92
  mbmi20 = 24.07

  bw = getLifecourseBodyWeightMale(age)
  bmi = getLifecourseBodyMassIndexMale(age)

  mvfatadult = 18.02
  mperfat20a = 22
  mperfat20b =  23.1

  if(age >= 0 && age < 20) {
    vadipose = bw*((2.9875*exp(-0.129*age) + 0.67)*bmi + 0.2635*age - 4.843)/100.0
  }
  else if(age >= 20) {
    vadipose = bw*(-5.33798*bmi + 0.11149*(bmi**2.0) + 0.09795*age + 85.24521)*(mperfat20a/mperfat20b)/100.0
  }

  return(vadipose)
}

#====================================================================================================
getLifecourseAdiposeVolumeFemaleSciV <- function(age)
  #====================================================================================================
{
  vadipose = NA

  # Note, these are not the Bosgra equations.  Those eqns apparently have some errors,
  # so I'm using the Scitovation eqns here.
  mbw25 = 81.21
  mbmi25 = 25.92
  mbmi20 = 24.07

  bw = getLifecourseBodyWeightFemale(age)
  bmi = getLifecourseBodyMassIndexFemale(age)

  mvfatadult = 24.15
  mperfat25a = 36.51
  mperfat25b =  37.32

  if(age >= 0 && age < 25) {
    vadipose = bw*((1.5334*exp(-0.103*age) + 0.67)*bmi + 0.6276*age + 1.0301)/100.0

  }
  else if(age >= 25) {
    vadipose = bw*(1.9224*bmi - 0.018517*(bmi**2.0) + 0.05537*age - 0.794894)*(mperfat25a/mperfat25b)/100.0

  }

  return(vadipose)
}


#====================================================================================================
getLifecourseBoneVolume <- function(age, gender, source = "sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if (source == "Bosgra"){
    if(gender == "M") {
      return(getLifecourseBoneVolumeMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseBoneVolumeFemale(age))
    }
  }else if (source == "sciv"){
    if(gender == "M") {
      return(getLifecourseBoneVolumeMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseBoneVolumeFemaleSciV(age))
    }
  }

}

#====================================================================================================
getLifecourseBoneVolumeMale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightMale(age)
  vbone = exp(0.0689)*((h/100)**2.67)
  return(vbone)
}

#====================================================================================================
getLifecourseBoneVolumeFemale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightFemale(age)
  vbone = exp(0.0689)*((h/100)**2.67)
  return(vbone)
}


getLifecourseBoneVolumeMaleSciV<- function(age){
  # calculated using linear interpolation
  x <- c(0,1,5,10,15,25)
  y <- c(0.05,0.15,0.34,0.63,1.08,1.17)
  vals <- approx(x,y,age,"linear",rule = 2)
  return(vals$y)
}

getLifecourseBoneVolumeFemaleSciV<- function(age){
  # calculated using linear interpolation
  x <- c(0,1,5,10,15,25)
  y <- c(0.5,0.15,0.34,0.63,1,0.9)
  vals <- approx(x,y,age,"linear",rule = 2)
  return(vals$y)
}

#====================================================================================================
getLifecourseBrainVolume <- function(age, gender, source = "sciv")
#====================================================================================================
{
  if(is.na(age)) return(NA)

  if(source=="bosgra"){
    if(gender == "M") {
      return(getLifecourseBrainVolumeMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseBrainVolumeFemale(age))
    }
  }else if(source=="sciv"){
    if(gender == "M") {
      return(getLifecourseBrainVolumeMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseBrainVolumeFemaleSciV(age))
    }
  }

}

#====================================================================================================
getLifecourseBrainVolumeMale <- function(age)
  #====================================================================================================
{
  v0 = 0.405
  vbrain = v0*(3.68 - 2.68*exp(-age/0.89))*(exp(-age/629))
  return(vbrain)
}

#====================================================================================================
getLifecourseBrainVolumeFemale <- function(age)
  #====================================================================================================
{
  v0 = 0.373
  vbrain = v0*(3.68 - 2.68*exp(-age/0.89))*(exp(-age/629))
  return(vbrain)
}

getLifecourseBrainVolumeMaleSciV <- function(age){
  #constants
  a <- 10.0
  b <- 0.315
  c <- 9.0
  d <- 6.92
  vbrain <- a*(age+b)/(c+d*age)
  return(vbrain)
}

getLifecourseBrainVolumeFemaleSciV <- function(age){
  #constants
  a <- 10.0
  b <- 0.315
  c <- 9.0
  d <- 6.92
  vbrain <- a*(age+b)/(c+d*age)
  return(vbrain)
}

#====================================================================================================
getLifecourseGonadVolume <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseGonadVolumeMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseGonadVolumeFemale(age))
  }
}
#====================================================================================================
getLifecourseGonadVolumeMale <- function(age)
  #====================================================================================================
{
  vgon = (3.3 + 53*(1 - exp(-(age/17.5)**5.4)))/1000 # Convert to kg
  return(vgon)
}

#====================================================================================================
getLifecourseGonadVolumeFemale <- function(age)
  #====================================================================================================
{
  vgon = (3.3 + 90*(1 - exp(-(age/16.8)**6.7)))/1000 # Convert to kg
  return(vgon)
}

#====================================================================================================
getLifecourseHeartVolume <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseHeartVolumeMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseHeartVolumeFemale(age))
  }
}

#====================================================================================================
getLifecourseHeartVolumeMale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightMale(age)
  vhrt = exp(-2.502)*((h/100)**2.13)
  return(vhrt)

}

#====================================================================================================
getLifecourseHeartVolumeFemale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightFemale(age)
  vhrt = exp(-2.502)*((h/100)**2.13)
  return(vhrt)
}
#the gut volume comes from internal gut equations. Will be used for the GI compartment in the model
# if volume for individual compartments is needed use bosgra equations for individual compartments
getLifecourseGutVolume <- function(age,gender){
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseGutVolumeMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseGutVolumeFemale(age))
  }

}

getLifecourseGutVolumeMale <- function(age){
  mbw <- getLifecourseBodyWeightMale(age)
  vfat <- getLifecourseAdiposeVolumeMaleSciV(age)
  lbw <- mbw - vfat
  vgut <- 0.021*lbw
  return(vgut)
}

getLifecourseGutVolumeFemale <- function(age){
  mbw <- getLifecourseBodyWeightFemale(age)
  vfat <- getLifecourseAdiposeVolumeFemaleSciV(age)
  lbw <- mbw - vfat
  vgut <- 0.027*lbw
  return(vgut)
}

#====================================================================================================
getLifecourseIntestineVolume <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseIntestineVolumeMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseIntestineVolumeFemale(age))
  }
}

#====================================================================================================
getLifecourseIntestineVolumeMale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightMale(age)
  vint = exp(-1.351)*((h/100)**2.47)
  return(vint)
}

#====================================================================================================
getLifecourseIntestineVolumeFemale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightFemale(age)
  vint = exp(-1.351)*((h/100)**2.47)
  return(vint)
}

#====================================================================================================
getLifecourseKidneyVolume <- function(age, gender, source = "sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)
  # remove this block once we have scitovation equations
  if (source == "sciv"){
    #warning("Scitovation equations not found, Using bosgra equations instead")
    source = "bosgra"
  }
  if(source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseKidneyVolumeMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseKidneyVolumeFemale(age))
    }
  }else if(source == "sciv"){
    stop("No Scitovation Equations found")
  }


}

#====================================================================================================
getLifecourseKidneyVolumeMale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightMale(age)
  vkid = exp(-2.306)*((h/100)**1.93)
  return(vkid)
}

#====================================================================================================
getLifecourseKidneyVolumeFemale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightFemale(age)
  vkid = exp(-2.306)*((h/100)**1.93)
  return(vkid)
}

#====================================================================================================
getLifecourseLiverVolume <- function(age, gender, source = "sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)
  if (source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseLiverVolumeMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseLiverVolumeFemale(age))
    }
  }else if(source == "sciv"){
    if(gender == "M") {
      return(getLifecourseLiverVolumeMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseLiverVolumeFemaleSciV(age))
    }
  }


}

#====================================================================================================
getLifecourseLiverVolumeMale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightMale(age)
  vliv = exp(-0.6786)*((h/100)**1.98)
  return(vliv)
}

#====================================================================================================
getLifecourseLiverVolumeFemale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightFemale(age)
  vliv = exp(-0.6786)*((h/100)**1.98)
  return(vliv)
}

getLifecourseLiverVolumeMaleSciV<- function(age){
  mbw <- getLifecourseBodyWeightMale(age)
  mbsa <- getLifecourseBodySurfaceAreaMale(age)
  if (age < 23){
    a <- 0.05012
    b <- 0.78
    vliv <- a*mbw^b
  }else{
    # mean adult (25y) liver weight
    mliv268 <- 0.0512*getLifecourseBodyWeightMale(25)^0.78
    # mean adult (25y) body surface area
    mbsa268 <- 2.01
    a <- 1.0728
    b <- 0.3457
    vliv <- (a * mbsa - b) * mliv268 / (a * mbsa268 - b)

  }
}

getLifecourseLiverVolumeFemaleSciV<- function(age){
  mbw <- getLifecourseBodyWeightFemale(age)
  mbsa <- getLifecourseBodySurfaceAreaFemale(age)
  if (age < 23){
    a <- 0.05012
    b <- 0.78
    vliv <- a*mbw^b
  }else{
    # mean adult (25y) liver weight
    mliv268 <- 0.0512*getLifecourseBodyWeightFemale(25)^0.78
    # mean adult (25y) body surface area
    mbsa268 <- 1.75
    a <- 1.0728
    b <- 0.3457
    vliv <- (a * mbsa - b) * mliv268 / (a * mbsa268 - b)

  }
}


#====================================================================================================
getLifecourseLungVolume <- function(age, gender,source = "bosgra")
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  # remove this block once we have scitovation equations
  if (source == "sciv"){
    # warning("Scitovation equations not found, Using bosgra equations instead")
    source = "bosgra"
  }

  if(source=="bosgra"){
    if(gender == "M") {
      return(getLifecourseLungVolumeMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseLungVolumeFemale(age))
    }
  }

}

#====================================================================================================
getLifecourseLungVolumeMale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightMale(age)
  vliv = exp(-2.092)*((h/100)**2.1)
  return(vliv)
}

#====================================================================================================
getLifecourseLungVolumeFemale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightFemale(age)
  vliv = exp(-2.092)*((h/100)**2.1)
  return(vliv)
}

#====================================================================================================
getLifecoursePancreasVolume <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecoursePancreasVolumeMale(age))
  }
  else if(gender == "F") {
    return(getLifecoursePancreasVolumeFemale(age))
  }
}

#====================================================================================================
getLifecoursePancreasVolumeMale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightMale(age)
  vpan = exp(-3.431)*((h/100)**2.43)
  return(vpan)
}

#====================================================================================================
getLifecoursePancreasVolumeFemale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightFemale(age)
  vpan = exp(-3.431)*((h/100)**2.43)
  return(vpan)
}

#====================================================================================================
getLifecourseSkinVolume <- function(age, gender,source = "sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)
  if(source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseSkinVolumeMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseSkinVolumeFemale(age))
    }
  }else if(source == "sciv"){
    if(gender == "M") {
      return(getLifecourseSkinVolumeMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseSkinVolumeFemaleSciV(age))
    }
  }

}

#====================================================================================================
getLifecourseSkinVolumeMale <- function(age)
  #====================================================================================================
{
  bsa = getLifecourseBodySurfaceAreaMale(age)
  vskn = exp(1.64*bsa - 1.93)
  return(vskn)
}

#====================================================================================================
getLifecourseSkinVolumeFemale <- function(age)
  #====================================================================================================
{
  bsa = getLifecourseBodySurfaceAreaFemale(age)
  vskn = exp(1.64*bsa - 1.93)
  return(vskn)
}

getLifecourseSkinVolumeMaleSciV <- function(age){
  mbsa <- getLifecourseBodySurfaceAreaMale(age) # in m^2
  mbsa <- mbsa*10000
  depth <- 0.15 #in cm
  vskin <- (mbsa*depth)/1000
  return(vskin)
}

getLifecourseSkinVolumeFemaleSciV <- function(age){
  mbsa <- getLifecourseBodySurfaceAreaFemale(age) # in m^2
  mbsa <- mbsa*10000
  depth <- 0.15 #in cm
  vskin <- (mbsa*depth)/1000
  return(vskin)
}

#====================================================================================================
getLifecourseSpleenVolume <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseSpleenVolumeMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseSpleenVolumeFemale(age))
  }
}

#====================================================================================================
getLifecourseSpleenVolumeMale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightMale(age)
  vspl = exp(-3.123)*((h/100)**2.16)
  return(vspl)
}

#====================================================================================================
getLifecourseSpleenVolumeFemale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightFemale(age)
  vspl = exp(-3.123)*((h/100)**2.16)
  return(vspl)
}

#====================================================================================================
getLifecourseStomachVolume <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseStomachVolumeMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseStomachVolumeFemale(age))
  }
}

#====================================================================================================
getLifecourseStomachVolumeMale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightMale(age)
  vsto = exp(-3.266)*((h/100)**2.45)
  return(vsto)
}

#====================================================================================================
getLifecourseStomachVolumeFemale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightFemale(age)
  vsto = exp(-3.266)*((h/100)**2.45)
  return(vsto)
}

#====================================================================================================
getLifecourseThymusVolume <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseThymusVolumeMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseThymusVolumeFemale(age))
  }
}

#====================================================================================================
getLifecourseThymusVolumeMale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightMale(age)
  vthy = 14*((7.1 - 6.1*exp(-age/11.9))*(0.14 + 0.86*exp(-age/10.3)))/1000 # convert to kg
  return(vthy)
}

#====================================================================================================
getLifecourseThymusVolumeFemale <- function(age)
  #====================================================================================================
{
  # Note, same as male eqn
  h = getLifecourseBodyHeightFemale(age)
  vthy = 14*((7.1 - 6.1*exp(-age/11.9))*(0.14 + 0.86*exp(-age/10.3)))/1000 # convert to kg
  return(vthy)
}

#====================================================================================================
getLifecourseRemainingVolume <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseRemainingVolumeMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseRemainingVolumeFemale(age))
  }
}

#====================================================================================================
getLifecourseRemainingVolumeMale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightMale(age)
  vrem = exp(-0.072)*((h/100)**1.95)
  return(vrem)
}

#====================================================================================================
getLifecourseRemainingVolumeFemale <- function(age)
  #====================================================================================================
{
  h = getLifecourseBodyHeightFemale(age)
  vrem = exp(-0.072)*((h/100)**1.95)
  return(vrem)
}

#====================================================================================================
getLifecourseRapidlyPerfusedVolume <- function(age, gender,source = "sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  # remove this block once we have bosgra equations
  if (source == "bosgra"){
    # warning("Bosgra equations not found, Using ScitoVation equations instead")
    source = "sciv"
  }
  if(source == "sciv"){
      if(gender == "M") {
        return(getLifecourseRapidlyPerfusedVolumeMaleSciV(age))
      }else if(gender == "F") {
        return(getLifecourseRapidlyPerfusedVolumeFemaleSciV(age))
      }
    }
}

getLifecourseRapidlyPerfusedVolumeMaleSciV<- function(age){
  vgut <- getLifecourseGutVolumeMale(age)
  vrpf <- 2.596*vgut
  return(vrpf)
}

getLifecourseRapidlyPerfusedVolumeFemaleSciV<- function(age){
  vgut <- getLifecourseGutVolumeFemale(age)
  vrpf <- 2.464*vgut
  return(vrpf)
}

#====================================================================================================
getLifecourseMuscleVolume <- function(age, gender,source="sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)
  if (source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseMuscleVolumeMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseMuscleVolumeFemale(age))
    }
  }else if (source == "sciv"){
    if (gender == "M"){
      return(getLifecourseMuscleVolumeMaleSciV(age))
    }else if (gender == "F"){
      return(getLifecourseMuscleVolumeFemaleSciV(age))
    }

  }


}

#====================================================================================================
getLifecourseMuscleVolumeMale <- function(age)
  #====================================================================================================
{
  vmus = getLifecourseBodyWeightMale(age) -
    getLifecourseAdiposeVolumeMaleSciV(age) -
    getLifecourseBloodVolumeMale(age) -
    getLifecourseBoneVolumeMale(age) -
    getLifecourseBrainVolumeMale(age) -
    getLifecourseGonadVolumeMale(age) -
    getLifecourseHeartVolumeMale(age) -
    getLifecourseIntestineVolumeMale(age) -
    getLifecourseKidneyVolumeMale(age) -
    getLifecourseLiverVolumeMale(age) -
    getLifecourseLungVolumeMale(age) -
    getLifecoursePancreasVolumeMale(age) -
    getLifecourseRemainingVolumeMale(age) -
    getLifecourseSkinVolumeMale(age) -
    getLifecourseSpleenVolumeMale(age) -
    getLifecourseStomachVolumeMale(age) -
    getLifecourseThymusVolumeMale(age)

  return(vmus)
}

#====================================================================================================
getLifecourseMuscleVolumeFemale <- function(age)
  #====================================================================================================
{
  vmus = getLifecourseBodyWeightMale(age) -
    getLifecourseAdiposeVolumeFemaleSciV(age) -
    getLifecourseBloodVolumeFemale(age) -
    getLifecourseBoneVolumeFemale(age) -
    getLifecourseBrainVolumeFemale(age) -
    getLifecourseGonadVolumeFemale(age) -
    getLifecourseHeartVolumeFemale(age) -
    getLifecourseIntestineVolumeFemale(age) -
    getLifecourseKidneyVolumeFemale(age) -
    getLifecourseLiverVolumeFemale(age) -
    getLifecourseLungVolumeFemale(age) -
    getLifecoursePancreasVolumeFemale(age) -
    getLifecourseRemainingVolumeFemale(age) -
    getLifecourseSkinVolumeFemale(age) -
    getLifecourseSpleenVolumeFemale(age) -
    getLifecourseStomachVolumeFemale(age) -
    getLifecourseThymusVolumeFemale(age)

  return(vmus)
}


#====================================================================================================
getLifecourseSlowlyPerfusedVolume <- function(age, gender,perf_frct=0.85)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if (gender == "M"){
    return(getLifecourseSlowlyPerfusedVolumeMale(age,perf_frct))
  }else if (gender == "F"){
    return(getLifecourseSlowlyPerfusedVolumeFemale(age,perf_frct))
  }
}

getLifecourseSlowlyPerfusedVolumeMale <- function(age,perf_frct=0.85){
  bw <- getLifecourseBodyWeightMale(age)
  perfused_bw <- perf_frct*bw
  total_volume <- (getLifecourseGutVolume(age,"M")+
                     getLifecourseIntestineVolume(age,"M")+
                     getLifecourseLiverVolume(age,"M")+
                     getLifecourseBloodVolume(age,"M")+
                     getLifecourseAdiposeVolume(age,"M")+
                     getLifecourseBoneVolume(age,"M")+
                     getLifecourseBrainVolume(age,"M")+
                     getLifecourseHeartVolume(age,"M")+
                     getLifecourseKidneyVolume(age,"M")+
                     getLifecourseLungVolume(age,"M")+
                     getLifecourseSkinVolume(age,"M")+
                     getLifecourseMuscleVolume(age,"M")+
                     getLifecourseRapidlyPerfusedVolume(age,"M"))
  vol <- perfused_bw- total_volume
  vol <- ifelse(vol < 0, 1e-5,vol)
  return(vol)

}

getLifecourseSlowlyPerfusedVolumeFemale <- function(age,perf_frct=0.85){
  bw <- getLifecourseBodyWeightFemale(age)
  perfused_bw <- perf_frct*bw
  total_volume <- (getLifecourseGutVolume(age,"F")+
                     getLifecourseIntestineVolume(age,"F")+
                     getLifecourseLiverVolume(age,"F")+
                     getLifecourseBloodVolume(age,"F")+
                     getLifecourseAdiposeVolume(age,"F")+
                     getLifecourseBoneVolume(age,"F")+
                     getLifecourseBrainVolume(age,"F")+
                     getLifecourseHeartVolume(age,"F")+
                     getLifecourseKidneyVolume(age,"F")+
                     getLifecourseLungVolume(age,"F")+
                     getLifecourseSkinVolume(age,"F")+
                     getLifecourseMuscleVolume(age,"F")+
                     getLifecourseRapidlyPerfusedVolume(age,"F"))
  vol <- perfused_bw- total_volume
  vol <- ifelse(vol < 0, 1e-5,vol)
  return(vol)

}
#====================================================================================================
getLifecourseMuscleVolumeMale <- function(age)
#====================================================================================================
{
  vmus = getLifecourseBodyWeightMale(age) -
    getLifecourseAdiposeVolumeMaleSciV(age) -
    getLifecourseBloodVolumeMale(age) -
    getLifecourseBoneVolumeMale(age) -
    getLifecourseBrainVolumeMale(age) -
    getLifecourseGonadVolumeMale(age) -
    getLifecourseHeartVolumeMale(age) -
    getLifecourseIntestineVolumeMale(age) -
    getLifecourseKidneyVolumeMale(age) -
    getLifecourseLiverVolumeMale(age) -
    getLifecourseLungVolumeMale(age) -
    getLifecoursePancreasVolumeMale(age) -
    getLifecourseRemainingVolumeMale(age) -
    getLifecourseSkinVolumeMale(age) -
    getLifecourseSpleenVolumeMale(age) -
    getLifecourseStomachVolumeMale(age) -
    getLifecourseThymusVolumeMale(age)

  return(vmus)
}

#====================================================================================================
getLifecourseMuscleVolumeFemale <- function(age)
  #====================================================================================================
{
  vmus = getLifecourseBodyWeightMale(age) -
    getLifecourseAdiposeVolumeFemaleSciV(age) -
    getLifecourseBloodVolumeFemale(age) -
    getLifecourseBoneVolumeFemale(age) -
    getLifecourseBrainVolumeFemale(age) -
    getLifecourseGonadVolumeFemale(age) -
    getLifecourseHeartVolumeFemale(age) -
    getLifecourseIntestineVolumeFemale(age) -
    getLifecourseKidneyVolumeFemale(age) -
    getLifecourseLiverVolumeFemale(age) -
    getLifecourseLungVolumeFemale(age) -
    getLifecoursePancreasVolumeFemale(age) -
    getLifecourseRemainingVolumeFemale(age) -
    getLifecourseSkinVolumeFemale(age) -
    getLifecourseSpleenVolumeFemale(age) -
    getLifecourseStomachVolumeFemale(age) -
    getLifecourseThymusVolumeFemale(age)

  return(vmus)
}
getLifecourseMuscleVolumeMaleSciV<- function(age){
  # calculated using linear interpolation
  x <- c(0,1,5,10,15,25)
  y <- c(0.8,1.9,5.6,11,24,29)
  vals <- approx(x,y,age,"linear",rule = 2)
  return(vals$y)
}

getLifecourseMuscleVolumeFemaleSciV<- function(age){
  # calculated using linear interpolation
  x <- c(0,1,5,10,15,25)
  y <- c(0.8,1.9,5.6,11,17,17.5)
  vals <- approx(x,y,age,"linear",rule = 2)
  return(vals$y)
}

#====================================================================================================
getLifecourseTissueVolumeSum <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseTissueVolumeSumMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseTissueVolumeSumFemale(age))
  }
}

#====================================================================================================
getLifecourseTissueVolumeSumMale <- function(age)
  #====================================================================================================
{
  vall = +
    getLifecourseAdiposeVolumeMaleSciV(age) +
    getLifecourseBloodVolumeMale(age) +
    getLifecourseBoneVolumeMale(age) +
    getLifecourseBrainVolumeMale(age) +
    getLifecourseGonadVolumeMale(age) +
    getLifecourseHeartVolumeMale(age) +
    getLifecourseIntestineVolumeMale(age) +
    getLifecourseKidneyVolumeMale(age) +
    getLifecourseLiverVolumeMale(age) +
    getLifecourseLungVolumeMale(age) +
    getLifecoursePancreasVolumeMale(age) +
    getLifecourseRemainingVolumeMale(age) +
    getLifecourseSkinVolumeMale(age) +
    getLifecourseSpleenVolumeMale(age) +
    getLifecourseStomachVolumeMale(age) +
    getLifecourseThymusVolumeMale(age) +
    getLifecourseMuscleVolumeMale(age)

  return(vall)
}

#====================================================================================================
getLifecourseTissueVolumeSumFemale <- function(age)
  #====================================================================================================
{
  vall = +
    getLifecourseAdiposeVolumeFemaleSciV(age) +
    getLifecourseBloodVolumeFemale(age) +
    getLifecourseBoneVolumeFemale(age) +
    getLifecourseBrainVolumeFemale(age) +
    getLifecourseGonadVolumeFemale(age) +
    getLifecourseHeartVolumeFemale(age) +
    getLifecourseIntestineVolumeFemale(age) +
    getLifecourseKidneyVolumeFemale(age) +
    getLifecourseLiverVolumeFemale(age) +
    getLifecourseLungVolumeFemale(age) +
    getLifecoursePancreasVolumeFemale(age) +
    getLifecourseRemainingVolumeFemale(age) +
    getLifecourseSkinVolumeFemale(age) +
    getLifecourseSpleenVolumeFemale(age) +
    getLifecourseStomachVolumeFemale(age) +
    getLifecourseThymusVolumeFemale(age) +
    getLifecourseMuscleVolumeFemale(age)

  return(vall)
}

#====================================================================================================
getLifecourseAdiposePerfusion <- function(age, gender, source = "sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseAdiposePerfusionMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseAdiposePerfusionFemale(age))
    }
  }else if(source == "sciv"){
    if(gender == "M") {
      return(getLifecourseAdiposePerfusionMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseAdiposePerfusionFemaleSciV(age))
    }
  }


}

getLifecourseAdiposePerfusionMaleSciV<- function(age){
  vfatc25 = 0.238
  qfatc25 = 0.05
  vfat <- getLifecourseAdiposeVolumeMaleSciV(age)
  bw <- getLifecourseBodyWeightMale(age)
  qc <- getLifecourseCardiacOutput(age,"M")
  vfatc <- vfat/bw
  qfatc <- (vfatc/vfatc25)*qfatc25
  qfat <- qfatc*qc
  return(qfat)
}

getLifecourseAdiposePerfusionFemaleSciV<- function(age){
  vfatc25 = 0.368
  qfatc25 = 0.085
  vfat <- getLifecourseAdiposeVolumeFemaleSciV(age)
  bw <- getLifecourseBodyWeightFemale(age)
  qc <- getLifecourseCardiacOutput(age,"F")
  vfatc <- vfat/bw
  qfatc <- (vfatc/vfatc25)*qfatc25
  qfat <- qfatc*qc
  return(qfat)
}

#====================================================================================================
getLifecourseAdiposePerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseAdiposeVolumeMaleSciV(age)
  m35 = getLifecourseAdiposeVolumeMaleSciV(35)
  q = m*19.5/m35
  return(q)
}

#====================================================================================================
getLifecourseAdiposePerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseAdiposeVolumeFemaleSciV(age)
  m35 = getLifecourseAdiposeVolumeFemaleSciV(35)
  q = m*30.1/m35
  return(q)
}

#====================================================================================================
getLifecourseBonePerfusion <- function(age, gender, source = "sciv")
#====================================================================================================
{
  if(is.na(age)) return(NA)

  if(source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseBonePerfusionMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseBonePerfusionFemale(age))
    }
  }else if(source == "sciv"){
    if(gender == "M") {
      return(getLifecourseBonePerfusionMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseBonePerfusionFemaleSciV(age))
    }
  }
}

getLifecourseBonePerfusionMaleSciV<- function(age){
  qbone25c <- 0.03
  qc <- getLifecourseCardiacOutput(age,"M")
  bw25 <- getLifecourseBodyWeightMale(25)
  bw <- getLifecourseBodyWeightMale(age)
  vbonec25 <- getLifecourseBoneVolumeMaleSciV(25)/bw25
  vbonec <- getLifecourseBoneVolumeMaleSciV(age)/bw
  qbone <- qc*(vbonec/vbonec25)*qbone25c
  return(qbone)
}

getLifecourseBonePerfusionFemaleSciV<- function(age){
  qbone25c <- 0.03
  qc <- getLifecourseCardiacOutput(age,"F")
  bw25 <- getLifecourseBodyWeightFemale(25)
  bw <- getLifecourseBodyWeightFemale(age)
  vbonec25 <- getLifecourseBoneVolumeFemaleSciV(25)/bw25
  vbonec <- getLifecourseBoneVolumeFemaleSciV(age)/bw
  qbone <- qc*(vbonec/vbonec25)*qbone25c
  return(qbone)
}
#====================================================================================================
getLifecourseBonePerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseBoneVolumeMale(age)
  m35 = getLifecourseBoneVolumeMale(35)
  q = m*7.8/m35
  return(q)
}

#====================================================================================================
getLifecourseBonePerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseBoneVolumeFemale(age)
  m35 = getLifecourseBoneVolumeFemale(35)
  q = m*7.1/m35
  return(q)
}

#====================================================================================================
getLifecourseBrainPerfusion <- function(age, gender,source = "sciv")
#====================================================================================================
{
  if(is.na(age)) return(NA)

  if(source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseBrainPerfusionMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseBrainPerfusionFemale(age))
    }
  }else if (source == "sciv"){
    if(gender == "M") {
      return(getLifecourseBrainPerfusionMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseBrainPerfusionFemaleSciV(age))
    }
  }
}

#====================================================================================================
getLifecourseBrainPerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseBrainVolumeMale(age)
  m35 = getLifecourseBrainVolumeMale(35)
  q = m*46.8/m35
  return(q)
}

#====================================================================================================
getLifecourseBrainPerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseBrainVolumeFemale(age)
  m35 = getLifecourseBrainVolumeFemale(35)
  q = m*42.5/m35
  return(q)
}

getLifecourseBrainPerfusionMaleSciV<- function(age){
  qc <- getLifecourseCardiacOutput(age,"M")
  qbrnc <- (3/age)*exp(-0.5*(log(age/41.92)/1.876)^2)
  qbrn <- qbrnc*qc
  return(qbrn)
}

getLifecourseBrainPerfusionFemaleSciV<- function(age){
  qc <- getLifecourseCardiacOutput(age,"F")
  qbrnc <- (3.064/age)*exp(-0.5*(log(age/47.57)/1.94)^2)
  qbrn <- qbrnc*qc
  return(qbrn)
}
#====================================================================================================
getLifecourseGonadPerfusion <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseGonadPerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseGonadPerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseGonadPerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseGonadVolumeMale(age)
  m35 = getLifecourseGonadVolumeMale(35)
  q = m*0.195/m35
  return(q)
}

#====================================================================================================
getLifecourseGonadPerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseGonadVolumeFemale(age)
  m35 = getLifecourseGonadVolumeFemale(35)
  q = m*0.071/m35
  return(q)
}

#====================================================================================================
getLifecourseHeartPerfusion <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseHeartPerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseHeartPerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseHeartPerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseHeartVolumeMale(age)
  m35 = getLifecourseHeartVolumeMale(35)
  q = m*15.6/m35
  return(q)
}

#====================================================================================================
getLifecourseHeartPerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseHeartVolumeFemale(age)
  m35 = getLifecourseHeartVolumeFemale(35)
  q = m*17.7/m35
  return(q)
}

#====================================================================================================
getLifecourseIntestinePerfusion <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseIntestinePerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseIntestinePerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseIntestinePerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseIntestineVolumeMale(age)
  m35 = getLifecourseIntestineVolumeMale(35)
  q = m*54.6/m35
  return(q)
}

#====================================================================================================
getLifecourseIntestinePerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseIntestineVolumeFemale(age)
  m35 = getLifecourseIntestineVolumeFemale(35)
  q = m*56.6/m35
  return(q)
}

#====================================================================================================
getLifecourseKidneyPerfusion <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseKidneyPerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseKidneyPerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseKidneyPerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseKidneyVolumeMale(age)
  m35 = getLifecourseKidneyVolumeMale(35)
  q = m*74.1/m35
  return(q)
}

#====================================================================================================
getLifecourseKidneyPerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseKidneyVolumeFemale(age)
  m35 = getLifecourseKidneyVolumeFemale(35)
  q = m*60.2/m35
  return(q)
}

#====================================================================================================
getLifecourseLiverArterialPerfusion <- function(age, gender, source = "sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseLiverArterialPerfusionMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseLiverArterialPerfusionFemale(age))
    }
  }else if (source == "sciv"){
    if(gender == "M") {
      return(getLifecourseLiverArterialPerfusionMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseLiverArterialPerfusionFemaleSciV(age))
    }
  }
}

#====================================================================================================
getLifecourseLiverArterialPerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseLiverVolumeMale(age)
  m35 = getLifecourseLiverVolumeMale(35)
  q = m*25.35/m35
  return(q)
}

#====================================================================================================
getLifecourseLiverArterialPerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseLiverVolumeFemale(age)
  m35 = getLifecourseLiverVolumeFemale(35)
  q = m*23.0/m35
  return(q)
}

getLifecourseLiverArterialPerfusionMaleSciV<- function(age){
  vliv <- getLifecourseLiverVolumeMaleSciV(age)
  bw <- getLifecourseBodyWeightMale(age)
  qc <- getLifecourseCardiacOutput(age,"M")
  vlivc <- vliv/bw
  qlivartc <- (vlivc/0.0199)*0.065
  qlivart <- qlivartc*qc
  return(qlivart)
}

getLifecourseLiverArterialPerfusionFemaleSciV<- function(age){
  vliv <- getLifecourseLiverVolumeFemaleSciV(age)
  bw <- getLifecourseBodyWeightFemale(age)
  qc <- getLifecourseCardiacOutput(age,"F")
  vlivc <- vliv/bw
  qlivartc <- (vlivc/0.0199)*0.065
  qlivart <- qlivartc*qc
  return(qlivart)
}

getLifecourseGutPerfusion<- function(age,gender){
  if(is.na(age)) return(NA)

  if(gender == "M"){
    return(getLifecourseGutPerfusionMale(age))
  }else if(gender == "F"){
    return(getLifecourseGutPerfusionFemale(age))
  }
}

getLifecourseGutPerfusionMale<- function(age){
  vgutc25 <- 0.016
  qgutc25 <- 0.15
  vgut <- getLifecourseGutVolumeMale(age)
  bw <- getLifecourseBodyWeightMale(age)
  qc <- getLifecourseCardiacOutput(age,"M")
  vgutc <- vgut/bw
  qgutc <- (vgutc/vgutc25)*qgutc25
  qgut <- qgutc *qc
  return(qgut)
}

getLifecourseGutPerfusionFemale<- function(age){
  vgutc25 <- 0.0170660419467526
  qgutc25 <- 0.17
  vgut <- getLifecourseGutVolumeFemale(age)
  bw <- getLifecourseBodyWeightFemale(age)
  qc <- getLifecourseCardiacOutput(age,"F")
  vgutc <- vgut/bw
  qgutc <- (vgutc/vgutc25)*qgutc25
  qgut <- qgutc *qc
  return(qgut)
}
#====================================================================================================
getLifecourseLiverSplancnicPerfusion <- function(age, gender)
#====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseLiverSplancnicPerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseLiverSplancnicPerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseLiverSplancnicPerfusionMale <- function(age)
  #====================================================================================================
{
  q = getLifecoursePancreasPerfusionMale(age) +
    getLifecourseSpleenPerfusionMale(age) +
    getLifecourseStomachPerfusionMale(age) +
    getLifecourseIntestinePerfusionMale(age)

  return(q)
}

#====================================================================================================
getLifecourseLiverSplancnicPerfusionFemale <- function(age)
  #====================================================================================================
{
  q = getLifecoursePancreasPerfusionFemale(age) +
    getLifecourseSpleenPerfusionFemale(age) +
    getLifecourseStomachPerfusionFemale(age) +
    getLifecourseIntestinePerfusionFemale(age)

  return(q)
}

#====================================================================================================
getLifecourseLiverTotalPerfusion <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseLiverTotalPerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseLiverTotalPerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseLiverTotalPerfusionMale <- function(age)
  #====================================================================================================
{
  q = getLifecourseLiverArterialPerfusionMale(age) +
    getLifecourseLiverSplancnicPerfusionMale(age)

  return(q)
}

#====================================================================================================
getLifecourseLiverTotalPerfusionFemale <- function(age)
  #====================================================================================================
{
  q = getLifecourseLiverArterialPerfusionFemale(age) +
    getLifecourseLiverSplancnicPerfusionFemale(age)

  return(q)
}

#====================================================================================================
getLifecourseMusclePerfusion <- function(age, gender, source = "sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)
  if(source == "sciv"){
    if(gender == "M") {
      return(getLifecourseMusclePerfusionMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseMusclePerfusionFemaleSciV(age))
    }
  }else if(source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseMusclePerfusionMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseMusclePerfusionFemale(age))
    }
  }


}

getLifecourseMusclePerfusionMaleSciV <- function(age){
  vmuscc25 <- getLifecourseMuscleVolumeMaleSciV(25)/getLifecourseBodyWeightMale(25)
  qmuscc25 <- 0.17
  vmusc <- getLifecourseMuscleVolumeMaleSciV(age)
  bw <- getLifecourseBodyWeightMale(age)
  qc <- getLifecourseCardiacOutput(age,"M")
  vmuscc <- vmusc/bw
  qmuscc <- (vmuscc/vmuscc25)*qmuscc25
  qmusc <- qmuscc *qc
  return(qmusc)
}

getLifecourseMusclePerfusionFemaleSciV <- function(age){
  vmuscc25 <- getLifecourseMuscleVolumeFemaleSciV(25)/getLifecourseBodyWeightFemale(25)
  qmuscc25 <- 0.12
  vmusc <- getLifecourseMuscleVolumeFemaleSciV(age)
  bw <- getLifecourseBodyWeightFemale(age)
  qc <- getLifecourseCardiacOutput(age,"F")
  vmuscc <- vmusc/bw
  qmuscc <- (vmuscc/vmuscc25)*qmuscc25
  qmusc <- qmuscc *qc
  return(qmusc)
}


#====================================================================================================
getLifecourseMusclePerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseMuscleVolumeMaleSciV(age)
  m35 = getLifecourseMuscleVolumeMaleSciV(35)
  q = m*66.3/m35
  return(q)
}

#====================================================================================================
getLifecourseMusclePerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseMuscleVolumeFemaleSciV(age)
  m35 = getLifecourseMuscleVolumeFemaleSciV(35)
  q = m*42.5/m35
  return(q)
}

#====================================================================================================
getLifecoursePancreasPerfusion <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecoursePancreasPerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecoursePancreasPerfusionFemale(age))
  }
}

#====================================================================================================
getLifecoursePancreasPerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecoursePancreasVolumeMale(age)
  m35 = getLifecoursePancreasVolumeMale(35)
  q = m*3.9/m35
  return(q)
}

#====================================================================================================
getLifecoursePancreasPerfusionFemale <- function(age)
#====================================================================================================
{
  m = getLifecoursePancreasVolumeFemale(age)
  m35 = getLifecoursePancreasVolumeFemale(35)
  q = m*3.54/m35
  return(q)
}

#====================================================================================================
getLifecourseSkinPerfusion <- function(age, gender, source = "sciv")
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if (source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseSkinPerfusionMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseSkinPerfusionFemale(age))
    }
  }else if(source == "sciv"){
    if(gender == "M") {
      return(getLifecourseSkinPerfusionMaleSciV(age))
    }
    else if(gender == "F") {
      return(getLifecourseSkinPerfusionFemaleSciV(age))
    }
  }
}

#====================================================================================================
getLifecourseSkinPerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseSkinVolumeMale(age)
  m35 = getLifecourseSkinVolumeMale(35)
  q = m*19.5/m35
  return(q)
}

getLifecourseSkinPerfusionMaleSciV<- function(age){
  vsknc25 <- getLifecourseSkinVolumeMaleSciV(25)/getLifecourseBodyWeightMale(25)
  qsknc25 <- 0.058
  vskn <- getLifecourseSkinVolumeMaleSciV(age)
  bw <- getLifecourseBodyWeightMale(age)
  qc <- getLifecourseCardiacOutput(age,"M")
  vsknc <- vskn/bw
  qsknc <- (vsknc/vsknc25)*qsknc25
  qskn <- qsknc *qc
  return(qskn)
}

getLifecourseSkinPerfusionFemaleSciV<- function(age){
  vsknc25 <- getLifecourseSkinVolumeFemaleSciV(25)/getLifecourseBodyWeightFemale(25)
  qsknc25 <- 0.058
  vskn <- getLifecourseSkinVolumeFemaleSciV(age)
  bw <- getLifecourseBodyWeightFemale(age)
  qc <- getLifecourseCardiacOutput(age,"F")
  vsknc <- vskn/bw
  qsknc <- (vsknc/vsknc25)*qsknc25
  qskn <- qsknc *qc
  return(qskn)
}

#====================================================================================================
getLifecourseSkinPerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseSkinVolumeFemale(age)
  m35 = getLifecourseSkinVolumeFemale(35)
  q = m*17.7/m35
  return(q)
}

#====================================================================================================
getLifecourseSpleenPerfusion <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseSpleenPerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseSpleenPerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseSpleenPerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseSpleenVolumeMale(age)
  m35 = getLifecourseSpleenVolumeMale(35)
  q = m*11.7/m35
  return(q)
}

#====================================================================================================
getLifecourseSpleenPerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseSpleenVolumeFemale(age)
  m35 = getLifecourseSpleenVolumeFemale(35)
  q = m*10.6/m35
  return(q)
}

#====================================================================================================
getLifecourseStomachPerfusion <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseStomachPerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseStomachPerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseStomachPerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseStomachVolumeMale(age)
  m35 = getLifecourseStomachVolumeMale(35)
  q = m*3.9/m35
  return(q)
}

#====================================================================================================
getLifecourseStomachPerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseStomachVolumeFemale(age)
  m35 = getLifecourseStomachVolumeFemale(35)
  q = m*3.54/m35
  return(q)
}

#====================================================================================================
getLifecourseThymusPerfusion <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseThymusPerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseThymusPerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseThymusPerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseThymusVolumeMale(age)
  m35 = getLifecourseThymusVolumeMale(35)
  q = m*0.39/m35
  return(q)
}

#====================================================================================================
getLifecourseThymusPerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseThymusVolumeFemale(age)
  m35 = getLifecourseThymusVolumeFemale(35)
  q = m*0.37/m35
  return(q)
}

#====================================================================================================
getLifecourseRemainingPerfusion <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseRemainingPerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseRemainingPerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseRemainingPerfusionMale <- function(age)
  #====================================================================================================
{
  m = getLifecourseRemainingVolumeMale(age)
  m35 = getLifecourseRemainingVolumeMale(35)
  q = m*21.8/m35
  return(q)
}

#====================================================================================================
getLifecourseRemainingPerfusionFemale <- function(age)
  #====================================================================================================
{
  m = getLifecourseRemainingVolumeFemale(age)
  m35 = getLifecourseRemainingVolumeFemale(35)
  q = m*20.5/m35
  return(q)
}

#====================================================================================================
getLifecourseLungPerfusion <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseLungPerfusionMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseLungPerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseLungPerfusionMale <- function(age)
  #====================================================================================================
{
  # Using data from Jongeneelen...
  # Their reported lung perfusion is not cardiac output,
  # but a small arterial flow (fraction of their reported CO)
  q0 = 390*0.03  # They report CO as 390 L/h, their perfusion fraction if 0.03
  m = getLifecourseRemainingVolumeMale(age)
  m35 = getLifecourseRemainingVolumeMale(35)
  q = m*q0/m35
  return(q)
}

#====================================================================================================
getLifecourseLungPerfusionFemale <- function(age)
  #====================================================================================================
{
  # Using data from Joneneelen...
  # Their reported lung perfusion is not cardiac output,
  # but a small arterial flow (fraction of their reported CO)
  q0 = 390*0.03  # They report CO as 390 L/h, their perfusion fraction if 0.03
  m = getLifecourseRemainingVolumeFemale(age)
  m35 = getLifecourseRemainingVolumeFemale(35)
  q = m*q0/m35
  return(q)
}

#===========
getLifecourseRichlyPerfusedTissuePerfusion<- function(age,gender){
  if (is.na(age)) return(NA)

  if(gender == "M"){
    getLifecourseRichlyPerfusedTissuePerfusionMale(age)
  }else if(gender == "F"){
    getLifecourseRichlyPerfusedTissuePerfusionFemale(age)
  }
}

getLifecourseRichlyPerfusedTissuePerfusionMale <- function(age){
  vrpfc25 <- 0.042
  qrpfc25 <- 0.215
  vrpf <- getLifecourseRapidlyPerfusedVolumeMaleSciV(age)
  bw <- getLifecourseBodyWeightMale(age)
  qc <- getLifecourseCardiacOutput(age,"M")
  vrpfc <- vrpf/bw
  qrpfc <- (vrpfc/vrpfc25)*qrpfc25
  qrpf <- qrpfc *qc
  return(qrpf)
}

getLifecourseRichlyPerfusedTissuePerfusionFemale <- function(age){
  vrpfc25 <- 0.042
  qrpfc25 <- 0.215
  vrpf <- getLifecourseRapidlyPerfusedVolumeFemaleSciV(age)
  bw <- getLifecourseBodyWeightFemale(age)
  qc <- getLifecourseCardiacOutput(age,"F")
  vrpfc <- vrpf/bw
  qrpfc <- (vrpfc/vrpfc25)*qrpfc25
  qrpf <- qrpfc *qc
  return(qrpf)
}

#===========
getLifecourseSlowlyPerfusedTissuePerfusion<- function(age,gender){
  if (is.na(age)) return(NA)

  if(gender == "M"){
    getLifecourseSlowlyPerfusedTissuePerfusionMale(age)
  }else if(gender == "F"){
    getLifecourseSlowlyPerfusedTissuePerfusionFemale(age)
  }
}

getLifecourseSlowlyPerfusedTissuePerfusionMale <- function(age){
  vspfc25 <- getLifecourseSlowlyPerfusedVolumeMale(25)/getLifecourseBodyWeightMale(25)
  qspfc25 <- 0.190 - 0.058 # 0.058 was fractional skin perfion
  vspf <- getLifecourseSlowlyPerfusedVolumeMale(age)
  bw <- getLifecourseBodyWeightMale(age)
  qc <- getLifecourseCardiacOutput(age,"M")
  vspfc <- vspf/bw
  qspfc <- (vspfc/vspfc25)*qspfc25
  qspf <- qspfc *qc
  return(qspf)
}

getLifecourseSlowlyPerfusedTissuePerfusionFemale <- function(age){
  vspfc25 <- getLifecourseSlowlyPerfusedVolumeFemale(25)/getLifecourseBodyWeightFemale(25)
  qspfc25 <- 0.190 - 0.058 # 0.058 was fractional skin perfion
  vspf <- getLifecourseSlowlyPerfusedVolumeFemale(age)
  bw <- getLifecourseBodyWeightFemale(age)
  qc <- getLifecourseCardiacOutput(age,"F")
  vspfc <- vspf/bw
  qspfc <- (vspfc/vspfc25)*qspfc25
  qspf <- qspfc *qc
  return(qspf)
}

#' Get the cardiac output based on the life course equations
#' @description Get the cardiac output for a specific age and gender. Uses either Bosgra or ScitoVation equations
#' @param age Age in years 
#' @param gender Gender can be either "M" for male or "F" for female
#' @param source Source of the equations. Either from Bosgra et al. "bosgra" or from ScitoVation publications as "sciv"
#' @param qc_var variability for cardiac output. Used to account for exercise level. Can only be used when source = "sciv"
#' @export
#' 
getLifecourseCardiacOutput <- function(age, gender, source = "sciv",qc_var= 0)
  #====================================================================================================
{
  if(is.na(age)) return(NA)
  if (source == "bosgra"){
    if(gender == "M") {
      return(getLifecourseCardiacOutputMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseCardiacOutputFemale(age))
    }
  }else if(source == "sciv"){
    if(gender == "M") {
      return(getLifecourseCardiacOutputMaleSciV(age,qc_var))
    }
    else if(gender == "F") {
      return(getLifecourseCardiacOutputFemaleSciV(age,qc_var))
    }
  }
}
#============================
getLifecourseCardiacOutputMaleSciV<- function(age,qc_var){
  mbsa <- getLifecourseBodySurfaceAreaMale(age)
  # scaling factor for body surface area to cardiac output
  bsa_qc_scaling <- 3.5
  #  QC min is variability factor for inter individual variation in QC,
  # can be also used to adjust cardiac output based on activity level
  qc <- mbsa*bsa_qc_scaling*(1+qc_var)*60
}

#============================
getLifecourseCardiacOutputFemaleSciV<- function(age,qc_var){
  mbsa <- getLifecourseBodySurfaceAreaFemale(age)
  # scaling factor for body surface area to cardiac output
  bsa_qc_scaling <- 3.5
  #  QC min is variability factor for inter individual variation in QC,
  # can be also used to adjust cardiac output based on activity level
  qc <- mbsa*bsa_qc_scaling*(1+qc_var)*60
}
#====================================================================================================
getLifecourseCardiacOutputMale <- function(age)
  #====================================================================================================
{
  q  = getLifecourseAdiposePerfusionMale(age) +
    getLifecourseBonePerfusionMale(age) +
    getLifecourseBrainPerfusionMale(age) +
    getLifecourseGonadPerfusionMale(age) +
    getLifecourseHeartPerfusionMale(age) +
    getLifecourseKidneyPerfusionMale(age) +
    getLifecourseLiverArterialPerfusionMale(age) +
    getLifecourseMusclePerfusionMale(age) +
    getLifecourseRemainingPerfusionMale(age) +
    getLifecourseSkinPerfusionMale(age) +
    getLifecourseThymusPerfusionMale(age) +
    getLifecourseIntestinePerfusionMale(age) +
    getLifecoursePancreasPerfusionMale(age) +
    getLifecourseSpleenPerfusionMale(age) +
    getLifecourseStomachPerfusionMale(age)

  return(q)
}

#====================================================================================================
getLifecourseCardiacOutputFemale <- function(age)
  #====================================================================================================
{
  q  = getLifecourseAdiposePerfusionFemale(age) +
    getLifecourseBonePerfusionFemale(age) +
    getLifecourseBrainPerfusionFemale(age) +
    getLifecourseGonadPerfusionFemale(age)+
    getLifecourseHeartPerfusionFemale(age) +
    getLifecourseKidneyPerfusionFemale(age) +
    getLifecourseLiverArterialPerfusionFemale(age) +
    getLifecourseMusclePerfusionFemale(age) +
    getLifecourseRemainingPerfusionFemale(age) +
    getLifecourseSkinPerfusionFemale(age) +
    getLifecourseThymusPerfusionFemale(age) +
    getLifecourseIntestinePerfusionFemale(age) +
    getLifecoursePancreasPerfusionFemale(age) +
    getLifecourseSpleenPerfusionFemale(age) +
    getLifecourseStomachPerfusionFemale(age)

  return(q)
}

#' Get the ventilation rate for a given age and gender
#' @description Uses life course equation to calculate a ventilation rate for a specific age and gender
#' @param age Age in years
#' @param gender Either "M" for Male for "F" for Female
#' @param activity Activity level. Can be "rest" or "light exercise"
#' @param source source of equations either "sciv" or "bosgra"
#' @export
getLifecourseVentilationRate <- function(age, gender,activity="rest",source="sciv")
#====================================================================================================
{
  if(is.na(age)) return(NA)

  if (source == "bosgra"){
    if (activity != "rest"){
      # warning(sprintf(
      #   "The source does not have valid equations for activity type '%s'.
      #   Using the default 'rest' activity instead",activity)
      # )
    }
    if(gender == "M") {
      return(getLifecourseVentilationRateMale(age))
    }
    else if(gender == "F") {
      return(getLifecourseVentilationRateFemale(age))
    }
  } else if(source == "sciv") {
    if(gender == "M"){
      return(getLifecourseVentilationRateMaleSCIV(age,activity))
    }else if(gender == "F"){
      return(getLifecourseVentilationRateFemaleSCIV(age,activity))
    }
  }


}
#=====
getLifecourseVentilationRateMaleSCIV <- function(age,activity){
  if (activity == "rest"){
   return(0.4493*exp(log(0.09919/0.4493)*exp(-0.175*age))*1000)
  }
}

#=======
getLifecourseVentilationRateFemaleSCIV<- function(age,activity){
  if (activity == "rest"){
    return(0.3255*exp(log(0.08771/0.3255)*exp(-0.3731*age))*1000)
  }
}

#====================================================================================================
getLifecourseVentilationRateMale <- function(age)
  #====================================================================================================
{
  co = getLifecourseCardiacOutputMale(age)
  qp = 0.96*co + 58
  return(qp)
}

#====================================================================================================
getLifecourseVentilationRateFemale <- function(age)
  #====================================================================================================
{
  co = getLifecourseCardiacOutputFemale(age)
  qp = 0.62*co + 82
  return(qp)
}

#'Get the Lung Dead Space for a given age and gender
#' @description Uses life course equation to calculate the lung dead space for a specific age and gender
#' @param age Age in years
#' @param gender Either "M" for Male for "F" for Female
#' @export
getLifecourseLungDeadSpace<- function(age,gender){
  if(is.na(age)) return(NA)

  if(gender == "M"){
    return(getLifecourseLungDeadSpaceMale(age))
  }else if(gender == "F"){
    return(getLifecourseLungDeadSpaceFemale(age))
  }
}

#=========
getLifecourseLungDeadSpaceMale <- function(age){
  return(0.17*exp(log(0.01371/0.17)*exp(-0.13*age)))
}

#=========
getLifecourseLungDeadSpaceFemale <- function(age){
  return(0.1274*exp(log(0.01345/0.1274)*exp(-0.1609*age)))
}

#' Get the tidal volume for a given age and gender
#' @description Uses life course equation to calculate the tidal volume for a specific age and gender
#' @param age Age in years
#' @param gender Either "M" for Male for "F" for Female
#' @param activity Activity level. Can be "rest" or "light activity"
#' @export
getLifecourseTidalVolume <- function(age,gender,activity ="rest"){
  if(is.na(age)) return(NA)

  if (gender=="M"){
    return(getLifecourseTidalVolumeMale(age,activity))
  }else if(gender=="F"){
    return(getLifecourseTidalVolumeFemale(age,activity))
  }
}

#======
getLifecourseTidalVolumeMale<- function(age,activity){
  if (activity == "rest"){
    return(0.6842*exp(log(0.04306/0.6842)*exp(-0.1357*age)))
  }else if(activity == "light exercise"){
    if(age <= 1){
      return(1.661*exp(log(0.07428/1.661)*exp(-0.1095*age)))
    }else{
      return(0.1041+(age^2.727)*(1.478-0.1041)/(age^2.727+12.23^2.727))
    }
  }
}
#=======
getLifecourseTidalVolumeFemale<- function(age,activity){
  if (activity == "rest"){
    return(0.02712+(age^0.9173)*(0.9599-0.02712)/(age^0.9173+27.01^0.9173))
  }else if(activity == "light exercise"){
    if (age <= 1){
      return(1.159*exp(log(0.07248/1.159)*exp(-0.1362*age)))
    }else{
      return(0.1036+(age^3.064)*(1.055-0.1036)/(age^3.064+9.58^3.064))
    }
  }
}

#' Get the urine production rate for a given age and gender
#' @description Uses life course equation to calculate a urine production rate for a specific age and gender
#' @param age Age in years
#' @param gender Either "M" for Male for "F" for Female
#' @export
getLifecourseUrineProductionRate <- function(age, gender)
#====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseUrineProductionRateMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseUrineProductionRateFemale(age))
  }
}

#====================================================================================================
getLifecourseUrineProductionRateMale <- function(age)
  #====================================================================================================
{
  ur = -2.96538E-06*age^4 + 7.71852E-04*age^3 - 7.54327E-02*age^2 + 3.04687E+00*age + 1.26931E+01
  return(ur)
}

#====================================================================================================
getLifecourseUrineProductionRateFemale <- function(age)
#====================================================================================================
{
  ur = 1.59275E-04*age^3 - 3.09917E-02*age^2 + 1.70013E+00*age + 2.07921E+01
  return(ur)
}

#' Get the Glomerular Filteration Rate for a given age and gender
#' @description Uses life course equation to calculate a GFR for a specific age and gender
#' @param age Age in years
#' @param gender Either "M" for Male for "F" for Female
#' @export
getLifecourseGlomerularFiltrationRate <- function(age, gender)
  #====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == "M") {
    return(getLifecourseGlomerularFiltrationRateMale(age))
  }
  else if(gender == "F") {
    return(getLifecourseGlomerularFiltrationRateFemale(age))
  }
}

#====================================================================================================
getLifecourseGlomerularFiltrationRateMale <- function(age)
#====================================================================================================
{
  gfr = NA

  bw     = getLifecourseBodyWeightMale(age)
  mbw10d = getLifecourseBodyWeightMale(1/365)
  mbw1   = getLifecourseBodyWeightMale(1)

  bsa    = getLifecourseBodySurfaceAreaMale(age)
  bsa1   = getLifecourseBodySurfaceAreaMale(1)
  bsa30  = getLifecourseBodySurfaceAreaMale(30)


  gfr_bsa = 7.62

  days = age/365

  if(age >= 0 && age <= 1/365)  # 1 day
  {
    gfr = 0.12 * (1.0 + 1.0) * (-0.0013 * mbw10d**2.0 + 0.2025 * mbw10d - 0.1363) * (gfr_bsa * bsa1 / 1.73) / ((-0.0013 * mbw1**2.0 + 0.2025 * mbw1 - 0.1363) * (0.48 + (0.0571 * (10.0 - 3.0))))
  }
  else if (age > 1/365 && age <= 3/365) # 3 days
  {
    gfr = 0.12 * (1.0 + days) * (-0.0013 * mbw10d**2.0 + 0.2025 * mbw10d - 0.1363) * (gfr_bsa * bsa1 / 1.73) / ((-0.0013 * mbw1**2.0 + 0.2025 * mbw1 - 0.1363) * (0.48 + (0.0571 * (10.0 - 3.0))))
  }
  else if (age > 3/365 && age <= 10/365) # 10 days
  {
    gfr = (0.48 + (0.0571 * (days - 3))) * (-0.0013 * mbw10d**2.0 + 0.2025 * mbw10d - 0.1363) * (gfr_bsa * bsa1 / 1.73) / ((-0.0013 * mbw1**2.0 + 0.2025 * mbw1 - 0.1363) * (0.48 + (0.0571 * (10.0 - 3.0))))
  }
  else if (age > 10/365 && age <= 1)
  {
    gfr = (-0.0013 * bw**2.0 + 0.2025 * bw - 0.1363) * (gfr_bsa * bsa1 / 1.73) / (-0.0013 * mbw1**2.0 + 0.2025 * mbw1 - 0.1363)
  }
  else if (age > 1 && age <= 30)
  {
    gfr = gfr_bsa * bsa / 1.73
  }
  else if (age > 30)
  {
    gfr = gfr_bsa * bsa30 / 1.73 - (0.0066 * (7.62 * bsa30 / 1.73) * (age - 30))
  }

  return(gfr)
}

#====================================================================================================
getLifecourseGlomerularFiltrationRateFemale <- function(age)
#====================================================================================================
{
  gfr = NA

  bw     = getLifecourseBodyWeightFemale(age)
  mbw10d = getLifecourseBodyWeightFemale(1/365)
  mbw1   = getLifecourseBodyWeightFemale(1)

  bsa    = getLifecourseBodySurfaceAreaFemale(age)
  bsa1   = getLifecourseBodySurfaceAreaFemale(1)
  bsa30  = getLifecourseBodySurfaceAreaFemale(30)


  gfr_bsa = 7.62

  days = age/365

  if(age >= 0 && age <= 1/365)  # 1 day
  {
    gfr = 0.12 * (1.0 + 1.0) * (-0.0013 * mbw10d**2.0 + 0.2025 * mbw10d - 0.1363) * (gfr_bsa * bsa1 / 1.73) / ((-0.0013 * mbw1**2.0 + 0.2025 * mbw1 - 0.1363) * (0.48 + (0.0571 * (10.0 - 3.0))))
  }
  else if (age > 1/365 && age <= 3/365) # 3 days
  {
    gfr = 0.12 * (1.0 + days) * (-0.0013 * mbw10d**2.0 + 0.2025 * mbw10d - 0.1363) * (gfr_bsa * bsa1 / 1.73) / ((-0.0013 * mbw1**2.0 + 0.2025 * mbw1 - 0.1363) * (0.48 + (0.0571 * (10.0 - 3.0))))
  }
  else if (age > 3/365 && age <= 10/365) # 10 days
  {
    gfr = (0.48 + (0.0571 * (days - 3))) * (-0.0013 * mbw10d**2.0 + 0.2025 * mbw10d - 0.1363) * (gfr_bsa * bsa1 / 1.73) / ((-0.0013 * mbw1**2.0 + 0.2025 * mbw1 - 0.1363) * (0.48 + (0.0571 * (10.0 - 3.0))))
  }
  else if (age > 10/365 && age <= 1)
  {
    gfr = (-0.0013 * bw**2.0 + 0.2025 * bw - 0.1363) * (gfr_bsa * bsa1 / 1.73) / (-0.0013 * mbw1**2.0 + 0.2025 * mbw1 - 0.1363)
  }
  else if (age > 1 && age <= 30)
  {
    gfr = gfr_bsa * bsa / 1.73
  }
  else if (age > 30)
  {
    gfr = gfr_bsa * bsa30 / 1.73 - (0.0062 * (7.62 * bsa30 / 1.73) * (age - 30))
  }

  return(gfr)
}

# Valid values for gender as required by below functions
PLETHEM_GENDER_MALE   = 1
PLETHEM_GENDER_FEMALE = 2

# Small utility function for linear interpolation
interp2 <- function(x1, x2, y1, y2, x)
{
  m = (y2 - y1)/(x2 - x1)
  y = y1 + m*(x - x1)
  return(y)
}

#====================================================================================================
getLifecourseBodyWeight <- function(age, gender)
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseBodyWeightMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseBodyWeightFemale(age))
  }
}

#====================================================================================================
getLifecourseBodyWeightMale <- function(age)
#====================================================================================================
{
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

#====================================================================================================
getLifecourseBodyWeightFemale <- function(age)
#====================================================================================================
{
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

#====================================================================================================
getLifecourseBodyHeight <- function(age, gender)
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseBodyHeightMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseBodyHeightFemale(age))
  }
}

#====================================================================================================
getLifecourseBodyHeightMale <- function(age)
#====================================================================================================
{
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseBodySurfaceAreaMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseBodyMassIndexMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseHematocritMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
getLifecourseBloodVolume <- function(age, gender)
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseBloodVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseBloodVolumeFemale(age))
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


#====================================================================================================
getLifecourseBloodVolumeFemale <- function(age) 
#====================================================================================================
{
  bsa = getLifecourseBodySurfaceAreaFemale(age)
  vblood = 2.66*bsa - 0.46  
  
  return(vblood)
}

#====================================================================================================
getLifecourseAdiposeVolume <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseAdiposeVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseAdiposeVolumeFemale(age))
  }
}

#====================================================================================================
getLifecourseAdiposeVolumeMale <- function(age) 
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
getLifecourseAdiposeVolumeFemale <- function(age) 
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
getLifecourseBoneVolume <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseBoneVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseBoneVolumeFemale(age))
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

#====================================================================================================
getLifecourseBrainVolume <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseBrainVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseBrainVolumeFemale(age))
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

#====================================================================================================
getLifecourseGonadVolume <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseGonadVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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

  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseHeartVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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

#====================================================================================================
getLifecourseIntestineVolume <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)

  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseIntestineVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
getLifecourseKidneyVolume <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseKidneyVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseKidneyVolumeFemale(age))
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
getLifecourseLiverVolume <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseLiverVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseLiverVolumeFemale(age))
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

#====================================================================================================
getLifecourseLungVolume <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseLungVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseLungVolumeFemale(age))
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecoursePancreasVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
getLifecourseSkinVolume <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseSkinVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseSkinVolumeFemale(age))
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

#====================================================================================================
getLifecourseSpleenVolume <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseSpleenVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseStomachVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseThymusVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseRemainingVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
getLifecourseMuscleVolume <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseMuscleVolumeMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseMuscleVolumeFemale(age))
  }
}

#====================================================================================================
getLifecourseMuscleVolumeMale <- function(age) 
#====================================================================================================
{
  vmus = getLifecourseBodyWeightMale(age) -
        getLifecourseAdiposeVolumeMale(age) -
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
        getLifecourseAdiposeVolumeFemale(age) -
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
getLifecourseTissueVolumeSum <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseTissueVolumeSumMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseTissueVolumeSumFemale(age))
  }
}

#====================================================================================================
getLifecourseTissueVolumeSumMale <- function(age) 
#====================================================================================================
{
    vall = +
    getLifecourseAdiposeVolumeMale(age) +
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
    getLifecourseAdiposeVolumeFemale(age) +
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
getLifecourseAdiposePerfusion <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseAdiposePerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseAdiposePerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseAdiposePerfusionMale <- function(age) 
#====================================================================================================
{
  m = getLifecourseAdiposeVolumeMale(age)
  m35 = getLifecourseAdiposeVolumeMale(35)
  q = m*19.5/m35 
  return(q)
}

#====================================================================================================
getLifecourseAdiposePerfusionFemale <- function(age) 
#====================================================================================================
{
  m = getLifecourseAdiposeVolumeFemale(age)
  m35 = getLifecourseAdiposeVolumeFemale(35)
  q = m*30.1/m35
  return(q)
}

#====================================================================================================
getLifecourseBonePerfusion <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseBonePerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseBonePerfusionFemale(age))
  }
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
getLifecourseBrainPerfusion <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseBrainPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseBrainPerfusionFemale(age))
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

#====================================================================================================
getLifecourseGonadPerfusion <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseGonadPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseHeartPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseIntestinePerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseKidneyPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
getLifecourseLiverArterialPerfusion <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseLiverArterialPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseLiverArterialPerfusionFemale(age))
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

#====================================================================================================
getLifecourseLiverSplancnicPerfusion <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseLiverSplancnicPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseLiverTotalPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
getLifecourseMusclePerfusion <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseMusclePerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseMusclePerfusionFemale(age))
  }
}

#====================================================================================================
getLifecourseMusclePerfusionMale <- function(age) 
#====================================================================================================
{
  m = getLifecourseMuscleVolumeMale(age)
  m35 = getLifecourseMuscleVolumeMale(35)
  q = m*66.3/m35
  return(q)
}

#====================================================================================================
getLifecourseMusclePerfusionFemale <- function(age) 
#====================================================================================================
{
  m = getLifecourseMuscleVolumeFemale(age)
  m35 = getLifecourseMuscleVolumeFemale(35)
  q = m*42.5/m35
  return(q)
}

#====================================================================================================
getLifecoursePancreasPerfusion <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecoursePancreasPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
getLifecourseSkinPerfusion <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseSkinPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseSkinPerfusionFemale(age))
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseSpleenPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseStomachPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseThymusPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseRemainingPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseLungPerfusionMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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


#====================================================================================================
getLifecourseCardiacOutput <- function(age, gender)
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseCardiacOutputMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseCardiacOutputFemale(age))
  }
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

#====================================================================================================
getLifecourseVentilationRate <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseVentilationRateMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
    return(getLifecourseVentilationRateFemale(age))
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

#====================================================================================================
getLifecourseUrineProductionRate <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseUrineProductionRateMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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

#====================================================================================================
getLifecourseGlomerularFiltrationRate <- function(age, gender) 
#====================================================================================================
{
  if(is.na(age)) return(NA)
  
  if(gender == PLETHEM_GENDER_MALE) {
    return(getLifecourseGlomerularFiltrationRateMale(age))
  }
  else if(gender == PLETHEM_GENDER_FEMALE) {
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

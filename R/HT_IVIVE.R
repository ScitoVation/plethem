# All functions in this script perform HT IVIVE
# They either use PLETHEM UI, PLETHEM CL, or HTTK UI

#' High Throughput reverse dosimetry calculation using UI
#' @description Main function called from PLETHEM UI to run HT- Reverse Dosimetry
#' @param vals values passed from the PLETHEM UI
#' @return List of oral equivalent dose, steady state plasma concentration and steady state
#' for each name in vals
#' @export
runPlthemHTIVIVE<- function(vals){
  row_keys <- names(vals)
  num_rows <- length(row_keys)
  #print(row_keys)
  result <- lapply(vals,preprocessUIData)
  return(result)
}

#'Main function to run HT-Reverse Dosimetry from command line
#'@param chemical list of chemicals
#'@param physiological List of lists for physiological parameter
#'@param type list of type of exposure to be assumed
#'@param POD  list of invitro point of depature in um
#'@param hepatic list of lists for hepatic clearance
#'@param renal list of TRUE/FALSE if renal clearance should be assumed
#'@param plasma list of plasma clearance values
#'
perform_HTIVIVE <- function(chemical, physiological,type, POD, hepatic,renal, plasma){
  return(NULL)
}

#'preprocess value list from UI
#'@description The function converts the data from the UI to standard units of liters/h for clearances
#'and mg/L for concentrations. This function is not available to the end user as it relied on UI names
#'@param val list containing data for each row from the UI
#'@return list of cleranace values for point of departure values and type of reverse dosimetry
preprocessUIData<- function(val){
  #print(val)
  # get chemical properties
  chem_data <- getChemDataFromDb(val$sel_chem)

  casn <- chem_data$casn
  mw <- chem_data$mw
  km <- val$num_km
  # get POD
  pod <- val$num_ivc
  pod_unit <- val$sel_ivunit
  if (pod_unit=="um"){
    pod <- pod*mw/1000
  }
  org <- "human"#ifelse(val$sel_org=="ha","human","rat")
  age <- 25#ifelse(val$sel_org=="ha",25,52)
 
  liver_wt <- val$num_lw
  bw <- val$num_bw
  hpgl <- val$num_hpgl
  km <- val$num_km
  mpcppgl <- c(val$num_mppgl,val$num_cppgl)
  # get hepatic clerance type
  hepcl_type <- val$tab_heptype
  scaled_hepcl <- switch(hepcl_type,
                         "hep_sc"=calculateScaledSCClearance(c(val$num_mscl,val$num_cycl),
                                                             c(val$sel_msunit,val$sel_cyunit),
                                                             org,age,liver_wt,
                                                             km,mpcppgl,
                                                             return_total = T),
                         "hep_s9"=calculateScaledS9Clearance(val$num_s9cl,val$sel_s9unit,
                                                             org,age,liver_wt,
                                                             km,mpcppgl),
                         "hep_whole"=calculateScaledWholeHepClearance(val$num_whcl,
                                                                      val$sel_whunit,
                                                                      liver_wt,hpgl,km),
                         "hep_recomb"=calculateRecombClearance(
                           list("CYP1A2"=val$num_cyp1a2cl,
                                "CYP2B6"=val$num_cyp2b6cl,
                                "CYP3A4"=val$num_cyp3a4cl,
                                "CYP2C19"=val$num_cyp2c19cl,
                                "CYP2C9"=val$num_cyp2c9cl,
                                "CYP3A5"=val$num_cyp3a5cl,
                                "CES1M"=val$num_ces1mcl,
                                "CES1C" =val$num_ces1ccl,
                                "CES2M"=val$num_ces2mcl,
                                "CES2C"=val$num_ces2ccl
                           ),org,age,liver_wt),
                         0
  )
  #print(scaled_hepcl)
  #scaled_hepcl_bw <- signif(scaled_hepcl*km/(bw^0.75),4)
  # calculate renal clearance
  scaled_rencl <- ifelse(val$ch_rencl,val$num_gfr*val$num_fup,0)

  #calculate scaled Plasma clearance
  scaled_plcl <- 0 # set it to zero till I have more clarification on units and their relation
  #get clearance scaling type
  cl_type <- val$rdo_cltype
  ql <- val$num_ql
  qalv <- getLifecourseVentilationRate(25,"M")
  qc <- val$num_qc
  fup <- val$num_fup
  bw<- val$num_bw
  pair <- val$num_pair
  dose <- 1
  # get the type of HTIVIVE
  ht_type <- val$rdo_rdtype
  ss_concentration <- switch(ht_type,
                             "oralnonvol"=calculateOralNonvolCss(dose,bw,ql,qc,fup,cl_type,
                                                                   scaled_hepcl,scaled_rencl,
                                                                   scaled_plcl),
                             "oralvol"=calculateOralVolCss(dose,bw,ql,qc,pair,scaled_hepcl,
                                                           scaled_plcl),
                             "inhvol"=calculateInhVolCss(dose,ql,qalv,pair,
                                                          scaled_hepcl,scaled_plcl),
                             stop("Invalid IVIVE type")
  )
  equivalent_dose <- pod/ss_concentration
  #equivalent_dose <- getEquivalentDose(ss_concentration)
  calcualted_vals_list <- list("hep"=signif(scaled_hepcl,4),
                               "ren"=signif(scaled_rencl,4),
                               "pls"=signif(scaled_plcl,4),
                               "css"=signif(ss_concentration,4),
                               "eqdose"= signif(equivalent_dose,4))

  existing_vals_list <- list()#getValsMetadata(vals)
  return(calcualted_vals_list)
}

#' Calculate scaled in-vivo clearance if in-vitro clearance is meausured using Sub-cellular components
#' @description In-vitro clearance is measured in microsomal and cytosolic fractions. The function uses measured in-vitro clearance
#' and physiological parameters to estimate in-vivo clearance. This function is not called directly by the user
#' @param clearance vector of (microsomal,cytosolic) clearance
#' @param units vector of (microsomal,cytosolic) clearance units
#' @param organism name of organism, "human" or "rat"
#' @param age age of organism in "years" if "human" or "weeks" if "rat"
#' @param liver_wt liver weight(kg)
#' @param return_total logical, type of value to be returned,
#' @return  Total scaled invivo clearance in L/h if return_total is TRUE, else a named list of individual clearances
calculateScaledSCClearance <- function(clearance,units,organism="human",
                                       age=NULL,liver_wt,km=1,mpcppgl = list(),return_total = T){
  if (length(mpcppgl) == 0){
    # get MPPGL and CPPGL
    if (organism == "human"){
      MPCPPGL <- calcMPCPPGL(age)
      MPCPPGL <- unlist(MPCPPGL)
      names(MPCPPGL)<- NULL
    }else if(organim == "rat"){
      MPCPPGL <- unlist(mpcppgl)
    }
  }else{
    MPCPPGL <- unlist(mpcppgl)
  }
  
  if (units[1] == "ummmP"){
    clearance <- clearance/km
    volume_multiplier <- 1
  }else{
    # bsaed on units for the clearance get the volume multiplier vector
    #i.e. multiply clerance by 10^-3 if expressed in mL or 10^-6 if expressed in uL
    volume_multiplier <- ifelse(substr(units,1,2)=="ml",10^-3,10^-6)
  }
  # based on units for the clearance get the time multiplier vector
  # i.e. multiply clearance by 60 if measured in uL/min/mg Protein
  time_multiplier <- ifelse(units %in% c("ulmmP","mlmmP","ummmP"),60,1)
  # convert clerance from unit in UI to L/h
  # need to convert ml to Liters
  # mg protein to gram liver (MPPGL)
  # if rate is per minutes then convert to per hour (time_multiplier)
  # L/h/g liver to L/h by using liver weight in kg
  temp_scaled_cl <- clearance*volume_multiplier*time_multiplier*MPCPPGL
  scaled_cl <- temp_scaled_cl*liver_wt*1000
  total_hepcl <- sum(scaled_cl)
  if (return_total){
    return(total_hepcl)
  }else{
    names(scaled_cl)<-c("MPPGL","CPPGL")
    return(scaled_cl)
  }

}

#' Calculate scaled in-vivo clearance if in-vitro clearance is meausured using S9 Fraction
#' @description In-vitro clearance is measured in S9 fraction. The function uses measured in-vitro clearance
#' and physiological parameters to estimate in-vivo clearance. This function is not called directly by the user
#' @param clearance number for measured S9 clearance
#' @param units string for S9 clearance units
#' @param organism name of organism, "human" or "rat"
#' @param age age of organism in "years" if "human" or "weeks" if "rat"
#' @param liver_wt liver weight(kg)
#' @return Scaled invivo clearance in L/h
calculateScaledS9Clearance<- function(clearance,units,organism,
                                      age,liver_wt,km=1,mpcppgl=list()){
  
  # get MPPGL and CPPGL
  if (length(mpcppgl) == 0){
    # get MPPGL and CPPGL
    if (organism == "human"){
      MPCPPGL <- calcMPCPPGL(age)
      MPCPPGL <- unlist(MPCPPGL)
      names(MPCPPGL)<- NULL
    }else if(organism == "rat"){
      MPCPPGL <- unlist(mpcppgl)
    }
  }else{
    MPCPPGL <- unlist(mpcppgl)
  }
  #assuming S9PPGL is the sum of MPPGL and CPPGL
  S9PPGL <- sum(MPCPPGL)
  # bsaed on units for the clearance get the volume multiplier vector
  #i.e. multiply clerance by 10^-3 if expressed in mL or 10^-6 if expressed in uL
  volume_multplier <- ifelse(substr(units,1,2)=="ml",10^-3,10^-6)
  # based on units for the clearance get the time multiplier vector
  # i.e. multiply clearance by 60 if measured in uL/min/mg Protein
  time_multiplier <- ifelse(units=="ulmmP",60,1)
  # convert clerance from unit in UI to L/h
  # need to convert ml to Liters
  # mg protein to gram liver (MPPGL)
  # if rate is per minutes then convert to per hour (time_multiplier)
  # L/h/g liver to L/h by using liver weight in kg
  temp_scaled_cl <- clearance*volume_multplier*time_multiplier*S9PPGL*1000
  scaled_cl <- temp_scaled_cl*liver_wt
  total_hepcl <- sum(scaled_cl)
  return(total_hepcl)
}

#' Calculate sclaed in-vivo clearance if in-vitro clearance is measured in whole hepatocytes
#' @description The functionuses measured in-vitro clearance in whole hepatocytes, and phyiological parameters, 
#' to estimate in-vivo clearance. This function is not called directly by the user.
#' @param clearance number for measured whole hepatocyte clearance
#' @param units string for whole hepatocyte clearance units
#' @param organism name of organism, "human" or "rat"
#' @param age age of organism in "years" if "human" or "weeks" if "rat"
#' @param liver_wt liver weight(kg),
#' @param km michelis-menten constant for chemical
#' @return Scaled invivo clearance in L/h
calculateScaledWholeHepClearance <- function(clearance,units,liver_wt,hpgl,km = 1){
  if (units == "Lh"){ #Liters per hour
    scaled_hepcl <- clearance
  }else if(units == "LhH"){ # Liters per hour per 10^6 hepatocytes
    scaled_hepcl <- clearance*hpgl*liver_wt*1000
  }else{ # umol/min/10^6 hepatocytes 
    scaled_hepcl <- (clearance/km)*60*hpgl*liver_wt*1000
  }
  return(scaled_hepcl)
}

#' Calculate scaled in-vivo clearance if in-vitro clearance is meausured using S9 Fraction
#' @description In-vitro clearance is measured in S9 fraction. The function uses measured in-vitro clearance
#' and physiological parameters to estimate in-vivo clearance. This function is not called directly by the user
#' @param clearance number for measured S9 clearance
#' @param organism name of organism, "human" or "rat"
#' @param age age of organism in "years" if "human" or "weeks" if "rat"
#' @param liver_wt liver weight(kg)
#' @param cyp_data dataframe containing all the cyp values with column names (name,loc,abundance,isef,fumic)
#' @param return_total logical, type of value to be returned,
#' @return  Total scaled invivo clearance in L/h if return_total is TRUE, else a named list of individual clearances

calculateRecombClearance <- function(clearance,organism,age,
                                     liver_wt,cyp_data,return_total = T){
  # get a vector of enzyme names
  cyp_names <- cyp_data[["name"]]
  # get a list of enzyme location
  cyp_locs <- cyp_data[["loc"]]
  # get mg Protein to g Liver based on location and organism
  if (organism == "human"){
    MPCPPGL <- calcMPCPPGL(age)
  }

  cyp_mgPgL <- unlist(lapply(cyp_locs,function(x){MPCPPGL[[x]]}))
  names(cyp_mgPgL)<- NULL

  # get abundance values
  cyp_abundance <- as.numeric(cyp_data[["abundance"]])
  # get the fumic values
  cyp_fumic <- as.numeric(cyp_data[["fumic"]])
  # get the ISEF values
  cyp_isef <- as.numeric(cyp_data[["isef"]])
  # if ontogeny values are passed.use them else set them to 1
  ontogeny_table <- getAllCypData(age)
  cyp_ontogeny <- ontogeny_table$Ontogeny
  names(cyp_ontogeny)<- as.character(ontogeny_table$Enzymes)
  cyp_ontogeny <- cyp_ontogeny[cyp_names]
  # reorder the clearances to have the same order as the cyp data enzyme
  clearance <- clearance[cyp_names]

  clearance <- as.numeric(unlist(clearance[cyp_names]))


  scaled_clearance <- clearance*cyp_mgPgL*cyp_abundance*cyp_ontogeny*cyp_isef*liver_wt*1000*60*(10^-6)/cyp_fumic

  if (return_total){
    total_hepcl <- sum(scaled_clearance)
    return(total_hepcl)
  }else{
    names(scaled_clearance)<- cyp_names
    return(scaled_clearance)
  }


}
#'calculate steady state concentration of orally adminitered non volatile compounds
#' based on steady state clearance for renal,#'hepatic and plasma clearance
#'
calculateOralNonvolCss <- function(dose,bw,ql,qc,fup,cl_type,scaled_hepcl,
                                  scaled_rencl,scaled_plcl){

  if (cl_type == "cl_eq1"){
    clh<- ql*fup*scaled_hepcl/(ql+(fup*scaled_hepcl))
  }else if (cl_type == "cl_eq2"){
    clh<- ql*fup*scaled_hepcl/(ql+scaled_hepcl)
  }else if (cl_type == "cl_eq3"){
    clh<- ql*scaled_hepcl/(ql+scaled_hepcl)
  }
  clb<- qc*scaled_plcl/(qc+scaled_plcl)
  css<- (dose*bw/24)/(clh+clb+scaled_rencl)
  return(css)
}
#'calculate steady state concentration of orally administered volatile compounds
#' based on steady state clearance for renal,#'hepatic and plasma clearance
#'
calculateOralVolCss <- function(dose,bw,ql,qc,pair,scaled_hepcl,scaled_plcl){

  if (cl_type == "cl_eq1"){
    clh<- ql*fup*scaled_hepcl/(ql+(fup*scaled_hepcl))
  }else if (cl_type == "cl_eq2"){
    clh<- ql*fup*scaled_hepcl/(ql+scaled_hepcl)
  }else if (cl_type == "cl_eq3"){
    clh<- ql*scaled_hepcl/(ql+scaled_hepcl)
  }
  clb<- qc*scaled_plcl/(qc+scaled_plcl)
  css<- (dose*bw/24)/(clh+clb+rcl())
  return(css)
}
#'calculate steady state concentration of inhaled volatile compounds
#' based on steady state clearance for renal,#'hepatic and plasma clearance
#'
calculateInhVolCss <- function(dose,ql,qalv,pair,scaled_hepcl,scaled_plcl){

  clh <- (ql/qalv)*(scaled_hepcl/(scaled_hepcl+ql))
  css <- dose/((1/pair)+clh)
  return(css)
  # if (cl_type == "cl_eq1"){
  #   clh<- ql*fup*scaled_hepcl/((ql+fup)*scaled_hepcl)
  # }else if (cl_type == "cl_eq2"){
  #   clh<- ql*fup*scaled_hepcl/(ql+scaled_hepcl)
  # }else if (cl_type == "cl_eq3"){
  #   clh<- ql*scaled_hepcl/(ql+scaled_hepcl)
  # }
  # clb<- qc*scaled_plcl/(qc+scaled_plcl)
  # css<- (dose*bw/24)/(clm+clc+clh+cls9+clb+rcl())
}


#' Get the chemical data from the project Db
#' This function is not designed to be used by the end user
getChemDataFromDb <- function(chemid = NULL){
  query <- sprintf("Select cas from ChemicalSet where chemid = '%i';",as.integer(chemid))
  result <- projectDbSelect(query)
  cas <- result$cas
  query <- sprintf("Select value from Chemical where param = 'mw' AND chemid = '%i'",
                   as.integer(chemid))
  result <- projectDbSelect(query)
  return(list("casn" = cas, "mw"= as.numeric(result$value)))
}
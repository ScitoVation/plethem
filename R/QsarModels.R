#'Calculate Partition Coefficient
#'@description This function calculates the partition coefficients based on the qsar model selected for the given tissues.
#'Currently only one QSAR model is supported by PLETHEM
#'@param selected_qsar QSAR model to use for estimating partition coefficient
#'@param chem_params A named list of chemical params. The list should contain the minimal number of parameters needed to run the QSAR model selected
#'@param tissue_list List of tissues for which the partition coefficients need to be calculated. See vignette on Qsar based parameter estimation of more details
#'@export
calculatePartitionCoefficients<- function(selected_qsar = "default",chem_params = NULL,
                                          tissue_list=list(),selected_org = "human"){

  partCoefficients <- list()
  #decide which qsar model to run
  if(selected_qsar == "one"){
    partCoefficients <- qsarModelDefault(chem_params,selected_org,tissue_list)
  }else if(selected_qsar == "two"){
    #partCoefficients <- qsarModeltwo(chemical_params)
  }

  return(partCoefficients)
}

#' Calculate Partition Coefficient using the default QSAR model
#' @description Calculates the partition coefficient using the default QSAR model. This QSAR model is based on the one described by Jongneelan et al as a part of the IndusChemFate model
#' @param chem_params A named list of parameters needed to run the model
#' @param selected_org Either "human" or "rat"
#' @param tissue_list List of tissues for which partition coefficients have to be calculated
#' @return Named list of partition coefficients, one for each tissue in the tissue list
qsarModelDefault <- function(chem_params,selected_org,tissue_list=NULL){

  # chemical_params <- list("den"=input$ms_den, "mw"=input$ms_mw, "vpa"=input$ms_vpa, "dkow"=input$ms_dkow, "lkow"=input$ms_lkow, "wsol"=input$ms_wsol, "res"=input$ms_res,
  #                         "fhprt"=input$ms_fhprt, "vmaxc"=input$ms_vmaxc, "km"=input$ms_km)

  lkow <- chem_params[["lkow"]]
  VPa <- chem_params[["vpa"]]
  mw <- chem_params[["mw"]]
  wsol <- chem_params[["wsol"]]
  temp <- 25
  
  # log of henry's coeff
  lhen <- log(VPa*mw/wsol/8.314/(temp+273.15))/log(10)
  lkoair <- lkow-lhen
  
  frwsol <- 0.993 / (0.993 + 0.007 * 10^lkow)
  
  if (selected_org == "human"){
    if (lhen < -1){
      pbair <- 0.4445*(1/10^lhen)+0.005189*10^lkoair
    }else{
      if(VPa > 4000){
        pbair <- 0.8417 * (1 / 10 ^ lhen) + 0.006232  * 10 ^ lkoair
      }else{
        pbair <- 0.4445 * (1 / 10 ^ lhen) + 0.005189 * 10 ^ lkoair
      }
      
    }
    
    kow = 10^lkow
    if (kow< 0.1){
      kow <- 0.1
    }
    
    # calculate compartment partition coefficients
    pfat <- (0.8 * kow^1.03 + 0.2) / (0.0056 * kow^1.03 + 0.83) - 0.38
    if (pfat < 0.1){
      pfat <- 0.1
    }
    pskin  <- (0.031 * kow^0.81 + 0.792) / (0.0056 * kow^0.81 + 0.83) - 0.22
    
    pmusc <- (0.031 * kow^0.81 + 0.792) / (0.0056 * kow^0.81 + 0.83) - 0.22
    pmarr <- (0.133 * kow^0.48 + 0.775) / (0.0056 * kow^0.48 + 0.83) - 0.21
    pbne  <- (0.031 * kow^0.81 + 0.792) / (0.0056 * kow^0.81 + 0.83) - 0.22
    pbrn  <- (0.133 * kow^0.48 + 0.775) / (0.0056 * kow^0.48 + 0.83) - 0.21
    plng  <- (0.031 * kow^0.81 + 0.792) / (0.0056 * kow^0.81 + 0.83) - 0.22
    phrt  <- (0.031 * kow^0.81 + 0.792) / (0.0056 * kow^0.81 + 0.83) - 0.22
    pgs   <- (0.049 * kow^0.81 + 0.711) / (0.0056 * kow^0.81 + 0.83) - 0.35
    pliv  <- (0.049 * kow^0.81 + 0.711) / (0.0056 * kow^0.81 + 0.83) - 0.35
    pkdn  <- (0.053 * kow^0.57 + 0.785) / (0.0056 * kow^0.57 + 0.83) - 0.19
    
    
  }else if(selected_org == "rat"){
    # pbair calculation
    if (lhen< -0.7){
      pbair <- 0.19338*(1/10^lhen)+(0.002592*10^lkoair)
    }else{
      pbair <- 2.32245*(1/10^lhen)+(0.0067389*10^lkoair)
    }
    #Limit lkow to range where  qsar model works
    if (lkow >5){
      lkow <- 5
    }
    if (lkow < -1){
      lkow <- -1
    }
    pfat <- 10^(1.106*lkow - 0.1253*lkow^2 - 0.451)
    pfat <- max(pfat,0.05)
    
    pskin <- 10^(0.1207*lkow-0.0453)
    pmusc <- 10^(0.05211*lkow- 0.2239)
    pmarr <- 10 ^ (0.1207 *lkow - 0.0453)
    pbne  <- 10 ^ (0.05211 *lkow - 0.2239)
    pbrn  <- 10^(0.1207 * lkow- 0.0453)
    plng  <- 10^(0.05211 * lkow - 0.2239)
    phrt  <- 10^(0.05211 * lkow - 0.2239)
    pgs   <- 10 ^ (0.15094 * lkow - 0.1111)
    pliv  <- 10^(0.15094 * lkow - 0.1111)
    pkdn  <- 10^(0.3125 * lkow - 0.1784)
    pkdn <- max(pkdn,0.5)
  }

  frwsol = 0.993 / (0.993 + 0.007 * 10^lkow)
  rsrp = 2.0 * (lkow + 0.5)
  if (rsrp >1){
    rsrp <- 0.99
  } else if (rsrp <1){
    rsrp <- 0.01
  }

  partCoefficients <- NULL
  partCoefficients <- list("pfat"=pfat, "pskin"=pskin, "pmusc"=pmusc, "pbone"=pmarr, "pbone"=pbne,
                           "pbrn"=pbrn, "plng"=plng, "phrt"=phrt, "pgi"=pgs, "pliv"=pliv, "pkdn"=pkdn,
                           "prpf"= pliv,"pspf"= pmusc,"pair"= pbair,"frwsol"= frwsol)
  partCoefficients <- lapply(partCoefficients,function(x){signif(x,4)})

  return(partCoefficients)
}



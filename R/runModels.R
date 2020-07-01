

# This script runs all the models


#' Run the PBPK models in forward dosimetry mode
#' @description Run the pbpk model in forward dosimetry mode.
#' This function is common across all PBPK models.
#' This function can be used from the console if all the inputs are provided
#' @param initial_values A list containing initial values needed to run the model
#' @param model The name of the PBPK model to simulate
#' @useDynLib plethem ,.registration = TRUE
#' @export
runFDPBPK<- function(initial_values,model ="rapidPBPK"){

  # if (model== "rapidPBPK"){
  #   print("Running Rapid PBPK")
  # }
  if (model == "rapidPBPK"){
    #get all input
    
    #initial_values <- calculateInitialValues(params,total_vol,prefc)
    initial_params <- initial_values[['initial_params']]
    
    event_data <- initial_values[['evnt_data']]
    times <- initial_values[['times']]
    tstop <- initial_values[['tstop']]
    state <- initial_values[['state']]
    times <- sort(c(deSolve::cleanEventTimes(times,event_data[["time"]]),
                    event_data[["time"]]))
    event_times <- unique(event_data[["time"]])
    state <- rapidPBPK_initStates(initial_params,state)
    initial_params <- rapidPBPK_initParms(initial_params)
   # print(initial_params[["cinh"]])
    modelOutput<- deSolve::ode(y = state, times = times,method = "lsodes",
                               func = "derivs", dllname = "plethem",initfunc= "initmod",parms = initial_params,
                               events=list(func="event", time=event_times),nout = length(rapidPBPK_Outputs),
                               outnames = rapidPBPK_Outputs)
    
    dfModelOutput <- as.data.frame(modelOutput,stringsAsFactors = F)
    result <- list("pbpk"=dfModelOutput)
  }else if(model == "httk_pbtk"){
    params4model <- setNames(as.numeric(initial_values$vals[initial_values$names]),
                             initial_values$names)
   # params4model <- setNames(as.numeric(params4model),initial_values$names)
    params4model <- as.list(params4model)
    print(params4model[["Clmetabolismc"]])
    days <- as.numeric(initial_values$vals["sim_dur"])
    tsteps <- 100
    bdose <- as.numeric(initial_values$vals["bdose"])
    ivdose <- as.numeric(initial_values$vals["ivdose"])
    if (bdose>0){
      iv.dose <- NULL
      if (initial_values$vals[["brep_flag"]] == "FALSE"){
        dose <- bdose
        doses.per.day <- as.numeric(initial_values$vals["breps"])
        daily.dose <- NULL
        httk_result <- solve_pbtk(parameters = params4model,
                                  days = days,
                                  tsteps = tsteps,
                                  dose = dose,
                                  doses.per.day = doses.per.day)
      }else{
        daily.dose <- bdose
        doses.per.day <- as.numeric(initial_values$vals["breps"])
        dose <- NULL
        httk_result <- solve_pbtk(parameters = params4model,
                                  days = days,
                                  tsteps = tsteps,
                                  daily.dose = daily.dose,
                                  doses.per.day = doses.per.day)
      }
      
    }else{
      iv.dose <- as.numeric(ivdose)
      httk_result <- solve_pbtk(parameters = params4model,
                                days = days,
                                tsteps = tsteps,
                                iv.dose = iv.dose)
    }

    result <- list("pbpk"=httk_result)
  }else if(model == "fishPBPK"){
    #get all input
    
    #initial_values <- calculateInitialValues(params,total_vol,prefc)
    initial_params <- initial_values[['initial_params']]
    
    event_data <- initial_values[['evnt_data']]
    times <- initial_values[['times']]
    tstop <- initial_values[['tstop']]
    state <- initial_values[['state']]
    times <- sort(c(deSolve::cleanEventTimes(times,event_data[["time"]]),
                    event_data[["time"]]))
    event_times <- unique(event_data[["time"]])
    state <- fishPBPK_initStates(initial_params,state)
    initial_params <- fishPBPK_initParms(initial_params)
    modelOutput<- deSolve::ode(y = state, times = times,method = "lsodes",
                               func = "derivsfishPBPK", dllname = "plethem",initfunc= "initmodfishPBPK",parms = initial_params,
                               events=list(func="eventfishPBPK", time=event_times),nout = length(fishPBPK_Outputs),
                               outnames = fishPBPK_Outputs)
    dfModelOutput <- as.data.frame(modelOutput,stringsAsFactors = F)
    result <- list("pbpk"=dfModelOutput)
  }

  

  return(result)
}

#' Run HT-IVIVE
#' @description Launches the HT-IVIVE UI.
#' @examples
#' \dontrun{
#' runHTIVIVE()
#' }
#' @export
runHTIVIVE <- function(){
  clearProjectDb()
  interactiveHT("HT-IVIVE")
  
}

fishPBPK_initParms <- function(newParms = NULL) {
  parms <- c(
    bw = 0,
    qc = 0,
    qg = 0,
    vfatc = 0,
    qfatc = 0,
    pfat = 0,
    vlivc = 0,
    qlivc = 0,
    pliv = 0,
    vkdnc = 0,
    qkdnc = 0,
    pkdn = 0,
    vrpfc = 0,
    qrpfc = 0,
    prpf = 0,
    vspfc = 0,
    qspfc = 0,
    pspf = 0,
    frspfkdn = 0.4,
    vfat = 0,
    vkdn = 0,
    vliv = 0,
    vrpf = 0,
    vspf = 0,
    qfat = 0,
    qkdn = 0,
    qliv = 0,
    qrpf = 0,
    qspf = 0,
    vmax = 0,
    km = 1e-10,
    cins = 0,
    pbldw = 1e10,
    gul = 1
  )
  
  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
    parms[names(newParms)] <- newParms
  }
  
  parmsfishPBPK <- within(as.list(parms), {
  })
  out <- .C("getParmsfishPBPK",  as.double(parms),
            out=double(length(parms)),
            as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

fishPBPK_Outputs <- c(
  "cv",
  "ca",
  "mbal"
)

fishPBPK_initStates <- function(parms, newStates = NULL) {
  Y <- c(
    cfat = 0.0,
    cliv = 0.0,
    cspf = 0.0,
    crpf = 0.0,
    ckdn = 0.0,
    cmet = 0.0,
    ains = 0.0,
    insswch = 0.0
  )
  
  if (!is.null(newStates)) {
    if (!all(names(newStates) %in% c(names(Y)))) {
      stop("illegal state variable name in newStates")
    }
    Y[names(newStates)] <- newStates
  }
  .C("initStatefishPBPK", as.double(Y));
  Y
}
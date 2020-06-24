

# This script runs all the models

#' Simulate the rapidPBPK model
#' @description Simulates the rapidPBPK model within PLETHEM and returns the timecourse for all state variables.
#' This function can be called from the console if all inputs are provided
#' @param parameters Parameter values for static parameters in the model.
#' @param init_states Initial values for all state variables in the model.
#' @param event_times Times for all events in the model.
#' @param times Times at which values need to be output.
#' @param outputs A list of of variables which are output by model.
#' @return modelOutput DataFrame containing timecourse for all the state variables and outputs from the model
#' @useDynLib plethem ,.registration = TRUE
#' @import deSolve
#' @export
simulateRapidPBPKModel <- function(parameters,initial_states){
  times <- sort(c(deSolve::cleanEventTimes(times,event_data[["time"]]),
                  event_data[["time"]]))
  modelOutput<- deSolve::ode(y = state, times = times,method = "lsodes",
                             func = "derivs", dllname = "plethem",initfunc= "initmod",parms = initial_params,
                             events=list(func="event", time=event_times),nout = length(rapidPBPK_Outputs),
                             outnames = rapidPBPK_Outputs)
}

#' Run PBPK workflows
#' @description Run all PBPK worfklows from the plethem package.
#' This function is called by the User interface to run the selected simulation.
#' See\code{\link{runFDPBPK}} for running forward dosimetry models from the console.
#' @param model The name of the PBPK model to simulate. Defaults to 'rapidPBPK'.
#' @param simid Simulation ID for the simulation to run from the project database in PLETHEM.
#' This function should not be called by the user from the console
#' Defaults to 'fd'.
#' @useDynLib plethem ,.registration = TRUE
#' @export
executeInteractivePBPKWorkflows <- function(model = 'rapidPBPK',simid=NULL,progressFunc=NULL){
  NULL
}

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
    # state <- c(
    #   #exposure related
    #   inhswch=0,ainh=0,aexh=0,
    #   totodose=0,odose=0,totddose=0,ddose=0,aabsgut=0,
    #   ivswch=0,aiv=0,
    #   #compartments
    #   abld=0,
    #   abfat=0,atfat=0,
    #   abskin=0,atskin=0,
    #   abmusc=0,atmusc=0,
    #   abbone=0,atbone=0,
    #   abbrn=0,atbrn=0,
    #   ablng=0,atlng=0,
    #   abhrt=0,athrt=0,
    #   abgi=0,atgi=0,
    #   abliv=0,atliv=0,
    #   abkdn=0,atkdn=0,
    #   abrpf=0,atrpf=0,
    #   abspf=0,atspf=0,
    #   # Clearance
    #   ametliv1=0,ametliv2=0,aclbld=0,auexc=0,anabsgut=0)
    
    # eventfun<- function(t,y,params){
    #   with (as.list(c(y,params)),{
    #     DOSE<- 2.68 #DOSE+(PDOSE*BODYWT)?
    #     TOTDOSE<- 8.9 # TOTDOSE+(PDOSE*BODYWT)?
    #     return(c(AMUSCLE,  AEXH,ABRAIN,AEXC,ALIVER,ABONE,ALUNG,ASKIN,AMARROW,AINH,AVEN,URINVOL,AFAT,AKIDNEY,ASTOM,AMET,TOTDOSE,DOSE,AART,AHEART,
    #              AUCMUSCLE,AUCBRAIN,AUCLIVER,AUCBONE,AUCLUNG,AUCSKIN,AUCMARROW,AUCVEN,AUCFAT,AUCKIDNEY,AUCSTOM,AUCART,AUCHEART))
    #   })
    # }
    
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
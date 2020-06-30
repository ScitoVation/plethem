

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


# This script runs all the models

#' Run the PBPK models in forward dosimetry mode
#' @description Run the pbpk model in forward dosimetry mode.
#' This function is common accross all PBPK models.
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
  }

  

  return(result)
}

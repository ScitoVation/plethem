# This file contains utility functions for PBPK models
# none of these functions should be called by the user

#' Gets the metabolism data. Should not be used by directly by the user
#' @description The function returns the relavent metabolism data if the simulation contains
#' data from the metabolism set
#' @param admeid The id for ADME set. The admeid is used to obtain information about the other sets.
#' @param model Model name
#' @return List containing the metabolism values needed to run PBPK model or
#' display simulation information
#' @export
getMetabData <- function(admeid,model="rapidPBPK"){
  admeid <- as.integer(admeid)
  # Get metabolism data from the table
  query <- sprintf("SELECT param,value FROM Adme WHERE admeid = %i AND (param = 'vmaxc' OR param = 'vkm1c');",admeid)
  result <- projectDbSelect(query)
  result <- setNames(result$value,result$param)
  # unserialize the table to get it back
  if (result[["vmaxc"]] == 0){
    metab_units <- "L/h/kg Liver"
    metab_type <- "First Order Metabolism"
    variable <- "vkm1c"
    value <- result[["vkm1c"]]
  }else{
    metab_units <- "\u00B5M/h/kg BW^0.75"
    metab_type <- "Michaelis Menten Kinetics"
    variable <- "vmaxc"
    value <- result[["vmaxc"]]
  }
 
  
  return(list("Type"= metab_type,
              "Units"=metab_units,
              "Value"= value,
              "Var"=variable))
}

#' Gets all the parameter values for the model. This function should not be used by the model
#' @description Get all the parameter values that are required for the model to run. The values
#' are obtained from the Project database. Only those values that are used in the model as
#' determined by the master database are returned by the function
#' @param simid Integer The id for simulation selected to run
#' @param model Character The string identifying the model to be run
#' @return list List that can be passed to the solver as model params
#' @export
getAllParamValuesForModel <- function(simid,model){
  simid <- as.integer(simid)
  # get the param names that are a part of the model
  # get physiological parameter names
  query <- sprintf("Select Var from ParamNames Where ModelParams = 'TRUE' AND ParamSet = 'Physiological' AND Model = '%s';",
                   model)
  result <- mainDbSelect(query)
  param_names <- result$Var
  # get Chemical parameter names
  query <- sprintf("Select Var from ParamNames Where ModelParams = 'TRUE' AND ParamSet = 'Chemical' AND Model ='%s' ;",
                   model)
  result <- mainDbSelect(query)
  param_names <- c(param_names,result$Var)
  # get Exposure parameter names
  query <- sprintf("Select Var from ParamNames Where ModelParams = 'TRUE' AND ParamSet = 'Exposure' AND Model = '%s';",
                   model)
  result <- mainDbSelect(query)
  param_names <- c(param_names,result$Var)
  #get adme parameter names
  query <- sprintf("Select Var from ParamNames Where ModelParams = 'TRUE' AND ParamSet = 'Adme' AND Model = '%s';",
                   model)
  result <- mainDbSelect(query)
  param_names <- c(param_names,result$Var)



  # get the physiological values for the simulation
  query <- sprintf("Select * FROM SimulationsSet Where simid = %i;",
                   simid)
  result <- projectDbSelect(query)
  admeid <- as.integer(result[["admeid"]])
  chemid <- as.integer(result[["chemid"]])
  expoid <- as.integer(result[["expoid"]])
  physioid <- as.integer(result[["physioid"]])
  tstart <- result[["tstart"]]
  time_multiplier <- switch(result$dur_units,
                            "h"=1,
                            "d"=24,
                            "w"=168)
  sim_dur <-result[["sim_dur"]]*time_multiplier
  

  # get all the physiology parameters
  query <- sprintf("SELECT param,value FROM Physiological WHERE physioid = %i;",
                   physioid)
  result <- projectDbSelect(query)
  physio_params <- result$value
  names(physio_params)<- result$param

  # get all the exposure parameters
  query <- sprintf("SELECT param,value FROM Exposure WHERE expoid = %i;",
                   expoid)
  result <- projectDbSelect(query)
  expo_params <- result$value
  names(expo_params)<- result$param

  # get all the Chemical parameters
  query <- sprintf("SELECT param,value FROM Chemical WHERE chemid = %i;",
                   chemid)
  result <- projectDbSelect(query)
  chem_params <- result$value
  names(chem_params)<- result$param
  
  # get all the ADME parameters
  query <- sprintf("SELECT param,value FROM Adme WHERE admeid = %i;",
                   admeid)
  result <- projectDbSelect(query)
  adme_params <- result$value
  names(adme_params)<- result$param

  params <- c(physio_params,expo_params,chem_params,adme_params)

  # add time start and sim duration as tstart and totdays to work with the model
  params[["tstart"]]<- tstart
  params[["totdays"]]<- as.integer(sim_dur/24)
  params[["sim_dur"]]<- sim_dur
  # get the metabolism data and adjust params accordingly
  metab_data <- getMetabData(admeid,model)
  metab_var <- metab_data$Var
  if (model == "rapidPBPK"){
    
    if(metab_var == "vmaxc"){
      params[["vmaxc"]]<- metab_data$Value
      params[["vkm1c"]]<- 0
    }else{
      params[["vkm1c"]]<- metab_data$Value
      params[["vmaxc"]]<- 0
    }
  }else if(model == "httk_pbtk"){
   
    params[["Clmetabolismc"]]<- metab_data$Value
  }else if(model == "fishPBPK"){
   
    params[["vmax"]]<- metab_data$Value
  }


  return(list("vals" = params,"names" =param_names))

}

#' Gets all the variability values for the model. This data returned by the function is not meant to be understandable by the user
#' @description Get all the variability values required for creating paramter sets for 
#' montecarlo analysis. The values are obtained from the Project database. 
#' @param simid Integer The id for simulation selected to run
#' @param params list of model parameters
#' @param mc_num number of montecarlo runs
#' @return matrix of parameters that will be used for individual montecarlo runs
#' 
#' @export
getAllVariabilityValuesForModel<- function(simid, params,mc_num){
  # set the seed
  set.seed(as.numeric(Sys.time()))
  
  params <- lapply(params,function(x){as.numeric(x)})
  # get the ids for variability sets
  query <- sprintf("Select expovarid,physiovarid,chemvarid,admevarid FROM SimulationsSet Where simid = %i;",
                   simid)
  result <- projectDbSelect(query)
  chemvarid <- as.integer(result[["chemvarid"]])
  expovarid <- as.integer(result[["expovarid"]])
  physiovarid <- as.integer(result[["physiovarid"]])
  admevarid <- as.integer(result[["admevarid"]])
  varid_list <- c(chemvarid,expovarid,physiovarid,admevarid)
  type_name2var <- list("Normal" = "norm","Log-normal"="lnorm","Uniform"="uform")
  mc_param_names <- list()
  cvs <- list()
  types <- list()
  bound_flag <- list()
  lbound <- list()
  ubound <- list()
  for(varid in varid_list){
    if(varid != 0){
      # get the data associated with varid
      query <- sprintf("Select var_tble from Variability where varid = %d",varid)
      ret_data <- projectDbSelect(query)
      var_tble <- unserialize(charToRaw(ret_data[["var_tble"]]))
      mc_param_names <- c(mc_param_names,var_tble$Parameter)
      cvs <- c(cvs,stats::setNames(as.numeric(var_tble$CV),var_tble$Parameter))
      types <- c(types,setNames(type_name2var[unlist(var_tble$Type)],var_tble$Parameter))
      bound_flag <- c(bound_flag,setNames(as.logical(unlist(var_tble$BFlag)),var_tble$Parameter))
      lbound <- c(lbound,setNames(as.numeric(var_tble$LowerBound),var_tble$Parameter))
      ubound <- c(ubound,setNames(as.numeric(var_tble$UpperBound),var_tble$Parameter))
    }
  }
  # set up mc matrix
  MC.matrix <- matrix(NA, nrow = mc_num, ncol = (length(cvs) ))
  colnames(MC.matrix) <- mc_param_names
  for (this.param in mc_param_names){
    if (!(this.param %in% names(params)))
      stop(paste("Cannot find cv.params parameter", this.param,
                 "in parameter list."))
    if (params[[this.param]] > 0){
      mean <- params[[this.param]]
      cv <- cvs[[this.param]]
      sd <- mean*cv
      dist_type <- types[[this.param]]
      flag <- bound_flag[[this.param]]
      if (dist_type=="norm"){
        if (flag){
          upperlim = ubound[[this.param]]
          lowerlim = lbound[[this.param]]
        }else{
          upperlim = mean+2*sd
          lowerlim = mean-2*sd
        }
        MC.matrix[,this.param]<- truncdist::rtrunc(mc_num,"norm",a = lowerlim, 
                                        b = upperlim, mean = mean, 
                                        sd = sd)
      }else if(dist_type == "lnorm"){
        sdlog = sqrt(log(1+sd^2/mean^2))
        meanlog = log(mean)-(0.5*sdlog^2)
       
        if (flag){
          upperlim = ubound[[this.param]]
          lowerlim = lbound[[this.param]]
        }else{
          upperlim = exp(meanlog+2*sdlog)
          lowerlim = exp(meanlog-2*sdlog)
        }
        MC.matrix[,this.param]<- truncdist::rtrunc(mc_num,"lnorm",a = lowerlim, 
                                        b = upperlim, mean = meanlog, 
                                        sd = sdlog)
      }else if(dist_type == "uform"){
        upperlim = mean+2*sd
        lowerlim = mean-2*sd
        MC.matrix[,this.param]<- stats::runif(mc_num,lowerlim,upperlim)
      }
      else{
        #default to uniform distribution
        upperlim = mean+2*sd
        lowerlim = mean-2*sd
        MC.matrix[,this.param]<- stats::runif(mc_num,
                                              ifelse(lowerlim>=0,lowerlim,0),
                                              upperlim)
      }
      
    }else{
      MC.matrix[, this.param] <- 0
    }
  }
  return(MC.matrix)
}

#' Get the values for parameters in a given set
#' @description Get all the parameter values for a given dataset and id
#' @param set_type Either "physio","chem"or "expo"
#' @param id integer id for the required set
#'@export
getParameterSet<- function(set_type = "physio",id = 1){
  set_name <- switch(set_type,
                     "physio" = "Physiological",
                     "chem" = "Chemical",
                     "expo" = "Exposure",
                     "adme"="Adme")
  id_name <- paste0(set_type,"id")
  set_table_name <- paste0(set_name,"Set")
  query <- sprintf("SELECT param, value FROM %s where %s = %s ;",
                   set_name,id_name,id)
  param_values <- projectDbSelect(query)
  param_names <- param_values$param
  param_values <- param_values$value
  names(param_values)<- param_names
  return(param_values)
}

#'get all set names for a given paramter set
#' @description This function returns all the sets of a given set type from the current project database
#' This is used internally to update drop downs or to get simulation choices
#' @param set_type The type of set can be "physio", "chem","expo","metab" or "sim"
#' @return named list of set names
#' @export
getAllSetChoices <- function(set_type = "physio"){
  set_type <- isolate(set_type)
  if (set_type!= "none"){
    set_name <- switch(set_type,
                       "physio" = "Physiological",
                       "chem" = "Chemical",
                       "expo" = "Exposure",
                       "metab"="Metabolism",
                       "adme"="Adme",
                       "biom"="Biomonitering",
                       "sim" = "Simulations")
    id_name <- paste0(set_type,"id")
    set_table_name <- paste0(set_name,"Set")
    query <- sprintf("SELECT %s, name FROM %s ;",id_name,set_table_name)
    set_choices <- projectDbSelect(query)
    set_list <- as.list(set_choices[[id_name]])
    names(set_list)<- set_choices$name
    return(set_list)
  } else {
    return(NULL)
  }

}

#' Get all observation sets
#' @description Get all the sets associated with observation in a given projects. Observations need to
#' be handled differently from the other set types since they can themselves be of multiple types
#' @param obs_type type of observation to return, can be "cl" or "conc" for clearance and concentration data
#' @return named list of all sets of the obs_type
#' @export
getObservationSetChoices <- function(obs_type){
  if (obs_type == "none"){
    return(NULL)
  }else{
    set_name <- "Observation"
    set_table_name <- "ObservationSet"
    id_name <- "obsid"
    query <- sprintf("SELECT %s, name FROM %s ;",id_name,set_table_name)
    set_choices <- projectDbSelect(query)
    set_list <- as.list(set_choices[[id_name]])
    names(set_list)<- set_choices$name
    return(set_list)
  }
}

#' Get all variability sets
#' @description Get all the variability datasets in a given projects. Variabilities need to
#' be handled differently from the other set types since they can themselves be of multiple types
#' @param var_type type of Variability set to return, can be "physio","chem" or "expo" or "conc"
#' @return named list of all sets of the var_type
#' @export
getVariabilitySetChoices <- function(var_type="physio"){
  query <- sprintf("Select varid,name From Variability where type = '%s'",var_type)
  set_choices <- projectDbSelect(query)
  set_list <- as.list(set_choices[["varid"]])
  names(set_list)<- set_choices$name
  return(set_list)
}

#' reshape plotted data to create wide form
#' @description Reshapes plot data in long form to wide form. The plot data has time as the id
#' @param plotData Plot Data in long form
#' @param type Workflow type - either fd (Forward Dosimetry) or mc (Monte Carlo Analysis)
#' @export
reshapePlotData<- function(plotData,type = "fd"){
  
  if (type == "fd"){
    data <- unique.data.frame(plotData)
    return(reshape2::dcast(data,time~variable))
  }else{
    data <- plotData
    return(reshape2::dcast(data,sample~variable))
  }
                          
  
}

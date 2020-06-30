#' Functions for performing NCA and returning the data
#' 

#' Calculate AUC, Cmax and Time to Cmax from the results of PBPK model run
#' @description Calculates  the The user will never need to call this function
#' @param result Result from the forward dosimetry or individual montecarlo run
#' @param var_names Variable names from the model for which NCA should be performed
#' @param mode Forward Dosimetry or Montecarlo
#' @importFrom NonCompart sNCA IntAUC AUC
#' @export
#' 
performPlethemNCA <- function(result,var_names,mode = "FD"){
  
  conc_data <- result[var_names]
  nca_conc_list<- lapply(var_names,function(x,conc_data,time){
    conc <- conc_data[[x]]
    NCA_res <-sNCA(time,conc)
    aucall <- NCA_res[["AUCALL"]]
    aucinf <- NCA_res[["AUCIFP"]]
    cmax <- NCA_res[["CMAX"]]
    hlfe <- NCA_res[["LAMZHL"]]
    termslope <- NCA_res[["LAMZ"]]
    tmax <- NCA_res[["TMAX"]]
    last_time <- time[length(time)]
    start_time <- ifelse(last_time < 24,0,last_time-24)
    auc24 <- IntAUC(time,conc,start_time,last_time,NCA_res)
    return(c("AUCall"=aucall,"AUCinf"=aucinf,"AUC24"=auc24,
             "Cmax"=cmax,"T_cmax"=tmax,
             "half_life"=hlfe,"slope"=termslope))
  },conc_data, result$time)
  
  ncadf <- as.data.frame(nca_conc_list)
  
  colnames(ncadf)<- var_names
 
  return(ncadf)
} 
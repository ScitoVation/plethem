#' Functions for performing NCA and returning the data
#' 

#' Calculate AUC, Cmax and Time to Cmax from the results of PBPK model run
#' @description Calculates  the The user will never need to call this function
#' @param result Result from the forward dosimetry or individual montecarlo run
#' @param mode Forward Dosimetry or Montecarlo
#' @export
#' 
performPlethemNCA <- function(result, mode = "FD"){
  query <- sprintf("Select model_var,plot_var,name from ResultNames where param_set = 'conc' AND model='rapidPBPK' AND mode = '%s';",mode)
  legend_df <- mainDbSelect(query)
  legend_names <- setNames(legend_df$name,legend_df$model_var)
  var_names <- setNames(legend_df$model_var,legend_df$plot_var)
  conc_data <- result[legend_df$model_var]
  conc_auc <- lapply(legend_df$model_var,function(x,conc_data,time){
    concs <- conc_data[[x]]
    aucsdf <- as.data.frame(AUC(time,concs,"log"),stringsAsFactors = F)
    return(dplyr::last(aucsdf$AUC))
  },conc_data, result$time)

  conc_cmax <- lapply(legend_df$model_var,function(x,conc_data){
    cmax <-max(conc_data[[x]]) 
    ifelse(cmax>1,max(cmax),cmax)
    return(cmax)
  },conc_data
  )
  conc_tmax <- lapply(legend_df$model_var,function(x,conc_data,time){
    max_idx <- which(conc_data[[x]] == max(conc_data[[x]]))
    max_idx <- ifelse(length(max_idx)>1,max(max_idx),max_idx)
    return(time[max_idx])
  },conc_data,result$time
  )
  ncadfs <- data.frame("names"=legend_df$model_var,"auc"=conc_auc,"cmax" =conc_cmax,"tmax" =conc_tmax)
  print(ncadfs)
  query <- sprintf("Select model_var,plot_var,name from ResultNames where param_set = 'amt' AND model='rapidPBPK' AND mode = '%s';",mode)
  legend_df <- mainDbSelect(query)
  amt_data <- result[legend_df$model_var]
  amt_auc <- lapply(legend_df$model_var,function(x,amt_data,time){
    amts <- amt_data[[x]]
    aucsdf <- as.data.frame(AUC(time,amts,"log"),stringsAsFactors = F)
    return(dplyr::last(aucsdf$AUC))
  },amt_data, result$time)
  amt_auc <- setNames(amt_auc,legend_df$model_var)
  
 
  return(ncadfs)
} 
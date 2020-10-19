#' Read and parse the chemical batch file
#' @description Function to read the chemical batch file. The batch file description can be found in the user guide.
#' @param file_path path to the batch chemical csv file
#'@export
readBatchChemicalFile<- function(file_path){
  ret_data <- read.csv(file_path,header = T,stringsAsFactors = FALSE)
  return(as.data.frame(ret_data))
}

#' Read and parse the predictions from OPERA
#' @description Function to read the OPERA predictions. The function removes predictions made by OPERA that are not used by PLETHEM.
#' @param file_path path to OPERA predictions
#' @importFrom dplyr select mutate_at
#'@export
readOperaPredictions<- function(file_path){
  preds <- read.csv(file_path,stringsAsFactors = FALSE)
  #preds <- as.data.frame(preds)
  columnTranslate <- list("MoleculeID"="ID","MolWeight"="Molecular Weight",
                          "LogP_pred"="LogKow","LogWS_pred"="Water Solubility",
                          "LogVP_pred"="Vapor Pressure","FUB_pred" = "Fraction Unbound",
                          "LogKM_pred"="Km")
  preds <- select(preds,names(columnTranslate))
  colnames(preds)[colnames(preds)==names(columnTranslate)]<- columnTranslate
  logColumns <- c("Km","Vapor Pressure","Water Solubility")
  preds <- mutate_at(preds,logColumns,exp)
  print(class(preds))
  return(preds)
}
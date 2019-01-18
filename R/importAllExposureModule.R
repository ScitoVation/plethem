#' UI for importing SEEM data.
#' @description This function is called by the pbpk model to import SEEM exposure estimates. Never called by the user
#' @param namespace namespace for the module UI.
#' 
#' @export
importAllExposureDataUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(..., title = NULL, footer = modalButton("Dismiss"),
                        size = c("m", "s", "l"), easyClose = FALSE, fade = TRUE))
}



#' Server function for seem data module
#' @description Server function for import seem data module. This function should not be called by the user
#' @param input input object for the UI
#' @param output input object to the UI
#' @param session session object for the module
#' @param fpath path to the SEEM database
#' @param expo_name_df dataframe containing variable names for exposure values
#' @export
importAllExposureData <- function(input,output,session,fpath,expo_name_df){#,expo_name_df){
  ns <- session$ns
}
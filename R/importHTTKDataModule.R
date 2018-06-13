#' UI for importing data from HTTK
#' @export
importHTTKDataUI <- function(namespace){
  shinyjs::useShinyjs()
  ns <- NS(namespace)
  httk_chem <- chem.physical_and_invitro.data$CAS
  names(httk_chem)<- chem.physical_and_invitro.data$Compound
  showModal(modalDialog(title = "Select Chemicals",size = "l",
    multiInput(ns("chem_names"),"Chemicals to Import",choices = httk_chem)
  ))
  return(NULL)
}

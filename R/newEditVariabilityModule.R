# This module handles the UI and server for creating variability parameter sets

#' @export
#' 
newEditVariabilityUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(
    title = "Variablility",
    tagList(pickerInput(ns("param_names"),
                        label = "Select Parameters to assign variability",
                        choices = NULL,multiple = T,
                        options = list('selected-text-format' = "count > 3")
                        ),
            uiOutput(ns("ui_var"))
    )
  ))
}
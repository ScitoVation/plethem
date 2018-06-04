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
                        options = list('selected-text-format' = "count > 3",
                                       'live-search'=TRUE,
                                       'size'=10)
                        ),
            textOutput(ns("debug_text")),
            uiOutput(ns("ui_var"))
    ),
    footer = tagList(
      actionBttn(ns("ok"),"Done"),
      modalButton("Cancel")
    )
  ))
}

newEditVariability <- function(input,output,session,set_type,ops_type,var_params_list){
  
  ns <- session$ns
  updatePickerInput(session,"param_names",choices = var_params_list)
  output$debug_text <- renderText({input$param_names})
  # observeEvent(input$param_names,{
  #   
  # })
  
  
}
# This module handles the UI and server for creating variability parameter sets

#' @export
#' 
newEditVariabilityUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(
    title = "Variablility",
    tagList(
      fluidRow(
        column(8,
               pickerInput(ns("param_names"),
                           label = "Select Parameters to assign variability",
                           choices = NULL,multiple = T,
                           options = list('selected-text-format' = "count > 3",
                                          'live-search'=TRUE,
                                          'size'=10)
                        )
               ),
        column(4,
               actionButton(ns("update"),"Update list",color = "default")
               )
      ),
      textOutput(ns("debug_text")),
      tags$div(id = ns("cv_ui"))
    ),
    footer = tagList(
      actionButton(ns("ok"),"Done"),
      modalButton("Cancel")
    )
  ))
}

newEditVariability <- function(input,output,session,set_type,ops_type,var_params_list){
  
  ns <- session$ns
  div_id <-paste0("#",ns(""),"cv_ui") 
  current_list <- list()
  updatePickerInput(session,"param_names",choices = var_params_list)
  output$debug_text <- renderText({input$param_names})
  observeEvent(input$update,{
    add_list <- setdiff(input$param_names,current_list())
    print(add_list)
    remove_list <- setdiff(current_list(),input$param_names)
    print(remove_list)
    # if new elements are selected, add UI
    if(length(add_list) != 0){
      for(x in add_list){
        insertUI(selector = div_id,
                 ui = tagList(fluidRow(
                   column(4,
                          tags$h5(x)),
                   column(3,
                          numericInput(ns(paste0("cv_",x)),label = "CV",value = 0))
                   ))
                 )
      }
    }else if(length(remove_list)!= 0){
      
    }
    current_list <<- reactiveVal(input$param_list)

  })
  
  
}
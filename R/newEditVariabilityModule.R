# This module handles the UI and server for creating variability parameter sets

#' @export
#' 
newEditVariabilityUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(
    title = "Variablility",
    size = "l",
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
      tags$div(id = ns("cv_ui"),
               fluidRow(
                 column(4,
                        tags$h3("Parameter Name")
                        ),
                 column(3,
                        tags$h3("Coefficient of Variation")
                        ),
                 column(3,
                        tags$h3("Type of Distribution")
                        )
               ))
    ),
    footer = tagList(
      actionButton(ns("ok"),"Done"),
      modalButton("Cancel")
    )
  ))
}

newEditVariability <- function(input,output,session,set_type,ops_type,var_params_list){
  
  ns <- session$ns
  param_names <- names(var_params_list)
  div_id <-paste0("#",ns(""),"cv_ui") 
  data4module  <- list()
  data4module$current_list <- list()
  updatePickerInput(session,"param_names",choices = var_params_list)
  output$debug_text <- renderText({input$param_names})
  observeEvent(input$update,{
    current_list <- data4module$current_list
    print(current_list)
    add_list <- setdiff(input$param_names,current_list)
    print(add_list)
    remove_list <- setdiff(current_list,input$param_names)
    print(remove_list)
    # if new elements are selected, add UI
    if(length(add_list) != 0){
      for(x in add_list){
        insertUI(selector = div_id,
                 ui = tagList(
                   tags$div(id = ns(paste0("div_",x)),
                            fluidRow(
                              column(4,
                                     param_names[which(var_params_list == x)]
                                     ),
                              column(3,
                                     numericInput(ns(paste0("cv_",x)),label = NULL,value = 0)
                                     ),
                              column(3,
                                     selectInput(ns(paste0("type_",x)),label = NULL,
                                                 choices = c("Normal"="norm",
                                                             "Log-normal"="lnorm"))
                                     )
                   )
                   )
                 )
                 )
      }
    }
    if(length(remove_list)!= 0){
      for(x in remove_list){
        removeUI(session = session,
                 selector = paste0("#",ns(paste0("div_",x))),
                 immediate = T)
      }
      
    }
    data4module$current_list <<- input$param_names

  })
  
  
}
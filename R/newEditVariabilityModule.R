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
      tags$div(id = ns("added_ui"),
               fluidRow(
                 column(4,
                        tags$h4("Parameter Name")
                        ),
                 column(3,
                        tags$h4("Coefficient of Variation")
                        ),
                 column(3,
                        tags$h4("Type of Distribution")
                        )
               ))
    ),
    footer = tagList(
      actionButton(ns("ok"),"Done"),
      modalButton("Cancel")
    )
  ))
}

newEditVariability <- function(input,output,session,set_type,ops_type,var_params_list,set_id = 0){
  
  ns <- session$ns
  param_names <- names(var_params_list)
  div_id <-paste0("#",ns(""),"added_ui")
  data4module  <- list("")
  data4module$current_list <- list()
  # type list for distribution
  type_var2name <- list("norm"= "Normal","lnorm"="Log-normal")
  type_name2var <- list("Normal" = "norm","Log-normal"="lnorm")
  if (ops_type == "edit"){
    query <- sprintf("Select name,descrp,var_tble from Variability where varid = %d",set_id)
    ret_data <- projectDbSelect(query)
    var_data <- unserialize(charToRaw(ret_data[["var_tble"]]))
    selected_vals <- var_data$Parameter
    data4module$current_list <<- selected_vals
    updatePickerInput(session,"param_names",choices = var_params_list,selected = selected_vals)
  }else{
    # get the current ID for the parameter set.
    query <- sprintf("SELECT varid FROM Variability;")
    id_list <- projectDbSelect(query)
    
    if (length(id_list[["varid"]])==0){
      set_id = 1
    }else{
      set_id = max(id_list[["varid"]])+1
    }
    updatePickerInput(session,"param_names",choices = var_params_list)
  }
  
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
  # Save the table
  observeEvent(input$ok,{
    input_list <- reactiveValuesToList(input)
    # get the ui elements that start with "cv" to find out which parameters to save
    # this is because not all elements in the drop down list may actually be present
    save_vars <- unlist(sub("cv_","",names(input_list )[which(startsWith(names(input_list ),"cv_"))]))
    save_names <- unlist(param_names[which(var_params_list %in% save_vars)])
    cvs <- unlist(input_list[paste0("cv_",save_vars)])
    types <- unlist(lapply(input_list[paste0("type_",save_vars)],function(x){type_var2name[[x]]}))
    var_tble <- data.frame("Name"=save_names,
                           "Parameter"=save_vars,
                           "CV" = cvs,"Type" = types,
                           stringsAsFactors = F)
    var_tble_serialized<- rawToChar(serialize(var_tble,NULL,T))
    name <- "test1"#input$name
    descrp <- "test1 descrp"#input$descrp
    query <-sprintf("Insert Into Variability (varid,name,descrp,type,var_tble) Values (%d,'%s','%s','%s','%s');",
                    set_id,name,descrp,set_type,var_tble_serialized)
    projectDbUpdate(query)
  })
  
}
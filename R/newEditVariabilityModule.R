#' UI function for defining variability and uncertainty datasets
#' @description UI function for defining variability and uncertainty datasets in the rapidPBPK model. This should not be called by the user
#' @param namespace namespace for the module when it is called form the PBPK UI
#' @export
newEditVariabilityUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(
    title = "Variability",
    size = "l",
    tagList(
      fluidRow(
        column(4,
               textInput(ns("name"),NULL,placeholder = "Name")
               ),
        column(8,
               textInput(ns("descrp"),NULL,placeholder = "Description"))
        ),
      fluidRow(
        column(8,
               pickerInput(ns("param_names"),
                           label = "Select Parameters to Assign Variability",
                           choices = NULL,multiple = TRUE
                           options = list('selected-text-format' = "count > 3",
                                          'live-search'=TRUE,
                                          'size'=10)
                        )
               ),
        column(4,
               actionButton(ns("update"),
                            "Update List",color = "default")
               )
      ),
      tags$div(id = ns("added_ui"),
               fluidRow(
                 column(3,
                        tags$h5("Parameter Name")
                        ),
                 column(2,
                        tags$h5("Coefficient of Variation")
                        ),
                 column(2,
                        tags$h5("Type of Distribution")
                        ),
                 column(1,
                        tags$h5("Use Limits")
                        ),
                 column(2,
                        tags$h5("Upper Limit")
                        ),
                 column(2,
                        tags$h5("Lower Limit")
                        )
               ))
    ),
    footer = tagList(
      actionButton(ns("ok"),"Done"),
      modalButton("Cancel")
    )
  ))
}

#' Server function for defining variability and uncertainty datasets
#' @description Server function for defining variability and uncertainty datasets in the rapidPBPK model. This should not be called by the user
#' @param input input object from UI
#' @param output output object to the UI
#' @param session session object for this module
#' @param set_type type of set for which variability is defined
#' @param ops_type Operation requested. new variability or edit existing
#' @param var_params_list List of parameters for variability
#' @param set_id id for the variability set in the database 
#' @export
newEditVariability <- function(input,output,session,set_type,ops_type,var_params_list,set_id = 0){
  
  returnValues <- reactiveValues()
  returnValues$savedat <- c("No","",0)
  ns <- session$ns
  param_names <- names(var_params_list)
  div_id <-paste0("#",ns(""),"added_ui")
  data4module  <- list()
  data4module$current_list <- list()
  # type list for distribution
  type_var2name <- list("norm"= "Normal","lnorm"="Log-normal","uform"="Uniform")
  type_name2var <- list("Normal" = "norm","Log-normal"="lnorm","Uniform"="uform")
  if (ops_type == "edit"){
    query <- sprintf("Select name,descrp,var_tble from Variability where varid = %d",
                     as.integer(set_id))
    ret_data <- projectDbSelect(query)
    var_data <- unserialize(charToRaw(ret_data[["var_tble"]]))
    selected_vals <- var_data$Parameter
    data4module$current_list <- selected_vals
    updatePickerInput(session,"param_names",choices = var_params_list,selected = selected_vals)
    for (x in seq(dim(var_data)[1])){
      param <- var_data$Parameter[x]
      name <- var_data$Name[x]
      cv <-var_data$CV[x]
      type <- type_name2var[[var_data$Type[x]]]
      ubound <- var_data$UpperBound[x] # change this to reflect data saved in table
      lbound <- var_data$LowerBound[x] # change this to reflect data saved in table
      bflag <- var_data$BFlag[x]
      insertUI(selector = div_id,
               ui = tagList(
                 tags$div(id = ns(paste0("div_",param)),
                          fluidRow(
                            column(3,
                                   name
                            ),
                            column(2,
                                   numericInput(ns(paste0("cv_",param)),label = NULL,min = 0,max = 1,step = 0.01,value = as.numeric(cv))
                            ),
                            column(2,
                                   selectInput(ns(paste0("type_",param)),label = NULL,selected = as.character(type),
                                               choices = c("Normal"="norm",
                                                           "Log-normal"="lnorm"))#,
                                                           #"Unifrom"= "uform"))
                            ),
                            column(1,
                                   checkboxInput(ns(paste0("bflag_",param)),label = NULL,value = as.logical(bflag))
                                   ),
                            column(2,
                                   numericInput(ns(paste0("ubound_",param)),label = NULL, 
                                                step = 0.1,value = as.numeric(ubound))),
                            column(2,
                                   numericInput(ns(paste0("lbound_",param)),label = NULL, 
                                                step = 0.1,value = as.numeric(lbound)))
                          )
                 )
               )
      )
    }
    updateTextInput(session,"name",value = ret_data$name)
    updateTextInput(session,"descrp",value = ret_data$descrp)
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
  
  
  observeEvent(input$update,{
    current_list <- data4module$current_list
    #print(current_list)
    add_list <- setdiff(input$param_names,current_list)
    #print(add_list)
    remove_list <- setdiff(current_list,input$param_names)
    #print(remove_list)
    # if new elements are selected, add UI
    if(length(add_list) != 0){
      for(x in add_list){
        insertUI(selector = div_id,
                 ui = tagList(
                   tags$div(id = ns(paste0("div_",x)),
                            fluidRow(
                              column(3,
                                     param_names[which(var_params_list == x)]
                                     ),
                              column(2,
                                     numericInput(ns(paste0("cv_",x)),label = NULL,min = 0,max = 1,step = 0.01,value = 0)
                              ),
                              column(2,
                                     selectInput(ns(paste0("type_",x)),label = NULL,selected = "norm",
                                                 choices = c("Normal"="norm",
                                                             "Log-normal"="lnorm"))#,
                                     #"Unifrom"= "uform"))
                              ),
                              column(1,
                                     checkboxInput(ns(paste0("bflag_",x)),label = NULL,value = F)
                              ),
                              column(2,
                                     numericInput(ns(paste0("ubound_",x)),label = NULL, 
                                                  step = 0.1,value = 0)),
                              column(2,
                                     numericInput(ns(paste0("lbound_",x)),label = NULL, 
                                                  step = 0.1,value = 0))
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
                 immediate = TRUE
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
    bound_flags <- unlist(lapply(input_list[paste0("bflag_",save_vars)],
                                function(x){as.character(x)}))
    ubounds <- unlist(input_list[paste0("ubound_",save_vars)])
    lbounds <- unlist(input_list[paste0("lbound_",save_vars)])
    var_tble <- data.frame("Name"=save_names,
                           "Parameter"=save_vars,
                           "CV" = cvs,"Type" = types,
                           "BFlag"=bound_flags,
                           "UpperBound"=ubounds,
                           "LowerBound"=lbounds,
                           stringsAsFactors = F)
    var_tble_serialized<- rawToChar(serialize(var_tble,NULL,T))
    name <- input$name
    descrp <- input$descrp
    if (ops_type == "new"){
      query <-sprintf("Insert Into Variability (varid,name,descrp,type,var_tble) Values (%d,'%s','%s','%s','%s');",
                      set_id,name,descrp,set_type,var_tble_serialized)
    }else{
     
      query <- sprintf("Update Variability Set name = '%s', descrp = '%s',type = '%s',var_tble = '%s' Where varid = %d;",
                       name,descrp,set_type,var_tble_serialized,as.integer(set_id))
    }
    
    projectDbUpdate(query)
    removeModal()
  })
  returnValues$savedat<- eventReactive(input$ok,{
    return(c("Yes",set_type,set_id))
    })
  return(returnValues$savedat)
}
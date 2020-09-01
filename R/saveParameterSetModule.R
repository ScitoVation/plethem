# This module for creating a new physiological, chemical or Exposure entry in the project database

#'UI for saving a new physiological, chemical or exposure set to the project database
#'@description UI for saving parameter sets. This function should not be called by the user
#'@param namespace namespace for the UI
#'@param set_type type of parameter set to save
#'@export
saveAsParameterSetUI <- function(namespace, set_type){
  shinyjs::useShinyjs()
  ns <- NS(namespace)
  
  set_name <- switch(set_type,
                     "physio" = "Physiological",
                     "chem" = "Chemical",
                     "expo" = "Exposure",
                     "adme"="Adme")
  id_name <- paste0(set_type,"id")
  set_table_name <- paste0(set_name,"Set")
  
  # get the current ID for the parameter set.
  
  query <- sprintf("SELECT %s FROM %s;",id_name,set_table_name)
  id_list <- projectDbSelect(query)
  
  if (length(id_list[[id_name]])==0){
    id_num = 1
  }else{
    id_num = max(id_list[[id_name]])+1
  }
  showModal(modalDialog(title = paste0("Save ",set_name," Parameter Set"),easyClose = TRUE,
                        tagList(
                          textInput(ns("name"),"Parameter Set Name",value = paste0(set_name," Set ",id_num)
                                   ),
                          textInput(ns("descrp"),"Description",value = "Description",
                                    placeholder = "Enter description for the dataset"),
                          shinyjs::hidden(textInput(ns("cas"),"CAS Number",placeholder = "Enter CAS Number"))
                        ),
                        footer= tagList(
                          bsButton(ns("add"),"Add",type = "action"),
                          modalButton("Cancel")
                        )
                        )
            )
}

#'server side function for saving a new physiological, chemical or exposure set to the project database
#' @description Server side function for running the save parameter module. This function should not be called by the user
#' @param input input object for the UI
#' @param output input object to the UI
#' @param session session object for the module
#' @param set_type type of parameter set to save
#' @param main_input input from the pbpk UI
#' @param name_df variable names for parameters
#' @param other placeholder paramter for data needed for certain sets
#'@export
saveAsParameterSet <- function(input,output,session,set_type,main_input,name_df,other= NULL){
  print("save parameter set")
  returnValues <- reactiveValues()
  returnValues$savedat <- c("No","",0)
  ns <- session$ns
  if(set_type == "chem"){
    shinyjs::show("cas")
  }
  set_name <- switch(set_type,
                     "physio" = "Physiological",
                     "chem" = "Chemical",
                     "expo" = "Exposure",
                     "adme"="Adme")
  id_name <- paste0(set_type,"id")
  set_table_name <- paste0(set_name,"Set")
  vals_table_name<- set_name

  # get the current ID for the parameter set.
  query <- sprintf("SELECT %s FROM %s;",id_name,set_table_name)
  id_list <- projectDbSelect(query)

  if (length(id_list[[id_name]])==0){
    id_num = 1
  }else{
    id_num = max(id_list[[id_name]])+1
  }
  returnValues$savedat<- eventReactive(input$add,{return(c("Yes",set_type,id_num))})


  
  observeEvent(input$add,{
    main_input <- reactiveValuesToList(main_input)

    # write the name to correct "Set" table
    if (set_type == "chem"){
      query <- sprintf("INSERT INTO %s (%s, name, descrp,cas) VALUES (%d,'%s','%s','%s');",
                       set_table_name,id_name,id_num,input$name,input$descrp,input$cas)
      projectDbUpdate(query)
      
    }else if (set_type == "adme"){
      expoid <- other[[1]]
      chemid <- other[[2]]
      physioid <- other[[3]]
      metabid <- other[[4]]
      query <- sprintf("INSERT INTO %s (%s,name,descrp,expoid,chemid,physioid,metabid) VALUES (%d,'%s','%s', %d, %d, %d,%d );",
                       set_table_name,id_name,id_num,input$name,input$descrp,
                       expoid,
                       chemid,
                       physioid,
                       metabid)
      projectDbUpdate(query)
    }else{
      query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                       set_table_name,id_name,id_num,input$name,input$descrp)
      projectDbUpdate(query)
    }
    

    # get all values for parameter set
    write_col_names <- sprintf("%s, param, value",id_name)
    var_names <- name_df$Var#[! name_df$Var %in% c("gender","brep_flag","ivrep_flag")]
    ui_names <- unlist(lapply(var_names,function(x){paste0("ms_",x)}))
    values <- as.character(main_input[ui_names])
    names(values) <- NULL
    values <- paste0("'",values,"'")
    all_values_string <- paste(paste0(sprintf('(%d,',id_num),sprintf("'%s'",var_names),',',values,')'),collapse = ", ")
    #write the parameter set
    query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",vals_table_name, write_col_names,all_values_string)
    projectDbUpdate(query)
    removeModal()
  },ignoreNULL = T,ignoreInit = T)
  return(returnValues$savedat)
}


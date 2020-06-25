# This module describes the import dialog for parameter sets.Currently only chemicals are supported

#'UI for importing parameter sets
#' @description UI for importing parameters from user or main databases
#' @param namespace namespace for this module
#' @param set_type type of data to be imported; physiological, chemical or exposure
#'
#'@export
importParameterSetUI <- function(namespace,set_type){
  shinyjs::useShinyjs()
  ns <- NS(namespace)
  set_name <- switch(set_type,
                     "physio" = "Physiological",
                     "chem" = "Chemical",
                     "expo" = "Exposure")
  showModal(modalDialog(
    title =paste0("Import ",set_name),size = "m",
    tagList(tabsetPanel(id = ns("src_type"),selected = "main",
                        
                        # # tabPanel("New",value = "new",
                        # #          DT::DTOutput(ns("new_tble"))),
                       

                                 #DTOutput(ns("user_tble"))),
                        tabPanel("PLETHEM Database",value = "main",
                                 pickerInput(ns("sel_main"),
                                                label = "Select Chemical",multiple = T,
                                                choices = NULL)),
                        tabPanel("Import from file",value = "batch",
                                 fileInput(ns("btn_batch_upload"),"Upload File",
                                           multiple = F),
                                 radioButtons(ns("rdo_ftype"),
                                              label = "Select file type",
                                              choices = list("Chemical Input File"="chem_batch",
                                                             "OPERA Predictions"="opera_predictions"))
                                 ),
                        tabPanel("User Database",value = "user",
                                 bsButton(ns("btn_userDb_file"),
                                          "Select User Database",
                                          block = T),
                                 pickerInput(ns("sel_user"),
                                                label = "Select Chemical",
                                                choices = NULL,multiple = T)
                                 )
                               # DTOutput(ns("main_tble")))




                   )
    ),
    footer =tagList(actionButton(ns("import"),"Import"),
      modalButton("Dismiss")
    )
  ))
}

#'Server for import parameter module
#'@description server function for importing parameter sets from user databases. This function should never be called by the user
#'@param input input object from the UI
#'@param output output object for the UI
#'@param session session object for the server
#'@param set_type type of data to be imported
#'@export
importParameterSet <- function(input,output,session,set_type){
  if(set_type == "chem"){
    shinyjs::show("cas")
  }
  returnValues <- reactiveValues()
  returnValues$retdata <- c("No","",0)
  ns <- session$ns

  set_name <- switch(set_type,
                     "physio" = "Physiological",
                     "chem" = "Chemical",
                     "expo" = "Exposure")
  id_name <- paste0(set_type,"id")
  set_table_name <- paste0(set_name,"Set")
  vals_table_name<- set_name

  # get the current ID for the parameter set from project database
  query <- sprintf("SELECT %s FROM %s ;",id_name,set_table_name)
  id_list <- projectDbSelect(query)

  if (length(id_list[[id_name]])==0){
    id_num = 1
  }else{
    id_num = max(id_list[[id_name]])+1
  }

  all_sets_query <- sprintf("SELECT * FROM %s ;",set_table_name)

  #Server operations for main table
  main_vals <- mainDbSelect(all_sets_query)
  main_chem_list <- as.list(main_vals[[id_name]])
  names(main_chem_list)<-main_vals$name
  updatePickerInput(session,"sel_main", choices = main_chem_list)

  #Server operations for user table
  db_path <- mainDbSelect("Select value FROM Utils where variable = 'UserDbPath'")$value
  if (!(is.na(db_path))){
    user_vals <- userDbSelect(all_sets_query)
    user_chem_list <- as.list(user_vals[[id_name]])
    names(user_chem_list)<-user_vals$name
    updatePickerInput(session,"sel_user", choices = user_chem_list)
  }
  fpath_userDb<- eventReactive(input$btn_userDb_file,{
    fpath <- getFileFolderPath(type = "file","Select User Database","*.sqlite")
    return(fpath)
  },ignoreInit = T)
  observe({
    fpath <- fpath_userDb()
    if (length(fpath)==0){
      sendSweetAlert(session,"No File Selected",type = "error",closeOnClickOutside = T)
    }else{
      sendSweetAlert("session","File Selected")
      query <- sprintf("Update Utils Set value = '%s' Where variable = 'UserDbPath';",fpath)
      mainDbUpdate(query)
      user_vals <- userDbSelect(all_sets_query)
      user_chem_list <- as.list(user_vals[[id_name]])
      names(user_chem_list)<-user_vals$name
      updatePickerInput(session,"sel_user", choices = user_chem_list)
    }
      
    
  })

  
  batch_data <- reactive({
    req(input$btn_batch_upload)
    tryCatch(
      {
        datafile <- read.csv(input$btn_batch_upload$datapath,header = T)
      },
      error = function(e){
        stop(safeError(e))
      }
    )
    return(datafile)
  })
  
  observeEvent(input$btn_batch_upload,{
    batch_file <- reactive({
      input$btn_batch_upload
    })
    batch_fpath <- batch_file()$datapath
    batchdata <- reactive({
      
    })
  },ignoreInit = T)

  observeEvent(input$import,{
    userDbIds <- input$sel_user
    mainDbIds <- input$sel_main
    for (user_id in userDbIds){
     
      name_query <- sprintf("SELECT name from %s where %s = %s",
                            set_table_name,id_name,user_id)
      name <- userDbSelect(name_query)$name
      #print(name)
      query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%s, '%s' , '%s' );",
                         set_table_name,
                         id_name,
                         id_num,
                         name,
                         "Imported from user database")
      projectDbUpdate(query)
      #print (query)
      vals_query <- sprintf("SELECT param,value FROM %s where %s = %s;",
                            set_name,id_name,user_id)
      tble <- userDbSelect(vals_query)
      tble <- tble[c(1,2)]
      colnames(tble)<- c("var","val")
      write_col_names <- sprintf("%s, param, value",id_name)
      var_names <- tble$var
      names(var_names) <- NULL
      values <- tble$val
      names(values) <- NULL
      values <- paste0("'",values,"'")
      all_values_projectDb_string <- paste(paste0(sprintf('(%d,',id_num),
                                                  sprintf("'%s'",var_names),
                                                  ',',values,')'),
                                           collapse = ", ")
      query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                        vals_table_name,
                        write_col_names,
                        all_values_projectDb_string)
      
      projectDbUpdate(query)
      #print(query)
      id_num <- id_num + 1
      #print(table)
    }
    for (main_id in mainDbIds){
      
      name_query <- sprintf("SELECT name from %s where %s = %s",
                            set_table_name,id_name,main_id)
      name <- mainDbSelect(name_query)$name
      #print(name)
      query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%s, '%s' , '%s' );",
                       set_table_name,
                       id_name,
                       id_num,
                       name,
                       "Imported from main database")
      projectDbUpdate(query)
      #print (query)
      vals_query <- sprintf("SELECT param,value FROM %s where %s = %s;",
                            set_name,id_name,main_id)
      tble <- mainDbSelect(vals_query)
      tble <- tble[c(1,2)]
      colnames(tble)<- c("var","val")
      write_col_names <- sprintf("%s, param, value",id_name)
      var_names <- tble$var
      names(var_names) <- NULL
      values <- tble$val
      names(values) <- NULL
      values <- paste0("'",values,"'")
      all_values_projectDb_string <- paste(paste0(sprintf('(%d,',id_num),
                                                  sprintf("'%s'",var_names),
                                                  ',',values,')'),
                                           collapse = ", ")
      query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                       vals_table_name,
                       write_col_names,
                       all_values_projectDb_string)
      
      projectDbUpdate(query)
      id_num <- id_num + 1

    }
    sendSweetAlert(session,NULL,"Chemicals imported to the HT-IVIVE Project")

    removeModal()
  })

  # if imported return the correct values back to the UI for update
  returnValues$retdata<- eventReactive(input$import,{return(c("Yes",set_type,id_num))})
  return(returnValues$retdata)


}

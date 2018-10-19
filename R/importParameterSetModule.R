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
    tagList(tabsetPanel(id = ns("src_type"),selected = "user",
                        # tabPanel("New",value = "new",
                        #          DT::DTOutput(ns("new_tble"))),
                        tabPanel("User Database",value = "user",
                                 selectizeInput(ns("sel_user"),
                                                label = "Select Chemical",
                                                choices = NULL),

                                 DTOutput(ns("user_tble"))),
                        tabPanel("Main Database",value = "main",
                                 selectizeInput(ns("sel_main"),
                                                label = "Select Chemical",
                                                choices = NULL),
                                 DTOutput(ns("main_tble")))




                   ),
            textInput(ns("name"),"Name",placeholder = "Chemical Name"),
            textAreaInput(ns("descrp"),"Description"),
            shinyjs::hidden(textInput(ns("cas"),"CAS Number",
                                      placeholder = "Enter CAS Number")),

            checkboxInput(ns("add2Db"),label = "Add to user database")
    ),
    footer =tagList(
      shinyjs::disabled(actionButton(ns("import"),"Import")),
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
  tables <- reactiveValues()
  ids <-reactiveValues()

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

  # get the current ID for the parameter set from user database
  query <- sprintf("SELECT %s FROM %s ;",id_name,set_table_name)
  user_id_list <- userDbSelect(query)

  if (length(user_id_list[[id_name]])==0){
    user_id_num = 1
  }else{
    user_id_num = max(as.integer(user_id_list[[id_name]]))+1
  }

  all_sets_query <- sprintf("SELECT * FROM %s ;",set_table_name)

  #col_names <- c("Name","Variable","Original Value","Updated Value")
  #Server operations for main table
  main_vals <- mainDbSelect(all_sets_query)
  main_chem_list <- as.list(main_vals[[id_name]])
  names(main_chem_list)<-main_vals$name
  updateSelectizeInput(session,"sel_main", choices = main_chem_list)

  # # populate the new chemical table
  # # get all the chemical variables
  # if (set_name == "Chemical"){
  #   query <- "SELECT Var FROM ParamNames Where ParamSet = 'Chemical' and ParamType = 'Numeric';"
  #   all_chem_params <- mainDbSelect(query)$Var
  # }
  # query <- "SELECT Var FROM ParamNames Where ParamSet = 'Chemical' and ParamType = 'Numeric';"
  # all_chem_params <- mainDbSelect(query)$Var
  # new_chem_table <- data.frame("Var" = all_chem_params,
  #                              "org_val"=rep(0,length(all_chem_params)),
  #                              "import_val"=rep(0,length(all_chem_params)),
  #                              stringsAsFactors = F)
  # output$new_tble <- DT::renderDataTable(DT::datatable(new_chem_table,
  #                                                       rownames = F,
  #                                                       escape = T,
  #                                                       selection = "none",
  #                                                       colnames = c("Variable","Original Value","Imported Value"),
  #                                                       autoHideNavigation = T,
  #                                                       editable = F,
  #                                                       options = list(dom = "tp",pageLength=5)
  # 
  # )
  # ,server = TRUE)
  # new_proxy = DT::dataTableProxy('new_tble')
  # 
  # observeEvent(input$new_tble_cell_edit,{
  #   databck <- input$new_tble_cell_edit
  #   row <- databck$row
  #   col <- databck$col+1
  #   val <- databck$value
  #   old_table <- new_chem_table
  #   old_table[row,col]<- val
  #   new_chem_table<<- old_table
  #   tables$new_store<-reactiveVal(old_table)
  #   #tables$new_disp<-reactiveVal(old_table)
  #   DT::replaceData(new_proxy,old_table,resetPaging = F,rownames = F)
  # })
  # 



  observeEvent(input$sel_main,{

    id <- input$sel_main
    if (id == ""){
      id = "1"
    }
    vals_query <- sprintf("SELECT param,value FROM %s where %s = %s;",
                          set_name,id_name,id)
    table <- mainDbSelect(vals_query)
    #table <-cbind(table,table[2])
    colnames(table)<- NULL
    if (set_type == "chem"){
      meta_query <- sprintf("SELECT name,cas FROM %s where %s = %s",set_table_name,id_name,id)
      metadata <- mainDbSelect(meta_query)
      updateTextInput(session,"cas",value = metadata$cas)
    }else{
      meta_query <- sprintf("SELECT name FROM %s where %s = %s",set_table_name,id_name,id)
      metadata <- mainDbSelect(meta_query)
    }
    updateTextInput(session,"name",value = metadata$name)
    ids$sel_user_id_num <<- reactiveVal(id)
    #table <- table[,c(3,1,2)]
    tables$main<<-reactiveVal(table)
    },ignoreInit = T,ignoreNULL = T)

  output$main_tble <- DT::renderDataTable(DT::datatable(tables$main(),
                                                        rownames = F,
                                                        escape = T,
                                                        selection = "none",
                                                        colnames = c("Variable","Value"),
                                                        autoHideNavigation = T,
                                                        editable = F,
                                                        options = list(dom = "tp",pageLength=10)

                                                        )
                                          ,server = TRUE)
    # main_proxy = DT::dataTableProxy('main_tble')

  # observeEvent(input$main_tble_cell_edit,{
  #   databck <- input$main_tble_cell_edit
  #   row <- databck$row
  #   col <- databck$col+1
  #   val <- databck$value
  #   old_table <- tables$main_store()
  #   old_table[row,col]<- val
  #   tables$main_store<-reactiveVal(old_table)
  #   tables$main<-reactiveVal(old_table)
  #   DT::replaceData(main_proxy,tables$main(),resetPaging = F,rownames = F)
  # })
  #Server operations for user table
  user_vals <- userDbSelect(all_sets_query)
  user_chem_list <- as.list(user_vals[[id_name]])
  names(user_chem_list)<-user_vals$name
  updateSelectizeInput(session,"sel_user", choices = user_chem_list)

  observeEvent(input$sel_user,{

    id <- input$sel_user
    if (id == ""){
      id = "1"
    }
    vals_query <- sprintf("SELECT param,value FROM %s where %s = %s;",
                          set_name,id_name,id)
    table <- userDbSelect(vals_query)
    #table <-cbind(table,table[2])
    colnames(table)<- NULL
    if (set_type == "chem"){
      meta_query <- sprintf("SELECT name,cas FROM %s where %s = %s",
                            set_table_name,id_name,id)
      metadata <- userDbSelect(meta_query)
      if (input$src_type != "new"){
        updateTextInput(session,"cas",value = metadata$cas)
      }else{
        updateTextInput(session,"cas",value = NULL,placeholder = "Enter CAS-RN")
      }
      

    }else{
      meta_query <- sprintf("SELECT name FROM %s where %s = %s",set_table_name,id_name,id)
      metadata <- userDbSelect(meta_query)
    }
    if (input$src_type != "new"){
      updateTextInput(session,"name",value = metadata$name)
    }else{
      updateTextInput(session,"name",value = NULL,placeholder = "Enter Chemical Name")
    }

    ids$sel_user_id_num <<- reactiveVal(id)
    #table <- table[,c(3,1,2)]
    tables$user<<-reactiveVal(table)
  },ignoreInit = T,ignoreNULL = T)

  output$user_tble <- DT::renderDataTable(DT::datatable(tables$user(),
                                                        rownames = F,escape = T,selection = "none",
                                                        colnames = c("Variable","Original Value"),
                                                        autoHideNavigation = T,
                                                        editable = F,
                                                        options = list(dom = "tp",pageLength=5)

  )
  ,server = TRUE)
  # user_proxy = DT::dataTableProxy('user_tble')
  # 
  # observeEvent(input$user_tble_cell_edit,{
  #   databck <- input$user_tble_cell_edit
  #   row <- databck$row
  #   col <- databck$col+1
  #   val <- databck$value
  #   old_table <- tables$user()
  #   old_table[row,col]<- val
  #   tables$user<-reactiveVal(old_table)
  #   tables$user_disp<-reactiveVal(old_table)
  #   DT::replaceData(user_proxy,tables$user_disp(),resetPaging = F,rownames = F)
  # })

  checkData <- reactive({
    req(input$name,input$descrp)
  })
  observe({
    if(checkData() != ""){
      shinyjs::enable("import")
    }
  })

  observeEvent(input$import,{
    if(input$src_type == "main"){
      tble <- tables$main()
      current_id_num <- 0
    }else if (input$src_type == "user"){
      tble <- tables$user()
      current_id_num <- as.integer(ids$sel_user_id_num())
    }else{
      tble <- tables$new_store()
      current_id_num <- 0
    }
    tble <- tble[c(1,2)]
    colnames(tble)<- c("var","val")
    # Conver table values to query that can be used with dbs
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
    all_values_userDb_string <- paste(paste0(sprintf('(%d,',user_id_num),
                                              sprintf("'%s'",var_names),
                                              ',',values,')'),
                                       collapse = ", ")


    #update user database
    if(input$add2Db==T){
      query <- sprintf("INSERT INTO %s (%s, name, descrp,cas) VALUES (%d, '%s' , '%s','%s' );",
                       set_table_name,id_name,user_id_num,input$name,input$descrp,input$cas)
      userDbUpdate(query)
      query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                       vals_table_name,
                       write_col_names,
                       all_values_userDb_string)
      userDbUpdate(query)
    }
    # update project database
    if (set_type == "chem"){
      query <- sprintf("INSERT INTO %s (%s,%s, name, descrp,cas) VALUES (%d,%d, '%s' , '%s','%s' );",
                       set_table_name,
                       id_name,
                       "extchemid",
                       id_num,
                       ifelse(input$add2Db == T,user_id_num,current_id_num),
                       input$name,
                       input$descrp,
                       input$cas)
      projectDbUpdate(query)
      
    }else{
      query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                       set_table_name,
                       id_name,
                       id_num,
                       input$name,
                       input$descrp)
      projectDbUpdate(query)
    }

    
    query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                     vals_table_name,
                     write_col_names,
                     all_values_projectDb_string)

    projectDbUpdate(query)

    removeModal()
  })

  # if imported return the correct values back to the UI for update
  returnValues$retdata<- eventReactive(input$import,{return(c("Yes",set_type,id_num))})
  return(returnValues$retdata)


}

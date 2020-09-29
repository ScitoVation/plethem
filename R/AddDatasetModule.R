#' UI for adding external observation datasets to projects
#' @description The UI function for the add dataset module used in PLETHEM. This function creates the add dataset dialog
#'   box that is triggered by the modeling interface. This is a module function and the user will never need to call it
#' @param namespace the namespace for this module
#' @param data_type the type of data to be uploaded based on where the UI is called from
#' @export

addDataSetUI <- function(namespace, data_type){

  # Create namespace variable
  ns <- NS(namespace)
  showModal(modalDialog(title = "Add Dataset",easyClose = TRUE,size = "l",
                        tagList(
                          tags$h4(paste0("Dataset for ",data_type)),
                          textInput(ns("name"),"Dataset Name",placeholder = "Enter Name for the dataset"),
                          textInput(ns("descrp"),"Description",placeholder = "Enter description for the dataset"),
                          fluidRow(
                            column(6,
                                   shinyWidgets::radioGroupButtons(ns("type"),"Select Data Type",
                                                                   choices = c("Tissue Concentration"="conc"))
                                   ),
                            column(6,
                                   uiOutput(ns("unit_ui"))

                            )
                          ),
                          fileInput(ns("file"),label = "Select CSV file",
                                    accept = c("text/csv","text/comma-separated-values",".csv"),multiple = TRUE),
                          DT::DTOutput(ns("obs_tble"))
                        ),
                        footer= tagList(
                          shinyjs::disabled(actionButton(ns("add"),"Add Dataset")),
                          modalButton("Cancel")
                        )
  )
  )
}

#' Server side function for the UI used to add external observation datasets to projects
#' @description The sever function for the add dataset module used in PLETHEM. This function interacts with the server function
#'   of the PLETHEM model. It saves the dataset imported as a .RDS file in the project folder. It also adds the dataset reference
#'   to the project database for further use.
#' @param input the input object from the add dataset module UI
#' @param output the output object from the add dataset module UI
#' @param session the shiny session information where the add dataset in called currently
#' @param data_type the data_type returned by the module.
#' @export
addDataSet <- function(input,output,session,data_type){
  returnValues <- reactiveValues()
  returnValues$savedat <- c("No","none")
  ns <- session$ns
  # data.path <- system.file("datasets",package = "plethem.r.package")
  # conn <- RSQLite::dbConnect(RSQLite::SQLite(),paste0(data.path,"/datadb.sqlite"))
  observeEvent(input$type,{
    #print(input$dtype)
    if(input$type == "conc"){
      choices = c("mg/l"="mgl")
    }else{
      choices = c("mg"="mg")
    }
    output$unit_ui <- renderUI({
      shinyWidgets::radioGroupButtons(ns("unit"),label = "Select Data Unit",choices = choices)
    })
  })
  # The selected file, if any
  userFile <- reactive({
    input$file
  })

  # The user's data, parsed into a data frame
  obs_tble <- reactive({
    validate(need(input$file, message = "No Dataset Uploaded"))
    shinyjs::enable("add")
    utils::read.csv(userFile()$datapath)
  })
  output$obs_tble <- DT::renderDT(DT::datatable(obs_tble(),rownames = FALSE))

  returnValues$savedat<- eventReactive(input$add,{return(c("Yes",input$type))})

  observeEvent(input$add,{
    name <- input$name
    descrp <- input$descrp
    if (descrp==""){
      descrp <- "No Description"
    }

    type <- input$dtype
    units <- input$dunit.

    id_name <- "obsid"
    set_table_name <- "ObservationSet"
    set_name <- "Observation"
    # get the current ID for the parameter set.
    query <- sprintf("SELECT %s FROM %s ;",id_name,set_table_name)
    id_list <- projectDbSelect(query)
    if (length(id_list[[id_name]])==0){
      id_num = 1
    }else{
      id_num = max(id_list[[id_name]])+1
    }
    obs_type <- input$type
    units <- input$unit


    # write the name to correct "Set" table
    query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                     set_table_name,id_name,id_num,
                     name,descrp)
    projectDbUpdate(query)

    # serialize and convert the loaded table to database
    serialized_obs_tble <- rawToChar(serialize(obs_tble(),NULL,T))

    query <- sprintf("INSERT INTO Observation (obsid,type,units,obs_tble) Values (%d,'%s','%s','%s');",
                     id_num,
                     obs_type,
                     units,
                     serialized_obs_tble)
    projectDbUpdate(query)

    # set_list <- getAllSetChoices(set_type)
    # query <- sprintf("INSERT INTO datasets (name,description,type,units) VALUES ('%s','%s','%s','%s');",name,descrp,type,units)
    # print(query)
    # RSQLite::dbExecute(conn,query)
    # saveRDS(dataframe(),paste0(data.path,"/",name,".rds"))
    # RSQLite::dbDisconnect(conn)
    removeModal()
  })
  return(returnValues$savedat)
}

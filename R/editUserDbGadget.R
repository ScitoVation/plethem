#' Gadget for creating/editing the a user database
#' @description This function defines the UI and server for creating a new user database. It accepts CSV files from users for chemicals and exposure.
#' @param file_path Path of the database where changes are to be made
#' @export
#' 
editUserDb <- function(file_path){
  if (file_path==""){
    base_path <- getFileFolderPath(type = "dir",caption="Select location for user database")
    file_path <- file.path(base_path,"userDb.sqlite")
  }
  
  ui <- miniPage(
    
    gadgetTitleBar(title = "User Database",left = miniTitleBarCancelButton("cancel"),
                 right = miniTitleBarButton("ok","Save changes")),
    miniTabstripPanel(id = "tabpanel",
                      between = tagList(
                                pickerInput("model_type","Select model type",
                                    choices = list("rapidPBPK","fishPBPK"),
                                                   
                                    inline = T,multiple = F,
                                    width = validateCssUnit("100%"))
                      ),
                      miniTabPanel("Chemical",
                                   miniContentPanel(
                                     fillPage(
                                       
                                       fillCol(flex = c(1,3),
                                         fillRow(flex = c(3,2,2),
                                                 fileInput("chemfile_upload","Select Chemical File"),
                                                 awesomeRadio("type_select",inline = T, 
                                                              label = "Select data source",
                                                              choices = list("Chemical Batch File"="batch",
                                                                             "OPERA predictions"="opera")),
                                                 awesomeCheckbox("chem_select","Select All Rows",
                                                                 value = F,status = "warning",
                                                              width = validateCssUnit("100%"))
                                         ),
                                         fillRow(
                                           DT::DTOutput("import_chem_data")
                                         )
                                         
                                       )
                                     )
                                     
                                   )),
                      miniTabPanel("Exposure",
                                   miniContentPanel(
                                   )))
    
  )
  server <- function(input,output,session){
    observeEvent(input$cancel,{
      stopApp(returnValue = "No changes saved")
    })
    observeEvent(input$ok,{
      chem_rows <- input$import_chem_data_rows_selected
      chem_save_data <- chem_data()[chem_rows,]
      writeChemData(chem_save_data,file_path,input$type_select)
      stopApp(returnValue = "Database updated")
    })
    # The selected chemical file
    chemFile <- reactive({
      input$chemfile_upload
    })
    
    # The user's data, parsed into a data frame
    chem_data <- reactive({
      validate(need(!(is.null(input$chemfile_upload)),
                    message = "No file selected"))
      
      #req(input$chem_file_upload)
      if(!(is.null(input$chemfile_upload))){
        data_file <- chemFile()$datapath
        data_type <- input$type_select
        if(data_type == "batch"){
          imported_data <- readBatchChemicalFile(data_file)
        }else{
          imported_data <- readOperaPredictions(data_file)
        }
        
      }
      
      return(imported_data)
      
    })
    chem_view_table <- reactive({DT::datatable(chem_data(),
                                               caption = NULL,
                                               rowname = NULL,
                                               options= list(dom = "tp",
                                                             pageLength = 5)) %>% DT::formatSignif(c(5,6,7),4)
                                               
      })
  
    output$import_chem_data <- DT::renderDT(chem_view_table(),
                                            server = T)
    DT_Proxy <- dataTableProxy("import_chem_data")
    observeEvent(input$chem_select,{
      if(isTRUE(input$chem_select)){
        DT::selectRows(DT_Proxy,input$import_chem_data_rows_all)
      }else{
        DT::selectRows(DT_Proxy,NULL)
      }
    })

    
  }
  runGadget(ui,server,viewer =dialogViewer("Create user database",width = 1800,height = 2500))
}
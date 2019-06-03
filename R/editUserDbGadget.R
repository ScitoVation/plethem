#' Gadget for creating/editing the a user database
#' @description This function defines the UI and server for creating a new user database. It accepts CSV files from users for chemicals and exposure.
#' @param file_path Path of the database where changes are to be made
#' @export
#' 
editUserDb <- function(file_path){
  ui <- miniPage(
    
    gadgetTitleBar(title = "User Database",left = miniTitleBarCancelButton("cancel"),
                 right = miniTitleBarButton("ok","Save changes")),
    miniTabstripPanel(id = "tabpanel",
                      between = tagList(
                                pickerInput("model_type","Select model type",
                                    choices = list("rapidPBPK"="pbpk",
                                                   "Trout"="trout"),
                                    inline = T,multiple = F,
                                    width = validateCssUnit("100%"))
                      ),
                      miniTabPanel("Chemical",
                                   miniContentPanel(
                                     fillPage(
                                       fillCol(flex = c(1,4),
                                         fillRow(flex = c(6,2),
                                                 # pickerInput(inputId = "chem_datasets",
                                                 #            label = "Select Datasets to remove",
                                                 #            choices = c("a","b","c")
                                                 #            ),
                                                 fileInput("chemfile_upload","Select Chemical File"),
                                                 awesomeRadio("chem_select",inline = T,
                                                              "Select All Rows",choices = c("Yes","No"),
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
      print(chem_save_data)
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
        ret_dat <- read.csv(chemFile()$datapath)
        
      }
      
      
      
      return(ret_dat)
      
    })
    chem_view_table <- reactive({DT::datatable(chem_data(),
                                               caption = NULL,
                                               rowname = NULL,
                                               options= list(dom = "tp",
                                                             pageLength = 5,
                                                             buttons = c("selectAll"))) %>% DT::formatSignif(c(5,6,7),4)
                                               
      })
  
    output$import_chem_data <- DT::renderDT(chem_view_table(),
                                            server = T)
    
  }
  runGadget(ui,server,viewer =dialogViewer("Create user database",width = 1800,height = 1800))
}
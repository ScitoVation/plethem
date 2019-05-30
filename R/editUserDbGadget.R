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
                      miniTabPanel("Chemical",
                                   miniContentPanel(
                                     fillPage(
                                       fillCol(flex = c(1,1),
                                         fillRow(flex = c(1,1),
                                                 pickerInput(inputId = "chem_datasets",
                                                            label = "Select Datasets to remove",
                                                            choices = c("a","b","c")
                                                            ),
                                                 fileInput("chemfile_upload","Select Chemical File")
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
      stopApp(returnValue = "Database updated")
    })
    # The selected chemical file
    chemFile <- reactive({
      input$chemfile_upload
    })
    
    # The user's data, parsed into a data frame
    chem_data <- reactive({
      if(!(is.null(input$chemfile_upload))){
        ret_dat <- read.csv(chemFile()$datapath)
      }else{
        ret_dat <- data.frame()
      }
      
      
      
      return(ret_dat)
      
    })
    output$import_chem_data <- DT::renderDT(DT::datatable(chem_data(),
                                               caption = "Chemical Sets to import",
                                               rowname = NULL,
                                               options= list(dom = "tp",pageLength = 5)),
                                 server = T
    )
  }
  runGadget(ui,server,viewer =dialogViewer("IVIVE",width = 1200,height = 1800))
}
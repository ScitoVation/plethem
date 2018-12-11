#' UI function for importing generic exposure data
#' @description UI function for importing generic exposure data into PLETHEM. The function should not be called by the user
#' @param namespace namespace for the module
#' @export
importBatchExposureUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(
    title= "Import exposure data",
    fileInput(ns("expo_upload"),
              "Select Exposure file",multiple = F,
              accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
              ),
    pickerInput(ns("sel_expo"),multiple = T,label = "Select Exposure",choices = NULL),
    DT::DTOutput(ns("expo_tble"))
    
    # tabsetPanel(id = ns("expo_box"),type = "pills",
    #             tabPanel(title = "Oral",
    #                      value = "oral",
    #                      DT::DTOutput(ns("oral_expo"))),
    #             tabPanel(tile = "Inhalation",
    #                      value = "inh",
    #                      DT::DTOutput(ns("inh_expo"))),
    #             tabPanel(title="Intravenous",
    #                      value ="iv"))


    
    
  ))
}
#'@export
importBatchExposure<- function(input,output,session,expo_name_df){
  expo_file <- reactive({   
    input$expo_upload
  })
  data_file_path <- reactive({
    validate(need(input$expo_upload,"No File Uploaded"))
    return(expo_file()$datapath)
  })
  
  # oral_tble <- reactive({
  #   data <- readxl::read_xlsx(data_file_path(),sheet = "Oral")
  #   return(data)
  # })
  save_val <- reactive({
    parseBatchExposureFile(data_file_path(),expo_name_df)
    temp_df <- data.frame()
    data_oral<- readxl::read_xlsx(data_file_path(),sheet = "Oral")
    data_inh<- readxl::read_xlsx(data_file_path(),sheet = "Inhalation")
    
    return(data$Name)
  })
  
  updatePickerInput(session,"sel_expo",choices = expo_choices())
  #output$oral_expo <- DT::renderDT(oral_tble())
}
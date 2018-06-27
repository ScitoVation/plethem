#' @export
importShedsDataUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(
    title = "Import SHEDS Data",
    selectInput(ns("sel_scene"),"Select Scenario",choices = NULL),
    pickerInput(ns("sel_chem"),"Select Chemical",choices = NULL,multiple = T),
    pickerInput(ns("sel_cohort"),"Select Cohort",
                choices = c("Population"="Total",
                            "Males"="Males",
                            "Females"="Females"),
                multiple = T),
    checkboxGroupButtons(ns("ch_expotype"),"Select Exposures",
                         choices = c("Oral","Inhalation"),#,"Dermal"
                         checkIcon = list(
                           yes = icon("ok", 
                                      lib = "glyphicon"))),
    prettyCheckbox(ns("ch_var"),"Create Variability Sets from Data",
                   fill = T,status = "info",bigger = T),
    footer = tagList(
      actionButton(ns("import"),"Import"),
      modalButton("Dismiss")
    )
    
    
  ))
}

#' @export
importShedsData <- function(input,output,session,path,expo_name_df){
  print(path)
  ns <- session$ns
  returnValues <- reactiveValues()
  returnValues$retdata <- c("No")
  id_name <- "expoid"
  set_table_name <- "ExposureSet"
  vals_table_name <- "Exposure"
  expo_id_num <- getNextID(set_table_name)
  var_id_num <- getNextID("Variability")
  # get all the scenarios run from the output folder
  #Path to output folder
  path2output <- file.path(path,"Output")
  scenario_dirs <- list.dirs(path2output,full.names = F)
  scenario_dirs <- scenario_dirs[scenario_dirs!= ""]
  updateSelectInput(session,"sel_scene",choices = scenario_dirs)
  observeEvent(input$sel_scene,{
    scenario <- input$sel_scene
    chem_list <-list.files(file.path(path2output,scenario))
    chem_options <- gsub(".csv","",gsub("CAS_","",chem_list))
    updatePickerInput(session,"sel_chem",choices = chem_options)
  },
  ignoreInit = T,ignoreNULL = T)
  
  observeEvent(input$import,{
    chem_list <- input$sel_chem
    for (each_chem in chem_list){
      file_name <- paste0("CAS_",each_chem,".csv")
      
    }
  })
  
  
  returnValues$retdata<- eventReactive(input$import,{return(c("No"))})
  
  return(returnValues$retdata)

}
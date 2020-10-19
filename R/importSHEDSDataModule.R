#' UI function for importing SHEDS data
#' @description UI function for importing SHEDS exposures estimates into PLETHEM. The function should not be called by the user
#' @param namespace namespace for the module
#' @export
importShedsDataUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(
    title = "Import SHEDS Data",
    selectInput(ns("sel_scene"),"Select Scenario",choices = NULL),
    pickerInput(ns("sel_chem"),"Select Chemical",choices = NULL,multiple = TRUE,
    pickerInput(ns("sel_cohort"),"Select Cohort",
                choices = c("Population"="Total",
                            "Males"="Males",
                            "Females"="Females"),
                multiple = TRUE,
    checkboxGroupButtons(ns("ch_expotype"),"Select Exposures",
                         choices = c("Oral","Inhalation"),#,"Dermal"
                         checkIcon = list(
                           yes = icon("ok", 
                                      lib = "glyphicon"))),
    prettyCheckbox(ns("ch_var"),"Create Variability Sets from Data",
                   fill = TRUEstatus = "info",bigger = TRUE,
    footer = tagList(
      actionButton(ns("import"),"Import"),
      modalButton("Dismiss")
    )
    
    
  ))
}

#' Server function for importing SHEDS data
#' @description Server function for importing SHEDS exposures estimates into PLETHEM. The function should not be called by the user
#' @param input input object for the UI
#' @param output input object to the UI
#' @param session session object for the module
#' @param path path where SHEDS results are stored
#' @param expo_name_df dataframe containing variable names for exposure values
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
  ignoreInit = TRUEignoreNULL = TRUE
  
  observeEvent(input$import,{
    chem_list <- input$sel_chem
    for (each_chem in chem_list){
      file_name <- paste0("CAS_",each_chem,".csv")
      
    }
  })
  
  
  returnValues$retdata<- eventReactive(input$import,{return(c("No"))})
  
  return(returnValues$retdata)

}
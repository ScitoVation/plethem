#' UI for importing SEEM data.
#' @description This function is called by the pbpk model to import SEEM exposure estimates. Never called by the user
#' @param namespace namespace for the module UI.
#' 
#' @export
importAllExposureDataUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(
    fluidPage(titlePanel("Import Data"),
      tabsetPanel(type = "tabs",
                  
                  ## Batch Exposure Input ##
                  tabPanel("Batch Exposure",
                           shinyWidgets::useSweetAlert(),
                           fileInput("batchExposure", "Choose Batch Exposure File",
                                     multiple = FALSE, accept = NULL, width = NULL),
                           shinyBS::bsCollapse(
                             shinyBS::bsCollapsePanel("Oral Exposure",
                                                      DT::DTOutput(ns("oralDT")) ),
                             shinyBS::bsCollapsePanel("Drinking Water Exposure",
                                                      DT::DTOutput(ns("dwDT")) ),
                             shinyBS::bsCollapsePanel("Inhalation Exposure",
                                                      DT::DTOutput(ns("inhDT")) ),
                             shinyBS::bsCollapsePanel("Intravenous Exposure",
                                                      DT::DTOutput(ns("ivDT")) )
                           ),
                           actionButton(ns("importBatch"), "Import Batch Exposure Data")
                           ),
                           # footer = tagList(modalButton("DismissI"),
                           #                  shinyBS::bsButton(ns("import"),"Import Selected Exposures")),
                           # size = "l"),
                  
                  ## Import SEEM Data ##
                  tabPanel(title = "Seem Data",
                           shinyBS::bsButton(ns("btn_SEEM_data_file"),
                                             "Select SEEM Data File",
                                             block = T),
                           uiOutput(ns("fltr_ui")),
                           actionButton(ns("get_list"),"Get Selected Chemical List"),
                           pickerInput(ns("chems"),"Select Chemicals to Import",choices = c(""),multiple = T),
                           checkboxGroupButtons(ns("data2add"),"Select Estimates to Import",
                                                choices = c("Population Median"="Total_Median",
                                                            "Population Upper 95th Percentile"="Total_Upper95")),
                           actionButton(ns("import_seem"),"Import")),
                  
                  ## Import SHEDS Data ##
                  tabPanel("SHEDS-HT",
                           title = "SHEDS Data",
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
                           )),
                  
                  ## Import TRA Data ##
                  tabPanel("TRA",
                           ## Begin ##
                                           fileInput("expoFile_upload",
                                                     label = "Upload Exposure Excel File",
                                                     multiple = F,
                                                     buttonLabel = "Browse"),
                                           pickerInput("sel_expo",
                                                       label= "Select Exposure",
                                                       width = validateCssUnit("600px"),
                                                       choices = NULL),
                                         fillRow(
                                           DT::DTOutput("expo_table")
                                         ),
                                          pickerInput("sel_export","Select exposures to export",
                                                       choices = NULL,multiple = T)
                           ## End ##
                           )))))}



#' Server function for seem data module
#' @description Server function for import seem data module. This function should not be called by the user
#' @param input input object for the UI
#' @param output input object to the UI
#' @param session session object for the module
#' @param fpath_seems path to the SEEM database
#' @param expo_name_df dataframe containing variable names for exposure values
#' @export
importAllExposureData <- function(input,output,session,expo_name_df){
  ns <- session$ns
  returnValues <- reactiveValues()
  returnValues$retdata <- c("No")
  
  ## Import Batch Data ##

  ## Import SEEMS Data ##
  
  observeEvent(input$btn_SEEM_data_file, ignoreInit = TRUE, {
    #showModal(modalDialog(title = "Button Works!"))
    fpath <- fpath_seem()
    id_name <- "expoid"
    set_table_name <- "ExposureSet"
    vals_table_name <- "Exposure"
    id_num <- getNextID(set_table_name)
    
    query <- "SELECT Category,catid from ChemData;"
    ret_data <- externDbSelect(query,fpath)
    #print(ret_data)
    radio_choices <- setNames(unique(ret_data$catid),
                              unique(ret_data$Category))
    output$fltr_ui <- renderUI({
      radioButtons(ns("seem_filter"),"Select Category",
                   choices = radio_choices)
    }) 
    #updateRadioButtons(session,"seem_filter",choices =choices)
    observeEvent(input$get_list,{
      query <- sprintf("Select CAS,preferred_name from ChemData where catid == '%s';",
                       input$seem_filter)
      path <- fpath
      result <- externDbSelect(query,path)
      result2display <- setNames(result$CAS,result$preferred_name)
      updatePickerInput(session,"chems",choices = result2display)
      # if(!(is.null(input$seem_db))){
      #   print(input$seem_db$datapath)
      # }
    })
    observeEvent(input$import_seem,{
      chem_list <- input$chems
      path <- fpath
      query <- sprintf("Select CAS,preferred_name from ChemData where catid == '%s';",
                       input$seem_filter)
      path <- fpath
      result <- externDbSelect(query,path)
      chem_names_list <- setNames(result$CAS,result$preferred_name)
      chem_cas_list <- setNames(result$preferred_name,result$CAS)
      
      
      for (each_cas in chem_list){
        query<- sprintf("SELECT Total_upper95,Total_Median From Predictions Where Substance_CASRN = '%s';",
                        each_cas)
        predictions <- externDbSelect(query,path)
        chem_name <- chem_cas_list[each_cas]
        for (each_prediction in input$data2add){
          quant_name <- ifelse(each_prediction=="Total_Median",
                               "Median",
                               "Upper 95th Percentile")
          expo_val <- predictions[each_prediction]
          name <- paste(chem_name,"Population",quant_name,sep = " ")
          descrp <-"Imported From SEEM"
          query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                           set_table_name,
                           id_name,
                           id_num,
                           name,
                           descrp)
          projectDbUpdate(query)
          var_names <- expo_name_df$Var
          data2write <- setNames(rep(0,length(var_names)),var_names)
          data2write[grep("flag",names(data2write))]<- "FALSE"
          data2write["bdose"]<- expo_val
          data2write["blen"]<- 1
          data2write["breps"]<- 1
          
          
          #var_names <- names(data2write)
          
          vals <- paste0("'",as.character(data2write),"'")
          
          all_values_string <- paste(paste0(sprintf('(%d,',id_num),
                                            sprintf("'%s'",var_names),
                                            ',',vals,')'),
                                     collapse = ", ")
          write_col_names <- sprintf("%s, param, value",id_name)
          query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                           vals_table_name,
                           write_col_names,
                           all_values_string)
          
          projectDbUpdate(query)
          
          id_num <- id_num+1
          
        }}})}
    )
  
  fpath_seem <- reactive({
    fpath <- tcltk::tk_choose.files(multi = F)
    return(fpath)
  })

  ## Import SHEDS-HT Data ##

  ## Import TRA Data ##
}

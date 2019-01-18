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
                           footer = tagList(modalButton("Dismiss"),
                                            shinyBS::bsButton(ns("import"),"Import Selected Exposures")),
                           size = "l"),
                  
                  ## Import SEEM Data ##
                  tabPanel("SEEM",
                           title = "Seem Data",
                           uiOutput(ns("fltr_ui")),
                           actionButton(ns("get_list"),"Get Selected Chemical List"),
                           pickerInput(ns("chems"),"Select Chemicals to Import",choices = c(""),multiple = T),
                           checkboxGroupButtons(ns("data2add"),"Select Estimates to Import",
                                                choices = c("Population Median"="Total_Median",
                                                            "Population Upper 95th Percentile"="Total_Upper95")),
                           
                           footer = tagList(
                             actionButton(ns("import"),"Import"),
                             modalButton("Dismiss"))),
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
                           )
      )
    )
  ))
}



#' Server function for seem data module
#' @description Server function for import seem data module. This function should not be called by the user
#' @param input input object for the UI
#' @param output input object to the UI
#' @param session session object for the module
#' @param fpath path to the SEEM database
#' @param expo_name_df dataframe containing variable names for exposure values
#' @export
importAllExposureData <- function(input,session){
  ns <- session$ns
  
}
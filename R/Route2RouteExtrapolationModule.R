# This module decribes the route to route extrapolation module that is run from a forward dosimetry interface

#' UI for performing route to route extrapolation
#' @description Module for running route to route extrapolation. This function is never called by the user.
#' @param namespace namespace for the current instance of the module. 
#' @param simid The id of the simulation to use for performing route to route extrapolation
#' @param model String representing the model that is running the simulation
#' @export
#' 
runRoute2RouteUI <- function(namespace, simid, model){
  query <- sprintf("Select expoid,chemid from SimulationsSet where simid = %f;",simid)
  ids_list <- sprintf("Select")
  
  allRoutes <- c('Oral', 'Drinking Water', 'Inhalation', 'IV', 'Dermal')
  
  if(expotype == 'oral'){
    myExpoType = 'Oral'
  } else if(expotype == 'dw'){
    myExpoType = 'Drinking Water'
  } else if(expotype == 'inh'){
    myExpoType = 'Inhalation'
  } else if(expotype == 'iv'){
    myExpoType = 'IV'
  } else if(expotype == 'derm'){
    myExpoType = 'Dermal'
  }
  
  if(is.null(input$sel_sim)){
    exposureChoices <- expoTypes
  } else{
    exposureChoices <- setdiff(expoTypes, myExpoType)
  }
  
  showModal(modalDialog(
    title = "Route to Route Extrapolation",
    size="l",
    tagList(tabsetPanel(id = ns("tab_type"),
                        tabPanel("Route Setting",value = "route_details",
                                 selectInput(ns("sel_newRoute"),"Select Route to Extrapolate To",
                                             choices = )
                                 )
                        )
            )
  ))
}
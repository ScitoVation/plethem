library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(shinybusy)
library(V8)
library(ggplot2)
library(shinyjs)
library(magrittr)
library(plotly)
library(doSNOW)
library(foreach)
#source(system.file("rapidPBPK_pop","rapidPBPK_inits.R",package = "plethem"))
#source(system.file("modules","createSimulationModule.R",package = "plethem"))
#source(system.file("modules","newEditBiomoniteringDataModule.R",package = "plethem"))
# Module for creating simulations in the rapidPBPK_pop PLETHEM model
# This module is only called by the rapidPBPK_pop UI and hence is not a part 
# of the main R/ folder of the package
# Module for uploading biomonitering data
# This module is only called by the rapidPBPK_pop UI and hence is not a part 
# of the main R/ folder of the package




newEditBiomoniteringDataUI <- function(namespace,biomid=NULL){
  if (is.null(biomid)){
    selected_set <- list("chem"=NULL,"tissue"=NULL,"units"=NULL)
    unit_choices = NULL
  }else{
    query <- sprintf("Select chem,tissue,units from Biomonitering Where biomid = %i",
                     biomid)
    selected_set <- projectDbSelect(query)
    if(selected_set$tissue=="pls"){
      unit_choices = list("\u00B5moles/L"="uml","mg/L"="mgl")
    }else{
      unit_choices = list("mg/L"="mgl","\u00B5g/day"="ugd")
    }
  }
  ns <- NS(namespace)
  modalType  <- "Import New Biomonitering Data"
  showModal(modalDialog(title = modalType,
                        useSweetAlert(),
                        fluidPage(
                          fluidRow(
                            column(6,
                                   fileInput(ns("btn_import_file"),
                                             "Upload Biomonitering Data",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv"))
                            )
                          ),
                          fluidRow(
                            textInput(ns("txt_biom_name"),NULL,placeholder = "Name",
                                      width = validateCssUnit("100%"))
                          ),
                          fluidRow(
                            textAreaInput(ns("txt_biom_descrp"),NULL,
                                          placeholder = "Description",
                                          width = validateCssUnit("100%")
                            )
                          ),
                          fluidRow(
                            column(
                              5,
                              shinyWidgets::radioGroupButtons(
                                ns("rdobtn_tissue"),
                                label = "Select Tissue Type",
                                choices = list("Plasma"="pls","Urine"="urine"),
                                selected = selected_set$tissue,
                                status = "info",
                                checkIcon = list("yes"=icon("ok",lib = "glyphicon")),
                                justified = T,
                                width = "100%"
                              )
                            ),
                            column(
                              5,
                              shinyWidgets::radioGroupButtons(
                                ns("rdobtn_chemType"),
                                "Select Chemical Type",
                                choices = list("Parent"="prnt", "Metabolite"="met"),
                                selected = selected_set$chem,
                                status = "info",
                                checkIcon = list("yes"=icon("ok",lib = "glyphicon")),
                                justified = T,
                                width = "100%"
                              )
                            ),
                            column(
                              2,
                              selectizeInput(
                                ns("sel_biomdata_units"),"Data Units",
                                choices = unit_choices,
                                selected = selected_set$units
                              )
                            )
                          )
                        ),
                        footer = tagList(
                          modalButton("Close"),
                          actionButton(ns("ok"),"Save Set")),
                        fade =T,
                        size = "l"
  )
  )
  
}

newEditBiomoniteringData <- function(input,output,session,type = "new",biomid = NULL){
  returnValues <- reactiveValues()
  returnValues$savedat <- c("No","",0)
  ns <- session$ns
  
  
  if(type == "edit"){
    query <- sprintf("Select name, descrp from BiomoniteringSet where biomid = %i",
                     biomid)
    set_meta <- projectDbSelect(query)
    updateTextInput(session,"txt_biom_name",value = set_meta$name)
    updateTextAreaInput(session,"txt_biom_descrp",value = set_meta$descrp)
    # query <- sprintf("Select tissue,units from Biomonitering Where biomid = %i",
    #                  biomid)
    # selected_set <- projectDbSelect(query)
    # if(selected_set$tissue=="pls"){
    #   unit_choices = list("\u00B5moles/L"="uml","ng/L"="ngl","mg/L"="mgl")
    # }else{
    #   unit_choices = list("ng/L"="ngl","mg/L"="mgl","\u00B5g/day"="ugd")
    # }
    # updateSelectizeInput(session,"sel_biomdata_units",choices = unit_choices,
    #                      selected =selected_set$units)
  }
  observeEvent(input$rdobtn_tissue,{
    tissue <- input$rdobtn_tissue
    if(tissue=="pls"){
      choices = list("\u00B5moles/L"="uml","ng/L"="ngl","mg/L"="mgl")
    }else{
      choices = list("ng/L"="ngl","mg/L"="mgl","\u00B5g/day"="ugd")
    }
    updateSelectizeInput(session,"sel_biomdata_units",choices = choices)
  },ignoreInit = T,ignoreNULL = T)
  returnValues$savedat<- eventReactive(input$ok,{
    name <- input$txt_biom_name
    descrp <- input$txt_biom_descrp
    chem <- input$rdobtn_chemType
    tissue <- input$rdobtn_tissue
    units <- input$sel_biomdata_units
    if(type == "new" && is.null(input$btn_import_file)){
      sendSweetAlert(session,"Error",
                     "No Biomonitering Data Uploaded",
                     type = "error")
      return(c("No","",0))
      
    }
    else if(name == "" || descrp== ""){
      sendSweetAlert(session,"Error",
                     "Please enter name and description",
                     type = "error")
      return(c("No","",0))
    }
    else{
      
      #write the data
      if(type == "new"){
        biom_fpath <- input$btn_import_file$datapath
        biom_data <- as.data.frame(read.csv(biom_fpath))
        serialized_biom_data <- rawToChar(serialize(biom_data,NULL,T))
        biomid <- as.integer(getNextID("BiomoniteringSet"))
        query <- sprintf("Insert Into BiomoniteringSet (biomid,name,descrp) VALUES (%i,'%s','%s');",
                         biomid,name,descrp)
        projectDbUpdate(query)
        query <- sprintf("Insert Into Biomonitering (biomid,chem,tissue,units,data) VALUES (%i,'%s','%s','%s','%s');",
                         biomid,chem,tissue,units,serialized_biom_data)
        projectDbUpdate(query)
        sendSweetAlert(session,title = "Saved","New biomonitering data added",type ="info")
      }else{
        # if the file import is null then new data should not be written
        if(is.null(input$btn_import_file$datapath)){
          query <- sprintf("Update Biomonitering Set chem = '%s',tissue='%s', units = '%s' where biomid = %i;",
                           chem,
                           tissue,
                           units,
                           biomid)
          projectDbUpdate(query)
        }else{
          biom_fpath <- input$btn_import_file$datapath
          biom_data <- as.data.frame(read.csv(biom_fpath))
          serialized_biom_data <- rawToChar(serialize(biom_data,NULL,T))
          query <- sprintf("Update Biomonitering Set chem = '%s',tissue='%s', units = '%s', data = '%s' where biomid = %i;",
                           chem,
                           tissue,
                           units,
                           serialized_biom_data,
                           biomid)
          projectDbUpdate(query)
        }
        
      }
      removeModal()
      return(c("Yes","biom",biomid))
    }
  })
  return(returnValues$savedat)
}



createSimulationUI <- function(namespace,set_list,selected_list){
  ns <- NS(namespace)
  showModal(modalDialog(title = "Create Simulation",
                        fluidPage(
                          
                          fluidRow(
                            textInput(ns("txt_sim_name"),NULL,placeholder = "Simulation Name",
                                      width = validateCssUnit("100%"))
                          ),
                          fluidRow(
                            textAreaInput(ns("txt_sim_descrp"),NULL,
                                          placeholder = "Simulation Description",
                                          width = validateCssUnit("100%")
                            )
                          ),
                          fluidRow(
                            selectizeInput(ns("sel_sim_type"),"Simulation Type",
                                           choices = list("Forward Dosimetry"="fd",
                                                          "Forward Dosimetry with Monte Carlo"="mc",
                                                          "Reverse Dosimetry"="rd",
                                                          "Route to Route Exptrapolation"="r2r")
                            )
                          ),
                          tabsetPanel(id = ns("tab_sim_sets"),type = "pills",
                                      tabPanel("Parameters",value = "param",
                                               fluidPage(
                                                 fluidRow(
                                                   tags$br()
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          selectizeInput(ns("sel_sim_expo"),
                                                                         "Exposure",
                                                                         choices = set_list$expo,
                                                                         selected= selected_list$expo,
                                                                         width = validateCssUnit("100%"),
                                                                         options = list(placeholder = "Exposure"))
                                                   ),
                                                   column(6,
                                                          selectizeInput(ns("sel_sim_chems"),
                                                                         "Parent Chemical",
                                                                         choices = set_list$chem,
                                                                         selected= selected_list$chem,
                                                                         width = validateCssUnit("100%"),
                                                                         options = list(placeholder = "Parent Chemical"))
                                                   )),
                                                 fluidRow(
                                                   column(6,
                                                          selectizeInput(ns("sel_sim_physio"),
                                                                         "Physiology",
                                                                         choices = set_list$physio,
                                                                         selected= selected_list$physio,
                                                                         width = validateCssUnit("100%"),
                                                                         options = list(placeholder = "Physiology"))
                                                   ),
                                                   column(6,
                                                          selectizeInput(ns("sel_sim_adme"),
                                                                         "ADME",
                                                                         choices = NULL,
                                                                         width = validateCssUnit("100%"),
                                                                         options = list(placeholder = "ADME")
                                                          )
                                                   )
                                                 )
                                               )
                                      ),
                                      tabPanel("Variability",value = "variability",
                                               fluidPage(
                                                 fluidRow(
                                                   tags$br()
                                                 ),
                                                 fluidRow(
                                                   conditionalPanel(
                                                     condition = "input.sel_sim_type == 'mc'",
                                                     ns = ns,
                                                     column(6,
                                                            selectizeInput(ns("sel_sim_expovar"),
                                                                           "Exposure",
                                                                           choices = set_list$expovar,
                                                                           selected = selected_list$expovar,
                                                                           width = validateCssUnit("100%"),
                                                                           options= list(placeholder = "No Variability Set Found"))
                                                     )
                                                   ),
                                                   column(6,
                                                          selectizeInput(ns("sel_sim_chemvar"),
                                                                         "Parent Chemical",
                                                                         choices = set_list$chemvar,
                                                                         selected = selected_list$chemvar,
                                                                         width = validateCssUnit("100%"),
                                                                         options= list(placeholder = "No Variability Set Found"))
                                                   )
                                                   
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          selectizeInput(ns("sel_sim_physiovar"),
                                                                         "Physiology",
                                                                         choices =  set_list$physiovar,
                                                                         selected = selected_list$physiovar,
                                                                         width = validateCssUnit("100%"),
                                                                         options= list(placeholder = "No Variability Set Found"))
                                                   ),
                                                   column(6,
                                                          selectizeInput(ns("sel_sim_admevar"),
                                                                         "ADME",
                                                                         choices =  set_list$admevar,
                                                                         selected = selected_list$admevar,
                                                                         width = validateCssUnit("100%"),
                                                                         options= list(placeholder = "No Variability Set Found")))
                                                 )
                                               )
                                      ),
                                      tabPanel("Workflow Specific Inputs",value = "workflow",
                                               fluidPage(
                                                 fluidRow(
                                                   tags$br()
                                                 ),
                                                 fluidRow(
                                                   conditionalPanel(
                                                     condition = "input.sel_sim_type == 'rd'",
                                                     ns = ns,
                                                     column(6,offset = 3,
                                                            selectizeInput(ns("sel_biomdata"),
                                                                           NULL,
                                                                           choices =  set_list$biom,
                                                                           selected = selected_list$biom,
                                                                           width = validateCssUnit("100%"),
                                                                           options = list(placeholder = "Biomonitoring dataset")
                                                            )
                                                     )
                                                   )
                                                 ),
                                                 fluidRow(
                                                   conditionalPanel(
                                                     condition = "input.sel_sim_type == 'r2r'",
                                                     ns = ns,
                                                     column(6,offset = 3,
                                                            selectizeInput(ns("sel_r2rExpo"),"Exposure Set to be used as a template",
                                                                           choices =  set_list$extrapolate,
                                                                           selected = selected_list$extrapolate,
                                                                           width = validateCssUnit("100%")
                                                            )
                                                     )
                                                     
                                                   )
                                                 ),
                                                 fluidRow(
                                                   column(6,offset=1,
                                                          numericRangeInput(ns("numrange_expo"),
                                                                            "Exposure range",
                                                                            value = c(0.01,1),
                                                                            separator = "to")
                                                   ),
                                                   column(3,offset = 1,
                                                          numericInput(ns("num_numexpos"),
                                                                       "Number of Exposures",
                                                                       value = 25)
                                                   )
                                                 )
                                               )
                                      ),
                                      tabPanel("Simulation",value = "sim",
                                               fluidPage(
                                                 fluidRow(
                                                   tags$br()
                                                 ),
                                                 fluidRow(
                                                   column(3,
                                                          numericInputIcon(ns("num_tstart"),
                                                                           "Simulation Start Time",
                                                                           value = 0,icon = list(NULL,"hours"))
                                                   ),
                                                   column(3,
                                                          numericInput(ns("num_sim_dur"),"Simulation Duration",
                                                                       value = 1
                                                          )
                                                   ),
                                                   column(2,
                                                          shiny::selectizeInput(ns("sel_dur_units"),"Duration Units",
                                                                                choices = list("Hours"="h",
                                                                                               "Days"="d",
                                                                                               "Weeks"="w"),
                                                                                selected = "h",multiple = F
                                                          )
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.sel_sim_type != 'fd'",
                                                     ns = ns,
                                                     column(4,
                                                            numericInput(ns("num_mcruns"),"Number of Montecarlo Runs",
                                                                         value = 1000)
                                                     )
                                                     
                                                   )
                                                 )
                                               )
                                      )
                          )
                          
                        ),
                        footer = tagList(modalButton("Cancel"),
                                         actionButton(ns("btn_create_sim"),
                                                      "Create Simulation")
                        ),
                        size = "l",
                        fade = T))
}
createSimulation <- function(input,output,session,type="new",sim_settings){
  returnValues <- reactiveValues()
  returnValues$savedat <- c("No","",0)
  ns <- session$ns
  
  # if a new simulation needs to be created
  if (type == "new"){
    simid <- getNextID("SimulationsSet")
  }
  # if editing an existing simulation
  else{
    simid <- sim_settings$simid
    updateTextInput(session,"txt_sim_name",value = sim_settings$name)
    updateTextAreaInput(session,"txt_sim_descrp",value = sim_settings$descrp)
    updateSelectizeInput(session,"sel_sim_type",selected = sim_settings$sim_type)
    updateSelectizeInput(session,"sel_dur_units",selected = sim_settings$dur_units)
    updateNumericInputIcon(session,"num_tstrart",value = sim_settings$tstart)
    updateNumericInput(session,"num_sim_dur",value = sim_settings$sim_dur)
    # set the rest if they are present in the 
    if(sim_settings$sim_type != "fd"){
      updateNumericInput(session,"num_mcruns",value = sim_settings$mcruns)
    }
    if(sim_settings$sim_type %in% c("rd","r2r")){
      updateNumericRangeInput(session,"numrange_expo",value = sim_settings$expo_range)
      updateNumericInput(session,"num_numexpos",value = sim_settings$num_expos)
    }
  }
  # only show input tabs relavent to selected workflows
  observeEvent(input$sel_sim_type,{
    if (input$sel_sim_type %in% c("rd","r2r") ){
      showTab("tab_sim_sets","workflow",session = session)
    }else{
      hideTab("tab_sim_sets","workflow",session = session)
    }
    if(input$sel_sim_type == 'fd'){
      hideTab("tab_sim_sets","variability",session = session)
    }else{
      showTab("tab_sim_sets","variability",session = session)
    }
  })
  #update the adme selection based on selected chemical,physiological and exposure set
  observeEvent({
    input$sel_sim_chems
    input$sel_sim_physio
    input$sel_sim_expo
  },{
    chemid <- as.integer(input$sel_sim_chems)
    physioid <- as.integer(input$sel_sim_physio)
    expoid <- as.integer(input$sel_sim_expo)
    if(!any((is.na(c(chemid,physioid,expoid))))){
      query <- sprintf("Select name,admeid from AdmeSet where chemid = %d AND physioid = %d AND expoid = %d;",
                       chemid, physioid, expoid)
      res <- projectDbSelect(query)
      set_list <- as.list(res[["admeid"]])
      names(set_list)<- res$name
      
      if(length(set_list)>0){
        updateSelectizeInput(session,"sel_sim_adme",choices = set_list)
      }else{
        updateSelectizeInput(session,"sel_sim_adme",choices = set_list,
                             options = list(placeholder = "No appropriate ADME set found"))
      }
      
    }
    
  },ignoreNULL = T, ignoreInit = T)
  
  #update the exposure extrapolation dropdown to exclude exposure set selected in the
  observeEvent(input$sel_sim_expo,{
    expo_sets <- getAllSetChoices("expo")
    selected_expo <- input$sel_sim_expo
    remaining_list <- expo_sets[which(expo_sets != as.integer(selected_expo))]
    updateSelectizeInput(session,"sel_r2rExpo",choices = remaining_list)
  })
  
  returnValues$savedat <- eventReactive(input$btn_create_sim,{
    #simid <- getNextId("SimulationSet")
    
    
    # if a new simulation needs to be created, create an blank simulation set to update the data into
    if(type == "new"){
      query <- sprintf("INSERT INTO SimulationsSet (simid) VALUES (%i);",simid)
      projectDbUpdate(query)
    }
    #update the simulation set with inputs common to all workflow types
    sim_type <- input$sel_sim_type
    sim_name <- input$txt_sim_name
    sim_descrp <- input$txt_sim_descrp
    expoid <- as.integer(input$sel_sim_expo)
    chemid <- as.integer(input$sel_sim_chems)
    physioid <- as.integer(input$sel_sim_physio)
    admeid <- as.integer(input$sel_sim_adme)
    tstart <- input$num_tstart
    sim_dur <- input$num_sim_dur
    dur_units <- input$sel_dur_units
    query <- paste(strwrap(sprintf("Update SimulationsSet SET
                                    name = '%s',
                                    descrp = '%s',
                                    sim_type = '%s',
                                    expoid = %i,
                                    chemid = %i, 
                                    physioid = %i,
                                    admeid = %i,
                                    tstart = %f,
                                    sim_dur = %f,
                                    dur_units = '%s' where simid = %i;",
                                   sim_name,
                                   sim_descrp,
                                   sim_type,
                                   expoid,
                                   chemid,
                                   physioid,
                                   admeid,
                                   tstart,
                                   sim_dur,
                                   dur_units,
                                   simid),
                           simplify = T),
                   sep = " ",collapse = "")
    projectDbUpdate(query)
    # update the simulation with inputs needed by montecarlo workflow
    if(sim_type == 'mc'){
      expovarid <- as.integer(input$sel_sim_expovar)
      chemvarid <- as.integer(input$sel_sim_chemvar)
      physiovarid <- as.integer(input$sel_sim_physiovar)
      admevarid <- as.integer(input$sel_sim_admevar)
      mcruns <- input$num_mcruns
      query <- paste(strwrap(sprintf("Update SimulationsSet SET
                                     expovarid = %i,
                                     chemvarid = %i,
                                     physiovarid = %i,
                                     admevarid = %i,
                                     mcruns = %i where simid = %i;",
                                     ifelse(is.na(expovarid),0,expovarid),
                                     ifelse(is.na(chemvarid),0,chemvarid),
                                     ifelse(is.na(physiovarid),0,physiovarid),
                                     ifelse(is.na(admevarid),0,admevarid),
                                     mcruns,
                                     simid),
                             simplify = T),
                     sep=" ",collapse = "")
      projectDbUpdate(query)
      
    }
    # update the simulation with inputs needed by the reverse dosimetry
    else if(sim_type == 'rd'){
      chemvarid <- as.integer(input$sel_sim_chemvar)
      physiovarid <- as.integer(input$sel_sim_physiovar)
      admevarid <- as.integer(input$sel_sim_admevar)
      mcruns <- as.integer(input$num_mcruns)
      biomid <- as.integer(input$sel_biomdata)
      num_expos <- as.integer(input$num_numexpos)
      low_dose_estimate <- input$numrange_expo[1]
      high_dose_estimate <- input$numrange_expo[2]
      query <-sprintf("Update SimulationsSet SET chemvarid = %i,physiovarid = %i,admevarid = %i,biomid = %i,mcruns = %i,num_expos = %i, low_dose_estimate = %f, high_dose_estimate = %f, expovarid = 0 where simid = %i;",
                      ifelse(is.na(chemvarid),0,chemvarid),
                      ifelse(is.na(physiovarid),0,physiovarid),
                      ifelse(is.na(admevarid),0,admevarid),
                      biomid,# change to biomoniternig ID once implemented
                      mcruns,
                      num_expos,
                      low_dose_estimate,
                      high_dose_estimate,
                      simid)
      projectDbUpdate(query)
    }
    # update the simulation with inputs needed by route to route dosimetry
    else if(sim_type == "r2r"){
      chemvarid <- as.integer(input$sel_sim_chemvar)
      physiovarid <- as.integer(input$sel_sim_physiovar)
      admevarid <- as.integer(input$sel_sim_admevar)
      mcruns <- input$num_mcruns
      extrapolateid <- as.integer(input$sel_r2rExpo)
      num_expos <- as.integer(input$num_numexpos)
      low_dose_estimate <- input$numrange_expo[1]
      high_dose_estimate <- input$numrange_expo[2]
      query <- sprintf("Update SimulationsSet SET chemvarid = %i, physiovarid = %i, admevarid = %i, extrapolateid = %i, mcruns = %i, num_expos = %i,low_dose_estimate = %f,high_dose_estimate = %f where simid = %i;",
                       ifelse(is.na(chemvarid),0,chemvarid),
                       ifelse(is.na(physiovarid),0,physiovarid),#,
                       ifelse(is.na(admevarid),0,admevarid),
                       extrapolateid,
                       mcruns,
                       num_expos,
                       low_dose_estimate,
                       high_dose_estimate,
                       simid)
      projectDbUpdate(query)
      
    }
    removeModal()
    return(c("Yes","sim",simid))
  })
  return(returnValues$savedat)
}
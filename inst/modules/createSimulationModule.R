# Module for creating simulations in the rapidPBPK_pop PLETHEM model
# This module is only called by the rapidPBPK_pop UI and hence is not a part 
# of the main R/ folder of the package
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
      query <- sprintf("Update SimulationsSet SET chemvarid = %i, physiovarid = %i, admevarid = %i, extrapolateid = %i, mcruns = %i, num_expos = %i,low_dose_estimate = %f,high_dose_estimate = %f, expovarid = 0 where simid = %i;",
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
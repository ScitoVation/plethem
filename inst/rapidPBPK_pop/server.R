library(shinyFiles)
shinyServer(function(input, output, session) {
  # Type of environment in which the shiny app is called
  run_type <- "prod" #"prod" for production, "dev" for development
  show_modal_spinner("orbit")
  shinyjs::useShinyjs()
  hideTab("menu","output")
  #check if a project is loaded
  name <- projectDbSelect("Select name from Project")$name
  if (length(name)>0){
    showTab("menu","setup")
  }else{
    hideTab("menu","setup")
  }
  # define the model name once here. It will be used throughout this server file
  # this will make it easier to create new model UI/SERVERS
  model <- "rapidPBPK"
  # this dataframe is only used to display the metabolism data.
  #The actual model uses values stored in the database
  metabolism_dataframe <- data.frame("Age"=c(25),"Clearance"=c(0),stringsAsFactors = F)

  dataset <- reactiveValues()
  dataset$savedat <- reactiveVal(c("No","none"))
  dataset$iviveDat <- reactiveVal(c("No",0,0,0))
  parameterSets <- reactiveValues()
  
  parameterSets$savedat <- reactiveVal(c("No","",0))
  parameterSets$sverestdat <- reactiveVal(c("None",0))
  parameterSets$importdat <- reactiveVal(c("No","",0))
  parameterSets$importSeem <- reactiveVal(c("No"))
  parameterSets$importSheds <- reactiveVal(c("No"))
  parameterSets$importBatch <- reactiveVal(c("No"))
  parameterSets$importAllData <- reactiveVal(c("No"))
  parameterSets$sim_table <- data.frame("Col1"="","Col2"=0,"Col3"=0,row.names = NULL)
  parameterSets$vardat <- reactiveVal(c("None","",0))
  expo_set <- getAllSetChoices("expo")
  physio_set <- getAllSetChoices("physio")
  chem_set <- getAllSetChoices("chem")
  adme_set <- getAllSetChoices("adme")
  biom_set <- getAllSetChoices("biom")
  #metab_set <- getAllSetChoices("metab")
  sim_set <- getAllSetChoices("sim")
  physiovar <-getVariabilitySetChoices("physio")
  chemvar <-getVariabilitySetChoices("chem")
  expovar <-getVariabilitySetChoices("expo")
  admevar <-getVariabilitySetChoices("adme")
  parameterSets$expo <- reactiveVal(expo_set)
  parameterSets$physio <- reactiveVal(physio_set)
  parameterSets$chem <- reactiveVal(chem_set)
  parameterSets$sim <- reactiveVal(sim_set)
  parameterSets$biom <- reactiveVal(biom_set)
  parameterSets$physiovar <- reactiveVal(physiovar)
  parameterSets$chemvar <- reactiveVal(chemvar)
  parameterSets$expovar <- reactiveVal(expovar)
  parameterSets$admevar <- reactiveVal(admevar)


  observe({
    exposet <- parameterSets$expo()
    updateSelectizeInput(session,"sel_set_expo",choices = exposet)
    updateSelectizeInput(session,"sel_expo4adme",choices = exposet)
    physioset <- parameterSets$physio()
    updateSelectizeInput(session,"sel_set_physio",choices = physioset)
    updateSelectizeInput(session,"sel_physio4adme",choices = physioset)
    chemset <- parameterSets$chem()
    updateSelectizeInput(session,"sel_set_chem",choices = chemset)
    updateSelectizeInput(session,"sel_chem4adme",choices = chemset)
    updateSelectizeInput(session,"sel_metabolite4adme",choices = c("No Metabolite"=0,chemset))

    physiovar <- parameterSets$physiovar()
    physiovar <- c("None"="0",physiovar)
    updateSelectizeInput(session,"sel_set_physiovar",choices = physiovar)
    chemvar <- parameterSets$chemvar()
    chemvar <- c("None"="0",chemvar)
    updateSelectizeInput(session,"sel_set_chemvar",choices = chemvar)
    expovar <- parameterSets$expovar()
    expovar <- c("None"="0",expovar)
    updateSelectizeInput(session,"sel_set_expovar",choices = expovar)
    admevar <- parameterSets$admevar()
    admevar <- c("None"="0",admevar)
    updateSelectizeInput(session,"sel_set_admevar",choices = admevar)
  })
  observeEvent({
    input$sel_set_chem
    input$sel_set_physio
    input$sel_set_expo
  },{
    chemid <- as.integer(input$sel_set_chem)
    physioid <- as.integer(input$sel_set_physio)
    expoid <- as.integer(input$sel_set_expo)
  if(!any((is.na(c(chemid,physioid,expoid))))){
    query <- sprintf("Select name,admeid from AdmeSet where chemid = %d AND physioid = %d AND expoid = %d;",
                                        chemid, physioid, expoid)
    res <- projectDbSelect(query)
    set_list <- as.list(res[["admeid"]])
    names(set_list)<- res$name
    updateSelectizeInput(session,"sel_set_adme",choices = set_list)
  }

  },ignoreNULL = T, ignoreInit = T)
 
  # get global variables needed to run the model


  # get the parameter table for physiological,exposure, chemical and adme variables.
  query <- sprintf("SELECT Name,Var,Units,ParamType,Variability FROM ParamNames Where Model='%s' AND ParamSet = 'Physiological' AND UIParams = 'TRUE';",
                   model)
  physio_name_df <- mainDbSelect(query)

  query <- sprintf("SELECT Name,Var,Units,ParamType,Variability FROM ParamNames Where Model='%s' AND ParamSet = 'Exposure' AND UIParams = 'TRUE';",
                   model)
  expo_name_df <- mainDbSelect(query)

  query <- sprintf("SELECT Name,Var,Units,ParamType,Variability FROM ParamNames Where Model='%s' AND ParamSet = 'Chemical'AND UIParams = 'TRUE' ;",
                   model)
  chem_name_df <- mainDbSelect(query)
  
  query <- sprintf("SELECT Name,Var,Units,ParamType,Variability FROM ParamNames Where Model='%s' AND ParamSet = 'Adme' AND UIParams = 'TRUE' ;",
                   model)
  adme_name_df <- mainDbSelect(query)

  #### Update the parameter set dropdowns if they exist for physiological and exposure sets
  set_choices <- getAllSetChoices(set_type = "physio")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_physio",choices = set_choices)
    shinyBS::updateButton(session,"btn_use_lifecourse",style = "primary")
    updateSelectizeInput(session,"sel_physio4adme",choices = set_choices)
    # shinyBS::updateButton(session,"btn_useQSAR4Partition",style = "primary")
  }
  set_choices <- getAllSetChoices(set_type = "expo")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_expo",choices = set_choices)
    updateSelectizeInput(session,"sel_expo4adme",choices = set_choices)
  }
  set_choices <- getAllSetChoices(set_type = "chem")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_chem",choices = set_choices)
    updateSelectizeInput(session,"sel_chem4adme",choices = set_choices)
    updateSelectizeInput(session,"sel_metabolite4adme",choices = set_choices)
  }
  set_choices <- getAllSetChoices(set_type = "adme")
  if (length(set_choices>0)){
    updateSelectizeInput(session,"sel_adme",choices = set_choices)
  }
  set_choices <- getAllSetChoices(set_type = "metab")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_metabfiles",choices = set_choices)
  }
  set_choices <- getAllSetChoices(set_type = "sim")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_sim",choices = set_choices)
  }
  obs_conc_set <- getObservationSetChoices("conc")
  if (length(obs_conc_set)>0){
    updatePickerInput(session,"cplt_data",
                      choices = c("No Dataset"="none",obs_conc_set),
                      selected = "none")
  }
  set_choices<- getVariabilitySetChoices("physio")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_physio_var",
                      choices = set_choices)
  }
  set_choices<- getVariabilitySetChoices("chem")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_chem_var",
                         choices = set_choices)
  }
  set_choices<- getVariabilitySetChoices("expo")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_expo_var",
                         choices = set_choices)
  }
  set_choices<- getVariabilitySetChoices("adme")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_adme_var",
                         choices = set_choices)
  }
  if(length(biom_set)>0){
    updateSelectizeInput(session,"sel_biom",choices = biom_set)
  }
  



  ########### The next chunck enables lumping compartments.
  compartment_list <-c("skin","fat","muscle","bone","brain","lung","heart","gi","liver","kidney","rpf","spf")
  vol_ids <- c("fat"="ms_vfatc","skin"="ms_vskinc",
               "muscle"="ms_vmuscc","bone"="ms_vbonec",
               "brain"="ms_vbrnc","lung"="ms_vlngc",
               "heart"="ms_vhrtc","gi"="ms_vgic",
               "liver"="ms_vlivc","kidney"="ms_vkdnc",
               "rpf"="ms_vrpfc","spf"="ms_vspfc","blood"="ms_vbldc",
               "bw"="ms_bw")
  flow_ids <- c("fat"="ms_qfatc","skin"="ms_qskinc",
                "muscle"="ms_qmuscc","bone"="ms_qbonec",
                "brain"="ms_qbrnc","lung"="ms_qlngc",
                "heart"="ms_qhrtc","gi"="ms_qgic","kidney"="ms_qkdnc",
                "rpf"="ms_qrpfc","spf"="ms_qspfc")

  observe({
    selected_list<- as.vector(input$ms_cmplist)
    inactive_list <- base::setdiff(compartment_list,selected_list)
    # set volumes of inactive compartments to 1e-12 ( very low)
    # set flows of inactive compartments to zero
    for(x in inactive_list){
      input_id <- as.character(flow_ids[x])
      updateNumericInput(session,input_id,value =0)
      input_id <- as.character(vol_ids[x])
      updateNumericInput(session,input_id,value = 1e-12)

    }
    # disable the tab for inactive compartments
    sapply(compartment_list,function(x){js$enableTab(x)})
    sapply(inactive_list,function(x){js$disableTab(x)})
  })
  ############ End chuck for handling lumping compartments

  ########### The next code chunk deals with updating select inputs for all parameter sets]
  # Import SEEM, SHEDS-HT, batch exposure, and TRA data
  observeEvent(input$btn_import_expo,{
    importAllExposureDataUI(paste0("allData",input$btn_import_expo))
    parameterSets$importAllData <- callModule(importAllExposureData,
                                              paste0("allData",input$btn_import_expo),
                                              expo_name_df)
  })
  
  observe({
    result_vector <- parameterSets$importAllData
    if(result_vector()[1]=="Yes"){
      set_type <- "expo"
      set_list <- getAllSetChoices(set_type)
      parameterSets[[set_type]] <- reactiveVal(set_list)
      updateSelectizeInput(session,paste0("sel_",set_type),
                           choices = set_list)
      updateSelectizeInput(session,"sel_expo4adme",
                           choices = set_list)
    }
  })
  
  
  ### Import button handlers
   # Chunk for handling chemical tab
   observeEvent(input$btn_import_chem,{
     
     importParameterSetUI(paste0("chem",input$btn_import_chem),"chem")
     parameterSets$savedat <- callModule(importParameterSet,paste0("chem",input$btn_import_chem),"chem")

   })
   # Chunk for handling physiological tab
   observeEvent(input$btn_import_physio,{
     importParameterSetUI(input$btn_import_physio,"physio")
     parameterSets$savedat <- callModule(importParameterSet,input$btn_import_physio,"physio")
     
   })
  ### SAVE AS BUTTON HANDLERS
   
  #Save a new physiological parameter set
  observeEvent(input$btn_saveas_physio,{
    active_comp <- input$ms_cmplist
    compartment_list <-c("skin","fat","muscle","bone","brain","lung","heart","gi","liver","kidney","rpf","spf")
    inactive_comp <- setdiff(compartment_list,active_comp)
    vol_comps <- c(active_comp,"blood")
    perfc <- input$ms_perfc
    total_vol <- sum(unlist(lapply(vol_comps,function(x){input[[vol_ids[x]]]})))
    # QC checks for the physiological set
    # ensure liver is an active compartment if gi is an active compartment
    if ("gi" %in% active_comp && !("liver" %in% active_comp)){
      shinyWidgets::sendSweetAlert(session,
                                   title = "Invalid Compartment Configuration",
                                   text = "Liver compartment needs to be active if GI compartment is active",
                                   type = "error")

    }
    # Ensure at least one compartment in the model is active
    else if (length(active_comp) == 0){
      shinyWidgets::sendSweetAlert(session,
                                   title = "Invalid Compartment Configuration",
                                   text = "At least one compartment needs to be active for the model to run",
                                   type = "error")

    }
    # the volume of active compartments and the volume of perfused tissue do not match
    else if(abs(total_vol-perfc)>0.03){

      error_text <- sprintf("The total volume of all compartments does not add up to %i %%",
              as.integer(perfc*100))

      shinyWidgets::sendSweetAlert(session,
                                   title = "Invalid Compartment Configuration",
                                   text = error_text,
                                   type = "error")

    }
    # # GI is an active compartment if oral dosing is selected
    # else if((input$ms_bdose>0 || input$ms_drdose>0) && !("gi" %in% active_comp)){
    #   showModal(
    #     modalDialog(
    #       tags$h4("Invalid Compartment Configuration"),
    #       tags$h5("GI compartment must be active for Oral and Drinking water routes of exposure"),
    #       title = "Error"
    #     )
    #   )
    # }
    else{
      ns <- paste0("physio",input$btn_saveas_physio)
      saveAsParameterSetUI(ns,"physio")
      parameterSets$savedat <- callModule(saveAsParameterSet,
                                          ns,
                                          "physio",isolate(input),
                                          physio_name_df)
    }

  })

  #Save a new exposure parameter set
  observeEvent(input$btn_saveas_expo,{
    expos_list <- c(input$ms_bdose,input$ms_drdose,input$ms_bdosev,
                    input$ms_inhdose,input$ms_ivdose,input$ms_dermrate)
    
    # make sure atleast one route of exposure is active before saving the data
    if((input$ms_bdose==0 || input$ms_breps == 0) && input$ms_drdose==0 && (input$ms_bdosev==0 || input$ms_brepsv == 0)&& input$ms_inhdose==0 && input$ms_ivdose==0 && input$ms_dermrate == 0){
      shinyWidgets::sendSweetAlert(session,
                                   title = "Invalid Exposure Parameters",
                                   text = "Atleast one route of exposure should be active",
                                   type = "error")

    }else if(length(expos_list[expos_list>0])>1){
      shinyWidgets::sendSweetAlert(session,
                                   title = "Invalid Exposure Parameters",
                                   text = "More than one route of exposure is active",
                                   type = "error")
      
    }else{
      ns <- paste0("expo",input$btn_saveas_expo)
      saveAsParameterSetUI(ns,"expo")
      parameterSets$savedat <- callModule(saveAsParameterSet,
                                          ns,
                                          "expo",isolate(input),
                                          expo_name_df)
    }

  })

  #Save a new chemical parameter set
  observeEvent(input$btn_saveas_chem,{
    ns <- paste0("chem",input$btn_saveas_chem)
    saveAsParameterSetUI(ns,"chem")
    parameterSets$savedat <- callModule(saveAsParameterSet,ns,
                                        "chem",isolate(input),
                                        chem_name_df)
  })
  
  #Save a new chemical parameter set
  observeEvent(input$btn_saveas_adme,{
    chemid <- as.integer(input$sel_chem4adme)
    physioid <- as.integer(input$sel_physio4adme)
    expoid <- as.integer(input$sel_expo4adme)
    metabid <- as.integer(input$sel_metabolite4adme)
    id_list <- c(expoid,chemid,physioid,metabid)
    # select chemical, exposure and physiology that the given adme set relates to
    if (any(is.na(id_list))){
      sendSweetAlert(session,"Configuration Error",
                     "Need to define Exposure, chemical and Physiology sets before defining an ADME set",
                     type = "error")
    }else{
      ns <- paste0("adme",input$btn_saveas_adme)
      saveAsParameterSetUI(ns,"adme")
      parameterSets$savedat <- callModule(saveAsParameterSet,ns,
                                          "adme",isolate(input),
                                          adme_name_df,id_list)
    }
    
  })
  
  ## CODE CHUNK FOR HANDLING BIOMONITORING DATA UPLOAD
  observeEvent(input$btn_new_biom,{
    namespace <- paste0("biom",input$btn_new_biom)
    newEditBiomoniteringDataUI(namespace)
    parameterSets$savedat <- callModule(newEditBiomoniteringData,namespace,
                                        type= "new")
  })
  
  observeEvent(input$btn_edit_biom,{
    if(input$sel_biom == ""){
      ## Error bubble no data yet loaded.
    } else {
      biomid <- as.integer(input$sel_biom)
      namespace <- paste0("biom",input$btn_edit_biom)
      newEditBiomoniteringDataUI(namespace,biomid)
      parameterSets$savedat <- callModule(newEditBiomoniteringData,namespace,
                                          type = "edit",biomid)
    }
  })
  
  
  ### CODE CHUNK FOR HANDLING SIMULATIONS TAB
  
  #New Create simulation dialog
  observeEvent(input$btn_new_sim,{
    set_names <-c("expo","physio","chem","adme",
                  "expovar","physiovar","chemvar","admevar",
                  "biom","extrapolate") 
    selected_list <- list()
    selected_list[set_names]<- list(NULL)
    set_list <- lapply(set_names,function(x){
      if(x == "adme" || x=="extrapolate"){
        return(NULL)
      }else{
        return(parameterSets[[x]]())
      }
    })
    set_list <-setNames(set_list,set_names)
    module_namespace <- paste0("newSim",input$btn_new_sim)
    createSimulationUI(module_namespace,set_list,selected_list)
    parameterSets$savedat <- callModule(createSimulation,
                               module_namespace,type = "new")
  })
  
  observeEvent(input$btn_edit_sim,{
    if(input$sel_sim == ""){
      ## Error bubble no data yet loaded.
    } else {
    simid <- as.integer(input$sel_sim)
    query <- sprintf("Select * from SimulationsSet where simid = %i",simid)
    sim_details <- projectDbSelect(query)
    # List of names for set dropdowns in the create/edit UI module
    set_names <-c("expo","physio","chem","adme",
                  "expovar","physiovar","chemvar","admevar",
                  "biom","extrapolate")
    # Get the choice of adme ids for the given simulation
    query <- sprintf("Select name,admeid from AdmeSet where chemid = %d AND physioid = %d AND expoid = %d;",
                     sim_details$chemid,
                     sim_details$physioid, 
                     sim_details$expoid)
    res <- projectDbSelect(query)
    adme_set <- as.list(res[["admeid"]])
    names(adme_set)<- res$name
    #create list for set selection dropdown
    set_list <- lapply(set_names, function(x,adme_set){
      if(x == "adme"){
        return(adme_set)
      }else if(x=="extrapolate"){
        return(NULL)
      }else{
        return(parameterSets[[x]]())
      }
      
    },adme_set)
    set_list <- setNames(set_list,set_names)
    
    # create list for selected options from the dropdowns
    selected_list <- lapply(set_names,function(x){
      var_id <- paste0(x,"id")
      return(sim_details[[var_id]])
      
    })
    selected_list<- setNames(selected_list,set_names)
    
    # Update simulation settings based on simulation type
    simulation_settings <- list()
    simulation_settings$simid <- simid
    simulation_settings$name <- sim_details$name
    simulation_settings$descrp <- sim_details$descrp
    simulation_settings$sim_type <- sim_details$sim_type
    simulation_settings$tstart <- sim_details$tstart
    simulation_settings$sim_dur <- sim_details$sim_dur
    simulation_settings$dur_units <- sim_details$dur_units
    
    sim_type <- sim_details$sim_type
    if(sim_details$sim_type %in% c("rd","r2r")){
      simulation_settings$expo_range <- c(sim_details$low_dose_estimate,
                                          sim_details$high_dose_estimate)
      simulation_settings$num_expos <- sim_details$num_expos
      
    }
    if(sim_details$sim_type != "fd"){
      simulation_settings$mcruns <- sim_details$mcruns
    }
    module_namespace <- paste0("newSim",input$btn_edit_sim)
    createSimulationUI(module_namespace,set_list,selected_list)
    parameterSets$savedat <- callModule(createSimulation,
                               module_namespace,type = "edit",simulation_settings)
    
  }})


  # update the paramter set dropdown if it is changed
  observe({
    result_vector <- parameterSets$savedat
    if(result_vector()[1]=="Yes"){
      set_type <- result_vector()[2]
      set_id <- result_vector()[3]
      set_list <- getAllSetChoices(set_type)
      parameterSets[[set_type]] <- reactiveVal(set_list)
      updateSelectizeInput(session,paste0("sel_",set_type),choices = set_list, selected = set_id)
      if(set_type == "chem"){
        updateSelectizeInput(session,"sel_chem4adme",choices = set_list)
        updateSelectizeInput(session,"sel_metabolite4adme",choices = c("No Metabolite"=0,set_list))
      }else if (set_type =="physio"){
        updateSelectizeInput(session,"sel_physio4adme",choices = set_list)
      }else if(set_type == "expo"){
        updateSelectizeInput(session,"sel_expo4adme",choices = set_list)
      }
      parameterSets$savedat <- reactiveVal(c("No","",0))
      saveProject()
      # updateSelectizeInput(session,paste0("sel_scene_",set_type),choices = set_list)
    }
  })

  ### Code chunk for handling save/restore buttons
  #Save restore physiologcal set
  observeEvent(input$btn_sverest_physio,{
    physioid <- input$sel_physio
    if(physioid == ""){
      sendSweetAlert(session,title = "No Physiological Set Found",type ="error",
                     "Please create an physiological set first")
    }else{
      set_values <- getParameterSet("physio",physioid)
      UI_values <- reactiveValuesToList(input)[paste0("ms_",physio_name_df$Var)]
      names(UI_values) <- gsub("ms_","",names(UI_values))
      module_ns <- paste0("physiorest",input$btn_sverest_physio)
      saveRestoreParameterSetUI(input$btn_sverest_physio)
      parameterSets$sverestdat <- callModule(saveRestoreParameterSet,
                                             input$btn_sverest_physio,
                                             UI_values,set_values,
                                             physio_name_df,"physio")
    }
    

  })

  #Save-restore exposure set
  observeEvent(input$btn_sverest_expo,{
    expoid <- input$sel_expo
    if(expoid == ""){
      sendSweetAlert(session,title = "No Exposure Set Found",type ="error",
                     "Please create an exposure set first")
    }else{
      set_values <- getParameterSet("expo",expoid)
      UI_values <- reactiveValuesToList(input)[paste0("ms_",expo_name_df$Var)]
      names(UI_values) <- gsub("ms_","",names(UI_values))
      module_ns <- paste0("exporest",input$sverest_expo)
      saveRestoreParameterSetUI(module_ns)
      parameterSets$sverestdat <- callModule(saveRestoreParameterSet,
                                             module_ns,
                                             UI_values,set_values,
                                             expo_name_df,"expo")
    }
    
  })

  #Save-restore chemical set
  observeEvent(input$btn_sverest_chem,{
    chemid <- input$sel_chem
    if(chemid == ""){
      sendSweetAlert(session,title = "No Chemical Set Found",type ="error",
                     "Please create an chemical set first")
    }else{
      set_values <- getParameterSet("chem",chemid)
      
      #chem_vars <- subset(chem_name_df$Var,!(chem_name_df$Var %in% c("name","cas","descrp")))
      UI_values <- reactiveValuesToList(input)[paste0("ms_",chem_name_df$Var)]
      names(UI_values) <- gsub("ms_","",names(UI_values))
      module_ns <- paste0("chemrest",input$btn_sverest_chem)
      saveRestoreParameterSetUI(module_ns)
      parameterSets$sverestdat <- callModule(saveRestoreParameterSet,
                                             module_ns,
                                             UI_values,set_values,
                                             chem_name_df,"chem")
    }
    
  })
  
  #Save/Restore Button function
  observeEvent(input$btn_sverest_adme,{
    sendSweetAlert(session,"Unavailable","Save/Restore Button is unavailable for ADME sets in this version of PLETHEM.")
    # admeid <- input$sel_adme
    # set_values <- getParameterSet("adme",admeid)
    # #chem_vars <- subset(chem_name_df$Var,!(chem_name_df$Var %in% c("name","cas","descrp")))
    # UI_values <- reactiveValuesToList(input)[paste0("ms_",adme_name_df$Var)]
    # names(UI_values) <- gsub("ms_","",names(UI_values))
    # module_ns <- paste0("admerest",input$btn_sverest_adme)
    # saveRestoreParameterSetUI(module_ns)
    # parameterSets$sverestdat <- callModule(saveRestoreParameterSet,
    #                                        module_ns,
    #                                        UI_values,set_values,
    #                                        adme_name_df,"adme")
  })

  observe({
    result_vector <- parameterSets$sverestdat()
    ops_type <- result_vector[1]
    if (ops_type == "save"){
      type <- result_vector[5]
      input_id <- as.integer(isolate(input[[paste0("sel_",type)]]))
      id_name <- paste0(type,"id")
      if (type == "physio"){
        table_name <- "Physiological"
      }else if(type == "chem"){
        table_name <- "Chemical"
      }else if(type == "expo"){
        table_name <- "Exposure"
      }else{
        table_name <- "Adme"
      }


      # create a data frame for the mapply below
      val_df <- data.frame("var"=result_vector[2],"val"= result_vector[4],stringsAsFactors = FALSE,row.names = NULL)
      print(val_df)
      # create the query
      query_list <-mapply(function(var,val,tbl_nme,id_nme,id){
        temp <- sprintf("UPDATE %s SET value = '%s' WHERE %s = %i AND param = '%s';",
                        tbl_nme,val,id_nme,id,var)
        return(temp)
      },
      val_df$Variable,val_df$Current.Value,table_name,id_name,input_id,SIMPLIFY = T)
      lapply(query_list,projectDbUpdate)
      saveProject()

    }else if (ops_type == "restore"){
      type <- result_vector[5]
      if (type == "physio"){
        name_data <- physio_name_df
      }else if(type == "chem"){
        name_data <- chem_name_df
      }else if(type == "expo"){
        name_data <- expo_name_df
      }else{
        name_data <- adme_name_df
      }
      var_type <- sapply(result_vector$Variable,function(var){
        tempvar <-  name_data$ParamType[which(name_data$Var == var, arr.ind = T)]
        return(tempvar)})
      change_df <- data.frame("Var"=result_vector$Variable,
                              "Val" = result_vector[["Original Value"]],
                              "ParamType"=var_type,
                              row.names = NULL,
                              stringsAsFactors = F)
      updateUIInputs(session,change_df)


    }
  })
  
  observeEvent(input$btn_new_varphys,{
    param_names <- physio_name_df$Name[which(physio_name_df$Variability == "TRUE")]
    param_vars <- physio_name_df$Var[which(physio_name_df$Variability == "TRUE")]
    names(param_vars) <- param_names
    ns <- paste0("vpn_",input$btn_new_varphys)
    newEditVariabilityUI(ns)
    parameterSets$vardat <- callModule(newEditVariability,ns,"physio","new",param_vars)
    ### Variability Tab
  },ignoreInit = T, ignoreNULL = T)
  
  observeEvent(input$btn_edit_varphys,{
    if(input$sel_physio_var == ""){
      sendSweetAlert(session,"No Set Found","No Variability set is created",
                     type = "error")
    }else{
      param_names <- physio_name_df$Name[which(physio_name_df$Variability == "TRUE")]
      param_vars <- physio_name_df$Var[which(physio_name_df$Variability == "TRUE")]
      names(param_vars) <- param_names
      ns <- paste0("vpe_",input$btn_edit_varphys)
      newEditVariabilityUI(ns)
      parameterSets$vardat <- callModule(newEditVariability,ns,"physio","edit",
                                         param_vars,input$sel_physio_var)
    }
    
    ### Variability Tab
  },ignoreInit = T, ignoreNULL = T)
  
  observeEvent(input$btn_new_varchem,{
    param_names <- chem_name_df$Name[which(chem_name_df$Variability == "TRUE")]
    param_vars <- chem_name_df$Var[which(chem_name_df$Variability == "TRUE")]
    names(param_vars) <- param_names
    ns <- paste0("vcn_",input$btn_new_varchem)
    newEditVariabilityUI(ns)
    parameterSets$vardat <- callModule(newEditVariability,ns,"chem","new",param_vars)
    ### Variability Tab
  },ignoreInit = T, ignoreNULL = T)
  
  observeEvent(input$btn_edit_varchem,{
    if(input$sel_chem_var == ""){
      sendSweetAlert(session,"No Set Found","No Variability set is created",
                     type = "error")
    }else{
      param_names <- chem_name_df$Name[which(chem_name_df$Variability == "TRUE")]
      param_vars <- chem_name_df$Var[which(chem_name_df$Variability == "TRUE")]
      names(param_vars) <- param_names
      ns <- paste0("vce_",input$btn_edit_varchem)
      newEditVariabilityUI(ns)
      parameterSets$vardat <- callModule(newEditVariability,ns,"chem","edit",
                                         param_vars,input$sel_chem_var)
    }
    ### Variability Tab
  },ignoreInit = T, ignoreNULL = T)
  
  observeEvent(input$btn_new_varexpo,{
      param_names <- expo_name_df$Name[which(expo_name_df$Variability == "TRUE")]
      param_vars <- expo_name_df$Var[which(expo_name_df$Variability == "TRUE")]
      names(param_vars) <- param_names
      ns <- paste0("ven_",input$btn_new_varexpo)
      newEditVariabilityUI(ns)
      parameterSets$vardat <- callModule(newEditVariability,ns,"expo","new",param_vars)
    
    ### Variability Tab
  },ignoreInit = T, ignoreNULL = T)
  
  observeEvent(input$btn_edit_varexpo,{
    if(input$sel_expo_var == ""){
      sendSweetAlert(session,"No Set Found","No Variability set is created",
                     type = "error")
    }else{
    param_names <- expo_name_df$Name[which(expo_name_df$Variability == "TRUE")]
    param_vars <- expo_name_df$Var[which(expo_name_df$Variability == "TRUE")]
    names(param_vars) <- param_names
    ns <- paste0("vee_",input$btn_edit_varexpo)
    newEditVariabilityUI(ns)
    parameterSets$vardat <- callModule(newEditVariability,ns,"expo","edit",
                                       param_vars,input$sel_expo_var)
    }
  },ignoreInit = T, ignoreNULL = T)
    
    observeEvent(input$btn_new_varadme,{
      param_names <- adme_name_df$Name[which(adme_name_df$Variability == "TRUE")]
      param_vars <- adme_name_df$Var[which(adme_name_df$Variability == "TRUE")]
      names(param_vars) <- param_names
      ns <- paste0("ven_",input$btn_new_varadme)
      newEditVariabilityUI(ns)
      parameterSets$vardat <- callModule(newEditVariability,ns,"adme","new",param_vars)
      ### Variability Tab
    },ignoreInit = T, ignoreNULL = T)
    
    observeEvent(input$btn_edit_varadme,{
      if(input$sel_adme_var == ""){
        sendSweetAlert(session,"No Set Found","No Variability set is created",
                       type = "error")
      }else{
        param_names <- adme_name_df$Name[which(adme_name_df$Variability == "TRUE")]
        param_vars <- adme_name_df$Var[which(adme_name_df$Variability == "TRUE")]
        names(param_vars) <- param_names
        ns <- paste0("vee_",input$btn_edit_varadme)
        newEditVariabilityUI(ns)
        parameterSets$vardat <- callModule(newEditVariability,ns,"adme","edit",
                                           param_vars,input$sel_adme_var)
      }
    },ignoreInit = T, ignoreNULL = T)
    ### Variability Tab
  
  
  observe({
    result_vector <- parameterSets$vardat
    if (result_vector()[1]=="Yes"){
      set_type <- result_vector()[2]
      varid <- result_vector()[3]
      set_list <- getVariabilitySetChoices(set_type)
      parameterSets[[paste0(set_type,"var")]] <- reactiveVal(set_list)
      updateSelectizeInput(session,paste0("sel_",set_type,"_var"),choices = NULL)
      updateSelectizeInput(session,
                           paste0("sel_",set_type,"_var"),
                           choices = set_list,
                           selected = as.integer(varid))
      saveProject()
    }
  })
  
  observeEvent(input$sel_physio_var,{
    varid <- input$sel_physio_var
    query <- sprintf("Select var_tble from Variability where varid = %d;",as.integer(varid))
    var_data <- projectDbSelect(query)
    dataset <- unserialize(charToRaw(var_data$var_tble))
    dataset <- dataset[,c(-2)]
    output$physio_var_tble <- DT::renderDT(DT::datatable(dataset,rownames = "",
                                                         colnames = c("Use Bounds" = 5,
                                                                      "Upper Bound"=6,
                                                                      "Lower Bound"=7)
                                                         )
                                           )
    
  },ignoreInit = TRUE, ignoreNULL =  TRUE)
  
  observeEvent(input$sel_chem_var,{
    varid <- input$sel_chem_var
    query <- sprintf("Select var_tble from Variability where varid = %d;",as.integer(varid))
    var_data <- projectDbSelect(query)
    dataset <- unserialize(charToRaw(var_data$var_tble))
    dataset <- dataset[,c(-2)]
    output$chem_var_tble <- DT::renderDT(DT::datatable(dataset,rownames = "",
                                                         colnames = c("Use Bounds" = 5,
                                                                      "Upper Bound"=6,
                                                                      "Lower Bound"=7)
    ))
    
  },ignoreInit = TRUE, ignoreNULL =  TRUE)
  
  observeEvent(input$sel_expo_var,{
    varid <- input$sel_expo_var
    query <- sprintf("Select var_tble from Variability where varid = %d;",as.integer(varid))
    var_data <- projectDbSelect(query)
    dataset <- unserialize(charToRaw(var_data$var_tble))
    dataset <- dataset[,c(-2)]
    output$expo_var_tble <- DT::renderDT(DT::datatable(dataset,rownames = "",
                                                         colnames = c("Use Bounds" = 5,
                                                                      "Upper Bound"=6,
                                                                      "Lower Bound"=7)
    ))
    
  },ignoreInit = TRUE, ignoreNULL =  TRUE)
  
  observeEvent(input$sel_adme_var,{
    varid <- input$sel_adme_var
    query <- sprintf("Select var_tble from Variability where varid = %d;",as.integer(varid))
    var_data <- projectDbSelect(query)
    dataset <- unserialize(charToRaw(var_data$var_tble))
    dataset <- dataset[,c(-2)]
    output$adme_var_tble <- DT::renderDT(DT::datatable(dataset,rownames = "",
                                                       colnames = c("Use Bounds" = 5,
                                                                    "Upper Bound"=6,
                                                                    "Lower Bound"=7)
    ))
    
  },ignoreInit = TRUE, ignoreNULL =  TRUE)
  
  
## CODE CHUNK TO UPDATE UI BASED ON SELECTED SET FROM THE DROPDOWN
  
  #update the inputs for the parameter set selected
  observeEvent(input$sel_physio,{
    physioid <- input$sel_physio
    #get values for the inputs
    physio_values <- getParameterSet("physio",physioid)
    # get all numeric values in the physio names dataframe
    params_df <- physio_name_df
    params_df$Val <- physio_values[physio_name_df$Var]
    updateUIInputs(session,params_df)
    shinyBS::updateButton(session,"btn_use_lifecourse",style = "primary")
    shinyBS::updateButton(session,"btn_useQSAR4Partition",style = "primary")
  },ignoreInit = TRUE, ignoreNULL =  TRUE)
  
  #update the inputs for the exposure set selected
  observeEvent(input$sel_expo,{
    expoid <- input$sel_expo
    #get the values for inputs
    expo_values <- getParameterSet("expo",expoid)
    params_df <- expo_name_df
    params_df$Val <- expo_values[expo_name_df$Var]
    updateUIInputs(session,params_df)

  },ignoreInit = TRUE, ignoreNULL = TRUE)
  
  #update the inputs for the chemical set selected
  observeEvent(input$sel_chem,{
    chemid <- input$sel_chem
    #get the values for inputs
    chem_values <- getParameterSet("chem",chemid)
    params_df <- chem_name_df
    params_df$Val <- chem_values[chem_name_df$Var]
    updateUIInputs(session,params_df)

  },ignoreInit = TRUE, ignoreNULL = TRUE)
  
  #update the inputs for the adme set selected
  observeEvent(input$sel_adme,{
    admeid <- as.integer(input$sel_adme)
    #get the values for inputs
    adme_values <- getParameterSet("adme",admeid)
    params_df <- adme_name_df
    params_df$Val <- adme_values[adme_name_df$Var]
    updateUIInputs(session,params_df)
    # ADME Set is special and also has other updates
    adme_details <- projectDbSelect(sprintf("Select chemid, physioid, expoid, metabid from AdmeSet where admeid = %i",
                                    admeid))
    updateSelectizeInput(session,"sel_expo4adme",selected = adme_details$expoid)
    updateSelectizeInput(session,"sel_chem4adme",selected = adme_details$physio)
    updateSelectizeInput(session,"sel_physio4adme",selected = adme_details$physio)
    updateSelectizeInput(session,"sel_metabolite4adme",select = adme_details$metabid)
    
  },ignoreInit = TRUE, ignoreNULL = TRUE)
  
  #update the inputs for the biomonitering set selected
  observeEvent(input$sel_biom,{
    
    biomid  <- as.integer(input$sel_biom)
    query <- sprintf("Select chem,tissue,units,data from Biomonitering where biomid = %i;",
                    biomid)

    biom_db_data <- projectDbSelect(query)
    chem <- biom_db_data$chem
    tissue <- biom_db_data$tissue
    units <- switch(biom_db_data$units,
                    "uml"="\u00B5moles/L",
                    "ngl"="ng/L",
                    "mgl"="mg/L",
                    "ugd"="\u00B5g/day")
    data <- unserialize(charToRaw(biom_db_data$data))
    density_fit <- density(data[,1],kernel = "gaussian") 
    density_fit$y <- density_fit$y/max(density_fit$y)

    if(tissue=="pls"){
      if(chem=="prnt"){
        x_label <- paste0("Parent Chemical Plasma Concentration (",units,")")
      }else{
        x_label <- paste0("Metabolite Concentration (",units,")")
      }
    }else{
      if(chem == "prnt"){
        if(biom_db_data$units == "ugd"){
          x_label <- paste0("Amount of Parent Chemical Excreted in Urine (\u00B5g/day)")
        }else{
          x_label <- paste0("Urinary Concentration of Parent Chemical (",units,")")
        }
      }else{
        if(biom_db_data$units == "ugd"){
          x_label <- paste0("Amount of Metabolite Excreted in Urine (\u00B5g/day)")
        }else{
          x_label <- paste0("Urinary Concentration of the Metabolite(",units,")")
        }
      }
    }
    output$biom_hist <- plotly::renderPlotly({
      p <- plot_ly(x = data[,1],type = "histogram", name = "Data")%>%
        add_trace(x = density_fit$x, y = density_fit$y,
                  type = "scatter",fill = "tozeroy",mode = "lines",
                  name = "Density",yaxis = "y2")%>%
        layout(
          title = "Biomonitoring Data",
          yaxis = list(title = "Count"),
                       
          yaxis2 = list(overlaying = "y",title ="Density",side = "right"),
          xaxis = list(title = x_label)
        )
    })
  },ignoreInit = T,ignoreNULL = T)
  

  ### This code chunk deals with updating pair using qsar models
  observeEvent(input$qsar4chem_props,{
    qsar_model <- input$qsarModelChem
    org <- ifelse(input$ms_org=="ha","human","rat")
    
    chemical_params <- list("den"=input$ms_den, "mw"=input$ms_mw,
                            "vpa"=input$ms_vpa, "dkow"=input$ms_dkow,
                            "lkow"=input$ms_lkow, "wsol"=input$ms_wsol,
                            "res"=input$ms_res,  "vmaxc"=input$ms_vmaxc,
                            "km"=input$ms_km)
    partitions <- calculatePartitionCoefficients(qsar_model,
                                                 chemical_params,
                                                 NULL,
                                                 org)
    pair <- partitions$pair
    frwsol <- partitions$frwsol
   
    updateNumericInput(session,"ms_frwsol",value = frwsol)
  })
  
  ## This code chunk deals with performing IVIVE for the chemical
  observeEvent(input$btn_ivive_chem,{
    performIVIVEUI(input$btn_ivive_chem)
    dataset$iviveDat <<- callModule(performIVIVE,input$btn_ivive_chem,input$ms_km)
  })
  
  observe({
    ivive_val <- dataset$iviveDat()
    if(ivive_val[1]=="Yes"){
      updateNumericInput(session,"ms_vkm1c",value = signif(as.numeric(ivive_val[2]),4))
      updateNumericInput(session,"ms_vmaxc",value = signif(as.numeric(ivive_val[3]),4))
      updateNumericInput(session,"ms_km",value = signif(as.numeric(ivive_val[4]),4))
    }
  })





  #### The next code chunk resets all the exposures in the app to zero.
  observeEvent(input$clear_expo,{
    input_names <- c("ms_bdose","ms_drdose","ms_vdw","ms_inhdose","ms_ivdose")
    lapply(input_names, function(x){updateNumericInput(session,x,value = 0)})
  })
  
  # Code chuck for displaying the correct absorptions inputs based on the exposure set seleted
  # in the ADME tab
  observeEvent(input$sel_expo4adme,{
    expoid <- as.integer(input$sel_expo4adme)
    query <- sprintf("SELECT value from Exposure where expoid = %i AND param = 'expo_sidebar'",expoid)
    expo_route <- projectDbSelect(query)$value
    toggleElement(id = "ms_pair",condition = (expo_route=="inh"))
    toggleElement(id = "ms_ka",
                  condition = (expo_route=="oral"||expo_route == "dw"||expo_route == "oralv"))
    toggleElement(id = "ms_fa",
                  condition = (expo_route == "oral"||expo_route == "dw"))
    toggleElement(id = "ms_kVtoL",condition = (expo_route == "oralv"))
    toggleElement(id = "ms_KPtot",condition = (expo_route == "derm"))
    toggleElement(id = "ms_maxcap",condition = (expo_route == "derm"))
    toggleElement(id = "ms_Kevap",condition = (expo_route == "derm"))
                  
    # if (expo_route=='inh'){
    #   hide
    # } 
  },ignoreNULL = T,ignoreInit = T)
  
  # if no metabolite is selected, remove metabolite specific value from the 
  # UI
  observeEvent(input$sel_metabolite4adme,{
    toggleElement("ms_fuplsmet",condition =input$sel_metabolite4adme != 0)
    toggleElement("ms_vdmetc",condition =input$sel_metabolite4adme != 0)
  })


  # metab_colnames <- reactive({
  #   if (input$metab_type == "m2"){
  #     c("Age(years)","Clearance(L/h/kg Liver)")
  #   }else{
  #     c("Age(years)","Clearance (\u00B5M/h/kg BW ^ 0.75)")
  #   }
  # })
  output$metab_tble <- DT::renderDT(DT::formatRound(DT::datatable(metabolism_dataframe,
                                                              caption = "Metabolism Table",

                                                              rowname = NULL,editable = F,
                                                              options= list(dom = "tp",pageLength = 5)),
                                                2,digits = 4,mark = "" ),
                                    server = T)
  metab_proxy <- DT::dataTableProxy("metab_tble",session)

  #Save current metabolism set.
  observeEvent(input$btn_saveas_metab,{
    if(is.null(input$metab_csv)){
      sendSweetAlert(session,"Error","No Dataset Uploaded","error")
    }else if(input$metab_set_name=="" || input$metab_set_descrp==""){
      sendSweetAlert(session,"Error","Both name and description are required","error")
    }else{
      #validate(need(input$metab_csv,"Metabolism Data"))
      #id <- input$sel_metab
      set_type <- "metab"
      id_name <- "metabid"
      set_table_name <- "MetabolismSet"
      set_name <- "Metabolism"
      # get the current ID for the parameter set.
      query <- sprintf("SELECT %s FROM %s ;",id_name,set_table_name)
      id_list <- projectDbSelect(query)
      if (length(id_list[[id_name]])==0){
        id_num = 1
      }else{
        id_num = max(id_list[[id_name]])+1
      }
      metab_type <- input$metab_type
      ref_age <- input$metab_ref_age
      use_ref <- as.character(input$use_ref)


      # write the name to correct "Set" table
      query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                       set_table_name,id_name,id_num,
                       input$metab_set_name,input$metab_set_descrp)
      projectDbUpdate(query)

      # serialize and convert the loaded table to database
      serialized_metab_tble <- rawToChar(serialize(metab_tble(),NULL,T))

      query <- sprintf("INSERT INTO Metabolism (metabid,type,use_ref,ref_age,metab_tble) Values (%d,'%s','%s',%f,'%s');",
                       id_num,
                       metab_type,
                       use_ref,
                       ref_age,
                       serialized_metab_tble)
      projectDbUpdate(query)

      set_list <- getAllSetChoices(set_type)
      parameterSets[[set_type]]<- reactiveVal(set_list)
      updateSelectizeInput(session,paste0("sel_",set_type),choices = set_list, selected = id_num)
    }
  })

  #update the UI on selecting input
  observeEvent(input$sel_metab,{
    metab_id <- input$sel_metab
    # get the name a description and update
    query <- sprintf("Select name,descrp From MetabolismSet where metabid = %d;",
                     as.integer(metab_id))

    ret_data <- projectDbSelect(query)
    updateTextInput(session,"metab_set_name",value = ret_data[["name"]])
    updateTextAreaInput(session,"metab_set_descrp",value = ret_data[["descrp"]])

    query <- sprintf("Select type,ref_age,metab_tble From Metabolism where metabid = %d",
                     as.integer(metab_id))
    ret_data <- projectDbSelect(query)
    #print(ret_data)
    shinyWidgets::updateRadioGroupButtons(session,"metab_type",selected = ret_data[["type"]])
    shinyWidgets::updateAwesomeCheckbox(session,"use_ref",value = as.logical(ret_data[["use_ref"]]))
    updateNumericInput(session,"metab_ref_age",value = ret_data[["ref_age"]])
    metabolism_dataframe <<- unserialize(charToRaw(ret_data[["metab_tble"]]))
    DT::replaceData(metab_proxy,metabolism_dataframe,rownames = F)

  },ignoreInit = TRUE, ignoreNULL = TRUE)
  
  ## CHUNK FOR HANDLING METABOLISM TAB UNDER ADME

  # show the modal to upload files when
  observeEvent(input$btn_metab_upload,{showModal(modalDialog(title = "Upload Metabolism Data",
                                                             tagList(
                                                               fluidPage(
                                                                 fluidRow(
                                                                   column(width = 5,
                                                                          fileInput("metab_csv","Upload Metabolism Data")),
                                                                   column(width = 5,
                                                                          downloadLink("metab_template","Template for metabolism file"))
                                                                 ),
                                                                 fluidRow(
                                                                   column(width = 4,
                                                                          textInput("metab_set_name","Name",
                                                                                    placeholder = "Enter the name for this metabolism set")),
                                                                   column(width = 8,
                                                                          textAreaInput("metab_set_descrp","Description",
                                                                                        resize = "none" ,row = 1))

                                                                 ),
                                                                 fluidRow(column(width = 12,
                                                                                 shinyWidgets::radioGroupButtons("metab_type",justified = T,
                                                                                                                 "Select Meatbolism Type",
                                                                                                                 choices = c("Saturable Hepatic"="m1","Linear Hepatic"="m2",
                                                                                                                             "Plasma Clearance"="m3","Gut Clerance"="m4"))
                                                                 )



                                                                 ),
                                                                 fluidRow(
                                                                 column(width = 6,
                                                                        shinyBS::popify(numericInput("metab_ref_age",
                                                                                     "Reference age in Years",
                                                                                     value = 25, min = 0),
                                                                                     title = "",
                                                                                     content = "If age defined in the physiological parameters is not a part of the table, the value at this age will be used")
                                                                 )
                                                                 ),
                                                                 fluidRow(
                                                                   fluidRow(column(width = 6, offset = 3,
                                                                                   DT::DTOutput("metab_upload_tble")))
                                                                 )

                                                               )
                                                             ),
                                                             size ="l",
                                                             footer = tagList(
                                                               actionButton("metab_upload_done","Add Metabolism"),
                                                               modalButton("Cancel")
                                                             )
  ))
  })
  output$metab_template <- downloadHandler(
    filename = function(){"Metabolism_Template.csv"},
    content = function(file){write.csv(data.frame("Age"=c(25),"Clearence"=c(0),stringsAsFactors = F),
                                       file,
                                       row.names = F)
    },
    contentType = "text/csv"
  )

  # The selected file
  metabFile <- reactive({
    input$metab_csv
  })

  # The user's data, parsed into a data frame
  metab_upload_tble <- reactive({
    validate(need(input$metab_csv,"No dataset uploaded"))
    ret_dat <- read.csv(metabFile()$datapath,header = T,stringsAsFactors = F)
    return(ret_dat)
  })
  output$metab_upload_tble <- DT::renderDT(DT::formatRound(DT::datatable(metab_upload_tble(),
                                                                     caption = "Metabolism Table",
                                                                     rowname = NULL,editable = F,
                                                                     options= list(dom = "tp",pageLength = 5)),
                                                       2,digits = 4,mark = "" ),
                                           server = T)

  observeEvent(input$metab_upload_done,{
    if(is.null(input$metab_csv)){
      sendSweetAlert(session,"Error","No Dataset Uploaded","error")
    }else if(input$metab_set_name=="" || input$metab_set_descrp==""){
      sendSweetAlert(session,"Error","Both name and description are required","error")
    }else{
      set_type <- "metab"
      id_name <- "metabid"
      set_table_name <- "MetabolismSet"
      set_name <- "Metabolism"
      # get the current ID for the parameter set.
      query <- sprintf("SELECT %s FROM %s ;",id_name,set_table_name)
      id_list <- projectDbSelect(query)
      if (length(id_list[[id_name]])==0){
        id_num = 2
      }else{
        id_num = max(id_list[[id_name]])+1
      }
      metab_type <- input$metab_type
      ref_age <- input$metab_ref_age

      # serialize and convert the loaded table to database
      metab_tble<-metab_upload_tble()
      if (!(ref_age %in% metab_tble$Age)){
        sendSweetAlert(session,"Error","Reference age must be a part of the table","error")
      }else{
        # write the name to correct "Set" table
        query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                         set_table_name,id_name,id_num,
                         input$metab_set_name,input$metab_set_descrp)
        projectDbUpdate(query)

        serialized_metab_tble <- rawToChar(serialize(metab_tble,NULL,T))

        query <- sprintf("INSERT INTO Metabolism (metabid,type,ref_age,metab_tble) Values (%d,'%s',%f,'%s');",
                         id_num,
                         metab_type,

                         ref_age,
                         serialized_metab_tble)
        projectDbUpdate(query)

        set_list <- getAllSetChoices(set_type)
        updateSelectizeInput(session,"sel_metabfiles",choices = set_list, selected = id_num)
        
        removeModal()
      }


    }
  })
  
  # logic for apply button- depending on the selected physiology and metab file,
  # populates the correct clearence value
  observeEvent(input$btn_use_age,{
    age_set <- input$sel_metabfiles
    if (age_set==""){
      sendSweetAlert(session,"Configuration Error","Please upload age dependent metabolism file",
                     type="error")
    }
    else if (input$sel_physio4adme==""){
      sendSweetAlert(session,"Configuration Error","Please create a physiology set",
                     type="error")
    }else{
      sendSweetAlert(session,title = NULL,"This will overwrite any existing data",
                     type="info")
      #get the age from the compartment
      physioid <- input$sel_physio4adme
      query <- sprintf("Select value from Physiological where physioid = %d AND param = 'age';",
                       as.integer(physioid))
      age <- as.integer(projectDbSelect(query)$value)
      metabid <- input$sel_metabfiles
      query <- sprintf("Select type,ref_age,metab_tble From Metabolism where metabid = %d",
                       as.integer(metabid))
      ret_data <- projectDbSelect(query)
      upload_type <- ret_data[["type"]]
      ref_age <- as.integer(ret_data[["ref_age"]])
      metabolism_dataframe <- unserialize(charToRaw(ret_data[["metab_tble"]]))
      if (age %in% metabolism_dataframe$Age){
        val2update <- metabolism_dataframe$Clearance[metabolism_dataframe$Age==age]
      }else{
        alert_message = sprintf("Physiology age not found in uploaded data. Using value at reference age of %d instead",
                                ref_age)
        sendSweetAlert(session,title = NULL,text=alert_message,type="info")
        val2update <- metabolism_dataframe$Clearance[metabolism_dataframe$Age==ref_age]
      }
      if (upload_type == "m1"){
        updateNumericInput(session,"ms_vmaxc",value =val2update)
        updateNumericInput(session,"ms_vkm1c",value =0)
      }else if (upload_type == "m2"){
        updateNumericInput(session,"ms_vkm1c",value =val2update)
        updateNumericInput(session,"ms_vmaxc",value =0)
      }else if (upload_type == "m3"){
        updateNumericInput(session,"ms_kbld",value =val2update)
      }else{
        updateNumericInput(session,"ms_kent",value =val2update)
      }
    }
  })

  ## END METABOLISM TAB UNDER ADME


  observeEvent(input$sel_sim,{
    simid <- as.integer(input$sel_sim)
    # get pertinent data from the database
    # get All values from the simulations database
    query <- sprintf("Select * FROM SimulationsSet Where simid = %i;",
                     simid)

    result <- projectDbSelect(query)
    # Simulation metadata
    sim_descrp <- result[["descrp"]]
    sim_type <- result$sim_type
    tstart <- round(result[["tstart"]],2)
    sim_dur <- round(result[["sim_dur"]],2)
    dur_units <- result$dur_units
    output$sim_descrp <- renderText(sim_descrp)
    output$sim_type <- renderText(switch(sim_type,
                                         "fd"="Forward Dosimetry",
                                         "rd"="Reverse Dosimetry",
                                         "mc"="Forward Dosimetry with Monte Carlo",
                                         "r2r"="Route to Route Extrapolation"))
    output$sim_start <- renderText(as.character(tstart))
    output$sim_dur <- renderText(as.character(sim_dur))
    output$dur_units <- renderText(switch(dur_units,
                                          "h"="Hours",
                                          "d"="Days",
                                          "w"="Weeks"))
    
    admeid <- as.integer(result[["admeid"]])
    chemid <- as.integer(result[["chemid"]])
    expoid <- as.integer(result[["expoid"]])
    physioid <- as.integer(result[["physioid"]])
    

    

    # get chemical name from chem table
    query <- sprintf("SELECT name from ChemicalSet WHERE chemid = %i ;",
                     chemid)

    result <- projectDbSelect(query)
    chem_name <- result$name
    output$sim_chem <- renderText(chem_name)

    # get exposure name form exposure set table
    query <- sprintf("SELECT name from ExposureSet WHERE expoid = %i ;",
                     expoid)

    result <- projectDbSelect(query)
    expo_name <- result$name
    output$sim_expo <- renderText(expo_name)

    # get metabolism data.
    metab_data <- getMetabData(admeid,model)
    output$sim_metab_type <- renderText(metab_data$Type)
    output$sim_metab_units <- renderText(metab_data$Units)
    output$sim_metab_val <- renderText(as.character(round(metab_data$Value,2)))



  },ignoreInit = TRUE, ignoreNULL =  TRUE)
  
  ### CODE CHUNK TO RUN THE SIMULATION
  results <- reactiveValues(pbpk=NULL,expo = NULL,simid = NULL,sim_type = NULL)
  observeEvent(input$btn_run_sim,{
    showTab("menu","output")
    
    
    # Get the simulation details
    simid <- as.integer(input$sel_sim)
    sim_details <- projectDbSelect(sprintf("Select * From SimulationsSet where simid = %i",simid))
    results$sim_type <- sim_details$sim_type
    results$simid <- sim_details$simid
    # Get expo, chem, physio and simulation parameters for the model
    model_params <- getAllParamValuesForModel(simid,model)
    # get initial values for parmeters, states and event times needed by all workflows
    
    initial_values <- calculateInitialValues(model_params)
    
    event_times <- unique(initial_values[['evnt_data']][["time"]])
    times <- initial_values[['times']]
    tstop <- initial_values[['tstop']]
    state <- initial_values[['state']]
    times <- sort(c(deSolve::cleanEventTimes(times,event_times),
                    event_times))
    state <- rapidPBPK_initStates(initial_values$initial_params,state)
    
    output <- rapidPBPK_Outputs
    # if the worfklow requires monte carlo analysis, set up the parameter matrices
    if(sim_details$sim_type %in% c("mc","rd","r2r")){
      mcruns <- sim_details$mcruns 
      MC.matrix <- suppressWarnings(
        getAllVariabilityValuesForModel(simid,model_params$vals,mcruns)
      )
    }
    # Workflow specific scripts
    # Forward Dosimetry
    if(sim_details$sim_type == 'fd'){
      initial_params <- rapidPBPK_initParms(initial_values$initial_params)
      pb <- Progress$new(session, min = 0, max = 100)
      pb$set(value = 99)
      if(run_type== "dev"){
        dyn.load("../../src/plethem.dll")
      }else if(run_type == "prod"){
        dyn.load(system.file("libs",
                             .Platform$r_arch,
                             paste0("plethem",.Platform$dynlib.ext),
                             package = "plethem")
                 )
      }
      
      
      
      modelOutput<- deSolve::ode(y = state, times = times,method = "lsodes",
                                 func = "derivs", dllname = "plethem",initfunc= "initmod",parms = initial_params,
                                 events=list(func="event", time=event_times),nout = length(output),
                                 outnames = output)
      if(run_type == "dev"){
        dyn.unload("../../src/plethem.dll")
      }else if(run_type == "prod"){
        dyn.unload(system.file("libs",.Platform$r_arch,paste0("plethem",.Platform$dynlib.ext),package = "plethem"))
      }
      dfModelOutput <- as.data.frame(modelOutput,stringsAsFactors = F)
      results$pbpk<- dfModelOutput
      pb$close()
      updateNavbarPage(session,"menu","output")
    }#MonteCarlo Mode
    else if(sim_details$sim_type == 'mc'){
      mcruns <- sim_details$mcruns
      pb <- Progress$new(session,min = 0 , max = mcruns)
      updatePB <- function(value = NULL){
        pb$set(value = value,message = sprintf("Simulating Model %i",value))
      }
      pb$set(value = 0,message = "Calculating Parameter Values")
      params_list <- lapply(1:mcruns,c)

       for(each_run in 1:mcruns){
         model_params$vals[colnames(MC.matrix)]<- MC.matrix[each_run,]
         params <- calculateInitialValues(model_params)$initial_params
         params <- rapidPBPK_initParms(params)
         params_list[[each_run]]<- params
       }
      states_list <- replicate(mcruns,state,F)
      times_list <- replicate(mcruns,times,F)
      event_times_list <- replicate(mcruns,event_times,F)
      output_list <- replicate(mcruns,output,F)
      cmax_list <- runMCParallel(mcruns,params_list,states_list,output_list,
                                 times_list,event_times_list,updatePB)
      results$pbpk <- cmax_list
      pb$close()
      
    }# Reverse Dosimetry
    else{ #if(sim_details$sim_type == 'rd'){
      # reverse dosimetry specific Calculations
      if (sim_details$sim_type == 'rd'){
        shinybusy::show_modal_progress_circle(value = 0,"Getting Biomonitering Details")
        # get biomonietring details
        biomid <- sim_details$biomid
        biom_details <- projectDbSelect(sprintf("Select * from Biomonitering where biomid = %i",biomid))
        biom_data <- unserialize(charToRaw(biom_details$data))
        chem <- biom_details$chem
        tissue <- biom_details$tissue
        units <- biom_details$units
        #ADME DETAILS:
        metabid <- projectDbSelect(sprintf("Select metabid from AdmeSet where admeid = %i",
                                           sim_details$admeid)
                                   )$metabid
        # create the list of lists for identifying the correct model variable 
        # that corresponds to the biomonitering data
        model_var_dict <- list(
          "pls"=list("prnt"=list("mgl"="cpls","uml"="cpls"),
                     "met"=list("mgl"="cmet","uml"="cmet")),
          "urine"=list("prnt"=list("mgl"="curine","ugd"="aurine"),
                       "met"=list("mgl"="cmet_urine","ugd"="amet_urine"))
        )
        if(chem == "prnt" && units == "mgl"){
          mw <- projectDbSelect(sprintf("Select value from Chemical where param = 'mw' and chemid = %i",
                                        sim_details$chemid))$value
          multiplier <- as.numeric(mw)/1000 
        }else if (chem == "met" && units == "mgl"){
          mw <- projectDbSelect(sprintf("Select value from Chemical where param = 'mw' and chemid = %i",
                                        sim_details$metabid))$value
          multiplier <- as.numeric(mw)/1000
        }else{
          multiplier <- 1
        }
        
        model_var<- model_var_dict[[tissue]][[chem]][[units]]
        # get the route for which exposure is to be estimated
        expo_route <- projectDbSelect(sprintf(
          "Select value from Exposure where param == 'expo_sidebar' AND expoid = %i;",
          sim_details$expoid
        ))$value
        
        #Get all exposure variables that are needed by the model
        expo_vars_list <- mainDbSelect("Select Var from ParamNames Where ModelParams = 'TRUE' AND ParamSet = 'Exposure' AND Model = 'rapidPBPK';")$Var
        
        # get all dataframe of values and params for the selected exposure set
        new_expo_data <- projectDbSelect(paste0(
          sprintf("Select param,value from Exposure where expoid = %i AND param in ",sim_details$expoid),
          "(",
          paste(lapply(expo_vars_list,function(x){
            paste0("'",x,"'")
          }),sep = "",collapse = ","),
          ")")
        )
        
        
      }else{
        shinybusy::show_modal_progress_circle(value = 0,"Running simulation for current route of exposure")
        # Run the original model to generate a biomonitering equivalent
        pb <- Progress$new(session,min = 0 , max = mcruns)
        updatePB <- function(value = NULL){
          pb$set(value = value,message = sprintf("Simulating Model %i",value))
        }
        pb$set(value = 0,message = "Calculating Parameter Values")
        params_list <- lapply(1:mcruns,c)
        for(each_run in 1:mcruns){
          model_params$vals[colnames(MC.matrix)]<- MC.matrix[each_run,]
          params <- calculateInitialValues(model_params)$initial_params
          params <- rapidPBPK_initParms(params)
          params_list[[each_run]]<- params
        }
        states_list <- replicate(mcruns,state,F)
        times_list <- replicate(mcruns,times,F)
        event_times_list <- replicate(mcruns,event_times,F)
        output_list <- replicate(mcruns,output,F)
        cmax_list <- runMCParallel(mcruns,params_list,states_list,
                                   output_list,times_list,event_times_list,updatePB)
        biom_data<- as.data.frame(cmax_list[,"cpls"])
        pb$close()
        # create the list of lists for identifying the correct model variable
        # that corresponds to the biomonitering data
        
        multiplier <- 1
        model_var<- "cpls"
       # get the route for which exposure is to be estimated
        expo_route <- projectDbSelect(sprintf(
          "Select value from Exposure where param == 'expo_sidebar' AND expoid = %i;",
            sim_details$extrapolateid
          ))$value

          #Get all exposure variables that are needed by the model
        expo_vars_list <- mainDbSelect("Select Var from ParamNames Where ModelParams = 'TRUE' AND ParamSet = 'Exposure' AND Model = 'rapidPBPK';")$Var

          # get all dataframe of values and params for the selected exposure set
        new_expo_data <- projectDbSelect(paste0(
            sprintf("Select param,value from Exposure where expoid = %i AND param in ",sim_details$extrapolateid),
            "(",
            paste(lapply(expo_vars_list,function(x){
              paste0("'",x,"'")
            }),sep = "",collapse = ","),
            ")")
          )
      }
      
      update_modal_progress(0,text = "Setting up multiple MC simulations")
      # details for generating MC simulation for reverse dosimetry
      low_expo <-sim_details$low_dose_estimate
      high_expo <- sim_details$high_dose_estimate
      num_expos <- sim_details$num_expos
      modelMCdata <- as.data.frame(matrix(NA,nrow = mcruns,ncol = num_expos))
      dose_list <- pracma::logseq(low_expo,high_expo,num_expos)
      # run a monte carlo simulation for each dose
      for (idx in seq_along(dose_list)){
        update_modal_progress(idx/num_expos,sprintf("Running dose %d of %d",idx,num_expos))
        each_dose <- dose_list[[idx]]
        pb <- Progress$new(session,min = 0 , max = mcruns)
        updatePB <- function(value = NULL){
          pb$set(value = value,message = sprintf("Simulating Model %i",value))
        }
        pb$set(value = 0,message = "Calculating Parameter Values")
        # Set up MC matrices
        params_list <- lapply(1:mcruns,c)#replicate(mcruns,initial_params,F)
        for(each_run in 1:mcruns){
          model_params$vals[colnames(MC.matrix)]<- MC.matrix[each_run,]
          #updated_initial_params <- replaceDose(initial_params,each_dose,expo_route)
          
          params <- rapidPBPK_initParms(calculateInitialValues(model_params,expo_route,
                                                               each_dose,new_expo_data)$initial_params)
          #params <- replaceDose(params,each_dose,expo_route)
          params_list[[each_run]]<- params
        }
        
        states_list <- replicate(mcruns,state,F)
        times_list <- replicate(mcruns,times,F)
        event_times_list <- replicate(mcruns,event_times,F)
        output_list <- replicate(mcruns,output,F)
        # Run MC simulation in parallel
        cmax_list <- runMCParallel(mcruns,params_list,states_list,output_list,
                                   times_list,event_times_list,updatePB)
        modelMCdata[idx]<- cmax_list[,model_var]*multiplier
        params_list <- NULL
        pb$close()
        }
      update_modal_progress(1,text = "Estimating exposure")
      # perform reverse dosimetry 
      reverse_dosimetry_values <- runReverseDosimetry(modelMCdata,biom_data,percentiles=c(5,10,25,50,75,95,99,100),dose_list = dose_list)
      
      reverse_dosimetry_values$dose_list <- dose_list
      expo_units <- switch(expo_route,
                           "inh"="ppm",
                           "iv"="mg/h",
                           "dw"="mg/L",
                           "dermal"="\U00B5g/cm2/h",
                           "mg/kg/day"
      )
      reverse_dosimetry_values$expo_units <- expo_units
      results$expo <- reverse_dosimetry_values
      results$pbpk <- modelMCdata
      shinybusy::remove_modal_progress()
    }
    # Depending on the type of the workflow selected hide appropriate output tabs before shifting focus
    if(sim_details$sim_type %in% c("fd","mc")){
      if(sim_details$sim_type == "fd"){
        showTab("Modeloutput","nca")
      }else{
        hideTab("Modeloutput","nca")
      }
      hideTab("Modeloutput","cdfpdf")
      hideTab("Modeloutput","percentile")
      showTab("Modeloutput","plots")
      showTab("Modeloutput","params")
    }else{
      
      showTab("Modeloutput","cdfpdf")
      showTab("Modeloutput","percentile")
      hideTab("Modeloutput","plots")
      hideTab("Modeloutput","params")
      hideTab("Modeloutput","nca")
    }
    
    updateNavbarPage(session,"menu","output")
  })

# Life course equation
  tissue_volumes<- reactive({
    tissues <- c(input$ms_cmplist,"blood")
    perfc <- input$ms_perfc
    vols <- getLifecourseTissueVolumes(input$ms_age, input$ms_gender,perfc, tissues)
    vols["bw"] <- getLifecourseBodyWeight(input$ms_age,input$ms_gender)
    return(vols)
  })
  tissue_ratios<- reactive({
    tissues <- c(input$ms_cmplist)
    flows <- getLifecourseTissuePerfusion(input$ms_age, input$ms_gender, tissues)
    flows["qc"]<- getLifecourseCardiacOutput(input$ms_age,input$ms_gender)
    #tissues <- list("fat", "skin", "muscle", "bone", "boneMarow", "brain", "lung", "heart", "gastric", "liver", "kidney")
    return(flows)
  })
#LifeCourse Equation
  observeEvent(input$btn_use_lifecourse,{
    org <- input$ms_org
    if (org == "ha"){
      shinyBS::updateButton(session,"btn_use_lifecourse",style = "primary")
      age <- input$ms_age
      
      gender<- input$ms_gender
      # get volumes from life course equations
      tissues <- c(input$ms_cmplist,"blood")
      perfc <- input$ms_perfc
      vols <- getLifecourseTissueVolumes(age,gender,perfc, tissues)
      vols["bw"] <- getLifecourseBodyWeight(age,gender)
      #update the UI with new volumes
      updateVolumes(session,vols)
      #Get blood flow ratios from life course equations
      tissues <- input$ms_cmplist # since there is no blood flow through blood
      flows <- getLifecourseTissuePerfusion(age,gender, tissues)
      flows["qc"]<- getLifecourseCardiacOutput(age,gender)
      updateRatios(session, flows)
      ventilation_rate <- getLifecourseVentilationRate(age,gender)
      updateNumericInput(session,"ms_respr",value = signif(ventilation_rate,4))
      tidal_volume <- getLifecourseTidalVolume(age,gender)
      updateNumericInput(session,"ms_tv",value = signif(tidal_volume,4))
      ds <- getLifecourseLungDeadSpace(age,gender)
      updateNumericInput(session,"ms_ds",value = signif(ds,4))
      gfr<- getLifecourseGlomerularFiltrationRate(age,gender)
      updateNumericInput(session,"ms_gfr",value = signif(gfr,4))
    }else{
      shinyBS::createAlert(session,"physio_header_alert",style = "error",
                           content = "Only human parameters can be estimated using lifecourse equations")
    }
    })

  # when age and gender are changed, change the type of button to indicate things are out of sync
  observeEvent({input$ms_age ;input$ms_gender; input$ms_cmplist},{
    shinyBS::updateButton(session,"btn_use_lifecourse",style = "warning")
  },ignoreInit = TRUE )

 observeEvent(input$ms_org,{
   if (input$ms_org == "ha"){
     updatePickerInput(session,"sel_qsar4Partition",choices =  c("QSAR model one" = 'one'))
   } else{
     updatePickerInput(session,"sel_qsar4Partition",choices = c("QSAR model one" = 'one',
                                                                "Unified QSAR model" = 'two'))
   } 
 })
  
#Qsar models
  observeEvent(input$btn_useQSAR4Partition,
               {
                 shinyBS::updateButton(session,"btn_useQSAR4partition",style = "primary")
                 chemid <- input$sel_chem4adme
                 qsar_model <- input$sel_qsar4Partition
                 org <- ifelse(input$ms_org=="ha","human","rat")
                 query <- sprintf("SELECT param,value FROM Chemical Where chemid = %i",
                                  as.integer(chemid))
                 ret_data <- projectDbSelect(query)
                 chemical_params <- setNames(ret_data$value,ret_data$param)

                 tissue_list <- list()
                 active_tissues <- input$ms_cmplist
                 active_tissues <- active_tissues[!(active_tissues %in% c("rpf","spf"))]
                 tissue_list$active <- active_tissues
                 tissue_list$spf <- c()
                 tissue_list$rpf <- c()
                 calculatedCoeff <- calculatePartitionCoefficients(qsar_model,chemical_params,tissue_list,org)
                 updateCoeffs(session, calculatedCoeff)
                 updateNumericInput(session,"ms_pair",value = calculatedCoeff$pair)
                 })
  # when chemical and/or model are changed, change the type of button to indicate things are out of sync
  observeEvent({input$sel_chem4partition ;input$sel_qsar4Partition},{
    shinyBS::updateButton(session,"btn_useQSAR4Partition",style = "warning")
  },ignoreInit = TRUE )

#Current Parameters table under Model output
current_params <-  reactive({
    temp <- getAllParamValuesForModel(input$sel_sim,model = model)
    # get exposure paramteres

    
    expo_params <- data.frame("var" = expo_name_df$Name, "val" = temp$vals[expo_name_df$Var],
                              stringsAsFactors = F)
    physio_params <- data.frame("var" = physio_name_df$Name, "val" = temp$vals[physio_name_df$Var],
                              stringsAsFactors = F)
    current_params <- data.frame("var" = chem_name_df$Name,"val" = temp$vals[chem_name_df$Var],stringsAsFactors = F)
    #current_params <- temp$a
    #current_params <- cbind(gsub("ms_", "",temp$b),current_params)
    return(list("cur" = current_params,"expo" = expo_params,"physio" = physio_params))
  })
output$chem_params_tble <- DT::renderDT(DT::datatable(current_params()$cur,
                                                  rownames = F),
                                    colnames=c("Variable names", "Value"))
output$expo_params_tble <- DT::renderDT(DT::datatable(current_params()$expo,
                                                  rownames = F,
                                                  colnames=c("Variable names", "Value"))
                                    )
output$physio_params_tble <- DT::renderDT(DT::datatable(current_params()$physio,
                                                         rownames = F,
                                                        colnames=c("Variable names", "Value")))


  observeEvent(input$btnAddData,{
    addDataSetUI(input$btnAddData,"Generic PBPK")
    dataset$savedat <- callModule(addDataSet,input$btnAddData,"Generic PBPK")
    # conc_datasets <- c("none",getDatasetNames("conc"))
    # updateSelectizeInput(session,"cplt_data",choices = conc_datasets)
  })

  observe({

    if(dataset$savedat()[1]=="Yes"){
      type <- "conc"
      set_list <- getObservationSetChoices(type)
      if(type == "conc"){
        ui_id <- "cplt_data"
      }else{
        ui_id <- "cl_data"
      }

      shinyWidgets::updatePickerInput(session,ui_id,
                           choices = c("No Dataset"="none",set_list),
                           selected = "none")
      dataset$savedat <- reactiveVal(c("No","None"))
      # updateSelectizeInput(session,paste0("sel_scene_",set_type),choices = set_list)
    }
  })

  # Exposure PLots data
  exposureData <- reactive({
    result<- as.data.frame(results$pbpk)
    values <- c()
    legend_names<-c("odose"= "Instantaneous Oral Dose",
                    "totodose"="Total Oral Dose",
                    "ddose"= "Instantaneous Drinking Dose",
                    "totddose"="Total Drinking Dose",
                    "ainh"="Total Inhalation Dose",
                    "InstInhDose"="Instantaneous Inhalation Dose")#,
                    #"ADRM"= "Total Dermal Dose",
                    #"InstDrmDose"= "Instantaneous Dermal Dose"
    #)
    # get exposure values for the simulation just run


    simid <- results$simid
    if(is.null(simid)){

      bdose <- 0
      ddose <- 0
      idose <- 0
    }else{
      query <- sprintf("SELECT expoid FROM SimulationsSet Where simid = %i ;",
                       simid)
      expoid <- projectDbSelect(query)$expoid
      query <- sprintf("Select param,value FROM Exposure WHERE expoid = %i;",
                       expoid)
      ret_data <- projectDbSelect(query)

      expo_data <- setNames(as.character(ret_data$value),
                            ret_data$param)
      bdose <- as.numeric(expo_data['bdose'])
      ddose <- as.numeric(expo_data['drdose'])
      idose <- as.numeric(expo_data['inhdose'])
    }

    if (input$r_expo_type == "act"){
      if (input$ch_dose == TRUE){
        if (bdose >0){
          values<- c("odose",values)
        }else if(ddose > 0){
          values <- c("ddose",values)
        }else if(idose >0){
          values<- c("InstInhDose",values)
        # }else if(drmdlen>0){
        #   values<- c("InstDrmDose",values)
        }

      }
      if(input$ch_totdose == TRUE){
        if (bdose >0){
          values<- c("totodose",values)
        }else if (ddose >0){
          values<- c("totddose",values)
        }else if(idose >0){
          values<- c("ainh",values)
        }
        # }else if(drmdlen>0){
        #   values<- c("adrm",values)
        #}

      }
    }else{
      if (input$ch_dose == TRUE){
        values <- c('odose','ddose','InstInhDose',values)

      }
      if(input$ch_totdose == TRUE){
        values <- c('totodose','totddose','ainh',values)

      }

    }


    if (exists("plot_frame")){
      rm(plot_frame)
    }
    # check if model was ever run
    if (dim(result)[1]==0){
      x<- 1:10
    }else{
      x<- as.integer(result$time)
    }
    plot_frame <- data.frame(time = x)
    #select appropriate variables to plot
    if (dim(result)[1]==0){
      plot_frame["Model Not Yet Run"]<-rep(0,length(x))
    }
    else if(length(values) >0 ){
      for (plt_name in values){
        y<- result[[plt_name]]
        plot_frame[legend_names[plt_name]] <-y
      }
    }else{
      plot_frame["No Data Selected"]<-rep(0,length(x))
    }
    plot_frame <- reshape2::melt(plot_frame,id.vars = "time")
    return(plot_frame)
  })

  # Dataset plotting
  concDataset <- reactive({
    if (input$cplt_data=="none"){
      return(data.frame("time"=c(0),"mean"=c(0),"sd"=c(0)))#data.frame("time"=NULL,"mean"=NULL,"sd"=NULL))
    }else{
      obsid <- input$cplt_data
      query <- sprintf("SELECT units, obs_tble FROM Observation WHERE obsid = %i",
                       as.integer(obsid))
      obs_data <- projectDbSelect(query)
      dataset <- unserialize(charToRaw(obs_data$obs_tble))
      if (ncol(dataset)<3){
        dataset[,"sd"]<- 0
      }

      names(dataset)<- c("time","mean","sd")
      return(dataset)
    }

  })
  concDatasetName <- reactive({
    if (input$cplt_data=="none"){
      return("No Dataset Selected")#data.frame("time"=NULL,"mean"=NULL,"sd"=NULL))
    }else{
      obsid <- input$cplt_data
      query <- sprintf("SELECT name FROM ObservationSet WHERE obsid = %i",
                       as.integer(obsid))
      obs_name <- projectDbSelect(query)
      return(obs_name)
    }
  })
  
  #NCA data processing
  ncaData <- reactive({
    mode <- results$sim_type
    query <- sprintf("Select name,model_var from ResultNames where param_set = 'conc' AND model='%s' AND mode = '%s' AND nca = 'TRUE';",
                     model,
                     mode)
    name_df<- mainDbSelect(query)
    var_names <- name_df$model_var
    param_names <- name_df$name
    validate(need(mode == "fd",message = "MC mode not implemented"))
    result <- results$pbpk
    nca_data<- performPlethemNCA(result,var_names,mode)
    colnames(nca_data)<- paste(param_names,"Concentration",sep = " ")
    return(nca_data)
  })
  
  output$tble_ncavals <- DT::renderDT(DT::datatable(ncaData(),
                                                    rownames = c("Total AUC (\U00B5M.h)",
                                                                 "AUC at infinity (\U00B5M.h)",
                                                                 "AUC in the last 24h (\U00B5M.h)",
                                                                 "Cmax (\U00B5M)",
                                                                 "Time at cmax (h)",
                                                                 "Terminal Half-life (h)",
                                                                 "Terminal Slope"),
                                                    extensions = "Buttons",
                                                    options = list(dom= 'Blfrtip',
                                                                   buttons = c('copy','csv','colvis'),
                                                                   scrollX = TRUE
                                                                   )
                                                    )
                                      )
  

  #  Concentration plot Data
  concData <- reactive({
    result <- results$pbpk
    units <- input$r_cplt_type
    simid <- results$simid
    mode <- results$sim_type
 
    if(is.null(simid)){
      mw <- 1000 # to keep the multiplier as 1

    }else{
      query <- sprintf("SELECT chemid FROM SimulationsSet Where simid = %i ;",
                       simid)
      chemid <- projectDbSelect(query)$chemid
      query <- sprintf("Select value FROM Chemical WHERE chemid = %i AND param = 'mw';",
                       chemid)
      mw <- projectDbSelect(query)$value
    }

    #get value multiplier based on concentration units
    if(units == "um"){
      multiplier <- 1
    }else{
      multiplier <- mw/1000
    }

    result<- as.data.frame(result)
    values <- c()
    
    query <- sprintf("Select model_var,plot_var,name from ResultNames where param_set = 'conc' AND model='%s' AND mode = '%s';",model,mode)
    legend_df <- mainDbSelect(query)
    legend_names <- setNames(legend_df$name,legend_df$model_var)
    var_names <- setNames(legend_df$model_var,legend_df$plot_var)

    plot_vals<- input$cplt_comp
    values <- unlist(lapply(plot_vals,function(x){var_names[x]}))
    names(values)<- NULL

    if (exists("plot_frame")){
      rm(plot_frame)
    }
    # check if model was ever run
    if (dim(result)[1]==0){
      plot_frame<- 1:10
    }else{
      if(mode == "fd"){
        x<- result$time
        plot_frame <- data.frame("time" = result$time,
                                 stringsAsFactors = F)
      }else{
        x <- 1:nrow(result)
        plot_frame <- data.frame("sample" = 1:nrow(result),
                                 stringsAsFactors = F)
      }
    }
   
    # select appropriate variables to plot
    if (dim(result)[1]==0){
      plot_frame["Model Not Yet Run"]<-rep(0,length(x))
    }
    else if(length(values) >0 ){
      for (plt_name in values){
        y<- result[[plt_name]] * multiplier
        plot_frame[[legend_names[plt_name]]] <-y
      }
    }else{
      plot_frame["No Data Selected"]<-rep(0,length(x))
    }
    if (mode == "fd"){
      plot_frame <- reshape2::melt(plot_frame,id.vars = "time")
    }else{
      plot_frame <- reshape2::melt(plot_frame,id.vars = "sample")
    }
    return(plot_frame)

    })
  #Concentration table data
  conc_tble_data <- reactive({
    mode <- results$sim_type
    plt_data<- concData()
    return(reshapePlotData(plt_data,mode))
  })

  amtData <- reactive({
    result <- results$pbpk
    units <- input$r_aplt_type
    simid <- results$simid
    mode <- results$sim_type
    
    if(is.null(simid)){
      mw <- 1000 # to keep the multiplier as 1
      
    }else{
      query <- sprintf("SELECT chemid FROM SimulationsSet Where simid = %i ;",
                       simid)
      chemid <- projectDbSelect(query)$chemid
      query <- sprintf("Select value FROM Chemical WHERE chemid = %i AND param = 'mw';",
                       chemid)
      mw <- projectDbSelect(query)$value
      
    }
    #get value multiplier based on concentration units
    if(units == "um"){
      multiplier <- 1
    }else{
      multiplier <- mw/1000
    }

    values <- c()
    query <- sprintf("Select model_var,plot_var,name from ResultNames where param_set = 'amt' AND model='%s' AND mode = '%s';",model,mode)
    legend_df <- mainDbSelect(query)
    legend_names <- setNames(legend_df$name,legend_df$model_var)
    var_names <- setNames(legend_df$model_var,legend_df$plot_var)
    plot_vals<- input$aplt_comp
    values <- unlist(lapply(plot_vals,function(x){var_names[x]}))
    names(values)<- NULL
    
    if (exists("plot_frame")){
      rm(plot_frame)
    }
    # check if model was ever run
    if (dim(result)[1]==0){
      plot_frame<- 1:10
    }else{
      if(mode == "fd"){
        x<- result$time
        plot_frame <- data.frame("time" = result$time,
                                 stringsAsFactors = F)
      }else{
        x <- 1:nrow(result)
        plot_frame <- data.frame("sample" = 1:nrow(result),
                                 stringsAsFactors = F)
      }
    }
    
    # select appropriate variables to plot
    if (dim(result)[1]==0){
      plot_frame["Model Not Yet Run"]<-rep(0,length(x))
    }
    else if(length(values) >0 ){
      for (plt_name in values){
        y<- result[[plt_name]] *multiplier
        plot_frame[[legend_names[plt_name]]] <-y
      }
    }else{
      plot_frame["No Data Selected"]<-rep(0,length(x))
    }
    if (mode == "fd"){
      plot_frame <- reshape2::melt(plot_frame,id.vars = "time")
    }else{
      plot_frame <- reshape2::melt(plot_frame,id.vars = "sample")
    }
    return(plot_frame)
  })
  
  #Concentration table data
  amt_tble_data <- reactive({
    mode <- results$sim_type
    plt_data<- amtData()
    return(reshapePlotData(plt_data,mode))
  })

  AUCData <- reactive({
    #getAUCPlotData(input,results$pbpk)
  })

  balData<- reactive({
    result<- as.data.frame(results$pbpk)
    # check if model was ever run
    if (dim(result)[1]==0){
      x<- 1:10
    }else{
      x<- result$time
    }
    plot_frame<-data.frame(time = x)
    # select appropriate variables to plot
    if (dim(result)[1]==0){
      plot_frame["Model Not Yet Run"]<-rep(0,length(x))
    }else{
      plot_frame["Mass Balance"]<- result$mbal
    }
    plot_frame <- reshape2::melt(plot_frame,id.vars = "time")

    return(plot_frame)
  })
  
  concplt <- reactive({
    if (results$sim_type == "fd"){
      plotly::plot_ly()%>%
        plotly::add_trace(data = concData(),x = ~time,
                          y = ~value,color = ~variable,
                          type = "scatter",mode = "lines") %>%
        plotly::add_trace(data = concDataset(),x = ~time,y  = ~mean,
                          type = "scatter",mode = "markers",
                          name = concDatasetName(),
                          marker = list(color = "#000"),
                          error_y = list(array= ~sd,
                                         color = '#000')
        )%>%
        plotly::layout(xaxis = list(title = ('Time(h)')),
                       yaxis = list(title = (ifelse(input$r_cplt_type=="um",
                                                    'Concentration (\u00B5M)',
                                                    'Concentration (mg/L)')),
                                    exponentformat = 'e'
                                    )
                       )
      
    }else{
      plotly::plot_ly()%>%
        plotly::add_trace(data = concData(),
                          y = ~value,color = ~variable,
                          type = "box")%>%
        plotly::layout(yaxis = list(title = (ifelse(input$r_cplt_type=="um",
                                                    'Concentration (\U00B5M)',
                                                    'Concentration (mg/L)')),
                                    exponentformat = 'e'
                                    )
                       )
    }
  })
  
  amtplt <- reactive({
    if (results$sim_type == "fd"){
      plotly::plot_ly() %>%
        plotly::add_trace(data = amtData(),x =~time,
                          y= ~value,color = ~variable,
                          type = "scatter",mode="lines")%>%
        plotly::layout(xaxis = list(title = ('Time(h)')),
                       yaxis = list(title = (ifelse(input$r_aplt_type=="um",
                                                    'Amount (\u00B5moles)',
                                                    'Amount (mg)')))
                       )
      # plotly::ggplotly(ggplot(amtData(), aes(x=time,y=value,color = variable))+geom_line()
      #                  +labs(x="Time (h)",y="Amount")
      #                  +theme(axis.text=element_text(size = 15),axis.title=element_text(size = 25),legend.text=element_text(size=15),legend.title=element_blank()))
    }else{
      plotly::plot_ly()%>%
        plotly::add_trace(data = amtData(),
                          y = ~value,color = ~variable,
                          type = "box")%>%
        plotly::layout(yaxis = list(title = (ifelse(input$r_aplt_type=="um",
                                                    'Concentration (\u00B5M)',
                                                    'Concentration (mg/L)'))
        )
        )
    }
  })
  output$concplt <- plotly::renderPlotly(concplt())
  output$exposureplt <- plotly::renderPlotly(plotly::ggplotly(ggplot(exposureData(), aes(x=time,y=value,color = variable))+geom_line()
                                                              +labs(x="Time (h)",y="Amount(umoles)")
                                                              +theme(axis.text=element_text(size = 15),axis.title=element_text(size = 25),legend.text=element_text(size=15),legend.title=element_blank())))


  output$amtplt <- plotly::renderPlotly(amtplt())

  # output$aucplt <- renderPlot(ggplot(AUCData(), aes(x=time,y=value,color = variable))+geom_line()
  #                             +labs(x="Time (h)",y="AUC (mg*h/L)")
  #                             +theme(axis.text=element_text(size = 15),axis.title=element_text(size = 25),legend.text=element_text(size=15),legend.title=element_blank()))

  output$balplt <- renderPlot(ggplot(balData(), aes(x=time,y=value,color = variable))+geom_line()
                              +labs(x="Time (h)",y="Amount (umoles)")
                              #+ylim(-1e-5,1e-5)
                              +theme(axis.text=element_text(size = 15),axis.title=element_text(size = 25),legend.position="none")
                             )
  #data tables
  output$conctble <- DT::renderDT(conc_tble_data())#reshapePlotData(concData()))
  output$expotble <- DT::renderDT(reshapePlotData(exposureData()))
  output$amttble <- DT::renderDT(amt_tble_data())#reshapePlotData(amtData()))
  output$baltble <- DT::renderDT(reshapePlotData(balData()))
  #output$auctble <- renderDataTable(reshapePlotData(AUCData()))

  #Download Plots data Tables
  output$expodwnld <- downloadHandler(
    filename = function(){
      return("expo_data.csv")
      },
    contentType = "text/csv",
    content = function(file) {
      write.csv(reshapePlotData(exposureData()), file)
    }
  )

  output$cdwnld <- downloadHandler(
    filename = function(){
      return("conc_data.csv")
    },
    contentType = "text/csv",
    content = function(file) {
      write.csv(conc_tble_data(), file)
    }
  )

  output$amwnld <- downloadHandler(
    filename = function(){
      return("amt_data.csv")
    },
    contentType = "text/csv",
    content = function(file) {
      write.csv(reshapePlotData(amtData()), file)
    }
  )

  output$aucdwnld <- downloadHandler(
    filename = function(){
      return("auc_data.csv")
    },
    contentType = "text/csv",
    content = function(file) {
      write.csv(reshapePlotData(AUCData()), file)
    }
  )

  output$cmbaldwnld <- downloadHandler(
    filename = function(){
      return("balance_data.csv")
    },
    contentType = "text/csv",
    content = function(file) {
      write.csv(reshapePlotData(results()), file)
    }
  )
  
  output$downloadModel <- downloadHandler(
    filename = function(){
      return("rapidPBPK.model.txt")
    },
    contentType = "text",
    content = function(file){
      file.copy(system.file("rapidPBPK.model",package = "plethem"),file)
    }
  )
  ## CODE CHUCK TO HANDLE OUTPUTS GENERATED BY REVERSE DOSIMETRY AND ROUTE TO ROUTE EXTRAPOLATION
  pdf_data <- reactive({
    return(results$expo$pdf)
  })
  
  
  output$PDF <- renderPlotly({
    p <- plot_ly(
      pdf_data(),
      x = ~dose_list,
      y = ~pdf,
      name = 'PDF',
      type = 'scatter',
      mode = 'lines'#'lines+markers'
    ) %>%
      layout(
        title = 'PDF',
        xaxis = list(
          title = paste0('Exposure (',results$expo$expo_units,")"),
          type = 'log'
        ),
        yaxis = list(
          title = 'Probability'
        )
      )
  })
  
  output$CDF <- renderPlotly({
    p <- plot_ly(
      results$expo$cdf,
      x = ~dose_list,
      y = ~cdf,
      name = 'CDF',
      type = 'scatter',
      mode = 'lines'#'lines+markers'
    ) %>%
      layout(
        title = 'CDF',
        xaxis = list(
          title = paste0('Exposure (',results$expo$expo_units,")"),
          type = 'log'
        ),
        yaxis = list(
          title = 'Cumulative'
        )
      )
  })

  output$expo_estimate<- DT::renderDT({DT::datatable(results$expo$expoEstimates,
                                                     rownames = F,
                                                     colname = c("Percentiles",paste("Exposure (",results$expo$expo_units,")",collapse = "")),
                                                     extensions = "Buttons",
                                                     options = list(dom= 'Blfrtip',
                                                                    buttons = c('copy','csv'),
                                                                    scrollX = TRUE
                                                                    )
                                                     )
    })
  
  # power button to shut down the app
  observeEvent(input$menu,{
    if(input$menu=="stop"){
      shinyWidgets::confirmSweetAlert(session,"close_dialog", "Close PBPK Application?",
                                   "Any changes made to the project since the last save will be lost. Consider saving the project before closing the application.",
                                   type = "question",danger_mode = T)
      updateTabsetPanel(session,"menu","home")

    }else if(input$menu == "save"){
      name <- projectDbSelect(sprintf("Select Name from Project;"))$name
      if(length(name)>0){
        shinyWidgets::confirmSweetAlert(session,"save_dialog", "Save Project",
                                        "Save project in its current state?",type = "question",danger_mode = T)
      }else{
        shinyWidgets::sendSweetAlert(session,"Nothing to save",
                                     text = "No project is currently open",
                                     type = "error")
      }
      
      
      updateTabsetPanel(session,"menu","home")
    }else if(input$menu == "load"){
      shinyWidgets::confirmSweetAlert(session,"load_dialog","Load New Project",
                                      "Load existing project? Unsaved changes to the current project will be lost",
                                      type = "question",danger_mode = T)
      updateTabsetPanel(session,"menu","home")
    }else if(input$menu == "new"){
      if(.Platform$OS.type == "windows"){
        shinyWidgets::inputSweetAlert(session,"new_dialog","Close current project and create a new one?",
                                      "Any changes made to the current project since it was last saved will be lost.",
                                      type= "question",input = "text",
                                      inputPlaceholder = "Project File Name",
                                      btn_labels = c("OK","Cancel"))
      }else{
        shinyWidgets::confirmSweetAlert(session,"new_dialog","Close current project and create a new one?",
                                        "Any changes made to the current project since it was last saved will be lost.",
                                        type = "question")
      }
      
      updateTabsetPanel(session,"menu","home")
    }
    
  })
  observeEvent(input$close_dialog,{
    if (input$close_dialog){
      clearProjectDb()
      query <- "Update Utils Set Value=NULL;"
      mainDbUpdate(query)
      stopApp()
    }
  })
  observeEvent(input$save_dialog,{
    if(input$save_dialog){
      saveProject()
    }
  })
  observeEvent(input$load_dialog,{
    myConfirmation <- input$load_dialog
    if(myConfirmation){ # if user confirmed to load a new project, pop up new modal
      showModal(
        modalDialog(
          tagList(
            shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE),
            textOutput("selectedFile",inline = TRUE)
          ),
          title="Select PLETHEM Project",
          footer = tagList(
            actionButton("loadProjectFile","Load Project"),
            modalButton("Dismiss")
          ), size = c("m"), easyClose = F, fade = T))
    } 
    # else{
    #   print('denied')
    # }
  })
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "files", roots = volumes, session = session, filetypes=c('Rdata'))
  
  observeEvent(input$loadProjectFile,{
    if(is.integer(input$files)){
      sendSweetAlert(session,NULL,"No File Selected",
                                        type = "error")
                         updateTabsetPanel(session,"menu","home")
    } else{
      fpath <- parseFilePaths(volumes, input$files)$datapath
      # fpath2 <<- parseFilePaths(volumes, input$files)
      # output$selectedFile <- renderPrint({fpath})
      loadProject(fpath,runUI = F)
          query <- "Update Utils Set Value=NULL;"
          mainDbUpdate(query)
          js$reset()
    }
    
    
  })
  
  output$selectedFile <- renderPrint({
    if(is.integer(input$files)){
      cat("No file has been selected")
    } else{
      cat(parseFilePaths(volumes, input$files)$name)
      # parseFilePaths(volumes, input$files)$datapath[[1]]
    }
  })
  observeEvent(input$new_dialog,{
    if(.Platform$OS.type == "windows"){
      name <- input$new_dialog
      if(name == ""){
        sendSweetAlert(session,NULL,"No file name given",
                       type = "error")
        updateTabsetPanel(session,"menu","home")
      }else{
        path <- getFileFolderPath("dir",
                                  caption =sprintf("Select folder where %s will be saved",name),
                                  )
        if(is.na(path)){
          sendSweetAlert(session,NULL,"No folder selected",
                         type = "error")
          updateTabsetPanel(session,"menu","home")
        }else{
          newProject(name,path)
          query <- "Update Utils Set Value=NULL;"
          mainDbUpdate(query)
          
          js$reset() 
        }
      }
      
    }else{
      if(input$new_dialog){
        path <- getFileFolderPath("file",new_flag = T)
        if(is.na(path)){
          sendSweetAlert(session,NULL,"No folder selected",
                         type = "error")
          updateTabsetPanel(session,"menu","home")
        }else{
          name <- basename(path)
          path <- dirname(path)
          newProject(name,path)
          newProject(name,path)
          query <- "Update Utils Set Value=NULL;"
          mainDbUpdate(query)
          js$reset() 
        }
      }
      
    }
    
  })
  Sys.sleep(1)
  remove_modal_spinner(session)
  
})

calculateInitialValues <- function(params_list,route=NULL,dose=NULL,new_expo_data=NULL){
  params <- params_list$vals
  brep_flag <- as.logical(params[["brep_flag"]])
  brepv_flag <- as.logical(params[["brepv_flag"]])
  iv_flag <- as.logical(params[["ivrep_flag"]])
  derm_flag <- as.logical(params[["dermrep_flag"]])
  params <- params[which(grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",params))]
  params <- lapply(params,function(x){as.numeric(x)})
  
  # check if dose needs to be replaced. it would need to be replaced if route is given
  if(!(is.null(route))){
    if(!(is.null(new_expo_data))){
      var_names <- new_expo_data$param
      
      var_values <- unlist(lapply(new_expo_data$value,as.numeric))
      params[var_names]<- var_values
    }
    
    #params[new_expo_data$param]<- new_expo_data$value
    # get the appropriate dos variable based on the route
    dose_var <- switch(route,
                       "oral"="bdose",
                       "oralv"="bdosev",
                       "dw"="drdose",
                       "inh"="inhdose",
                       "iv"="ivdose",
                       "dermal"="dermrate"
    )
    #Replace the correct dose value based on the dosing route current active
    params[[dose_var]]<- dose
  }

  initial_params <- within(as.list(params),{
    #total fractional volume
    # this is used to correctly scale the volume if 
    # since the distributions from the MC analysis can cause 
    # fractional volume to go above 1
    total_vol <- vfatc+vskinc+vmuscc+vbonec+vbrnc+vlngc+vhrtc+vgic+vlivc+vkdnc+vrpfc+vspfc+vbldc
    #Scaled Tissue Volumes
    vbld <- vbldc*(perfc/total_vol)*bw     #L;Blood
    vpls <- vbld*(1-hct)
    vfat <- vfatc*(perfc/total_vol)*bw
    vskin <- vskinc*(perfc/total_vol)*bw
    vmusc <- vmuscc*(perfc/total_vol)*bw
    vbone <- vbonec*(perfc/total_vol)*bw
    vbrn <- vbrnc*(perfc/total_vol)*bw
    vlng <- vlngc*(perfc/total_vol)*bw
    vhrt <- vhrtc*(perfc/total_vol)*bw
    vkdn <- vkdnc*(perfc/total_vol)*bw
    vgi <- vgic*(perfc/total_vol)*bw
    vliv <- vlivc*(perfc/total_vol)*bw
    vrpf <- vrpfc*(perfc/total_vol)*bw
    vspf <- vspfc*(perfc/total_vol)*bw
    vdmet <- vdmetc*bw #volume of distribution for the metabolite

    #Total Fractional Perfusion
    total_perf <- qfatc+qskinc+qmuscc+qbonec+qbrnc+qlngc+qhrtc+qkdnc+qvlivc+qrpfc+qspfc  # This does not include flow to GI since that is a part of liver venous flow

    #Scaled Perfusion
    qcp <- qcc*(1-hct)
    qfat <- qfatc*(1/total_perf)*qcp
    qskin <- qskinc*(1/total_perf)*qcp
    qmusc <- qmuscc*(1/total_perf)*qcp
    qbone <- qbonec*(1/total_perf)*qcp
    qbrn <- qbrnc*(1/total_perf)*qcp
    qlng <- qlngc*(1/total_perf)*qcp
    qhrt <- qhrtc*(1/total_perf)*qcp
    qkdn <- qkdnc*(1/total_perf)*qcp
    qvliv <- qvlivc*(1/total_perf)*qcp
    qgi <- (qgic/(qgic+qalivc))*qvliv
    qaliv <- (qalivc/(qgic+qalivc))*qvliv
    qrpf <- qrpfc*(1/total_perf)*qcp
    qspf <- qspfc*(1/total_perf)*qcp

    #Scaled tissue permeability coefs
    pafat <- pafat*vfat**0.75
    paskin <- paskin*vskin**0.75
    pamusc <- pamusc*vmusc**0.75
    pabone <- pabone*vbone**0.75
    pabrn <- pabrn*vbrn**0.75
    palng <- palng*vlng**0.75
    pahrt <- pahrt*vhrt**0.75
    pakdn <- pakdn*vkdn**0.75
    pagi <- pagi*vgi**0.75
    paliv <- paliv*vliv**0.75
    parpf <- parpf*vrpf**0.75
    paspf <- paspf*vspf**0.75

    vkm1 <- vkm1c*vliv
    vmaxliv <- vmaxc*bw**0.75

    tstop <- tstart+sim_dur

    cinh <- (inhdose/24.45) # converting from  ppm to umoles/L
    # ppm * mw /24.45 => mg/m^3 => 1000 *mg/L
    # mg/L => mw/1000 umoles/L => ppm/24.45
    
    qalv <- (tv-ds)*respr
    pair <- ifelse(pair >0,pair,1E-10)
    # scaled urinary flow rate per day
    uflw <- uflwc*bw/24.0
  })
  
  

  #function for dosing

  mw <- initial_params[["mw"]]
  bw <- initial_params[["bw"]]
  #ORAL
  bdose <- initial_params[["bdose"]]
  breps <- initial_params[["breps"]]
  blen <- initial_params[["blen"]]

  totbreps <- initial_params[["totbreps"]]<-breps*blen
  #Drinking Water
  ddose <- initial_params[["drdose"]]
  vdw <- initial_params[["vdw"]]
  dreps <- initial_params[["dreps"]]
  
  #ORAL  with vehicle
  bdosev <- initial_params[["bdosev"]]
  brepsv <- initial_params[["brepsv"]]
  blenv <- initial_params[["blenv"]]
  
  totbrepsv <- initial_params[["totbrepsv"]]<-brepsv*blenv

  #inhalation
  inhdose <- initial_params[["inhdose"]]
  inhtlen <- initial_params[["inhtlen"]]
  inhdays <- initial_params[["inhdays"]]

  #iv
  ivdose <- initial_params[["ivdose"]]
  ivlen <- initial_params[["ivlen"]]
  
  #dermal
  dermrate <- initial_params[["dermrate"]]
  dermlen <- initial_params[["dermlen"]]
  skarea <- initial_params[["skarea"]]

  #simulation
  tstart <- initial_params[["tstart"]]
  totdays <- initial_params[["totdays"]]
  tstop <- initial_params[["tstop"]]
  #if bolus oral dose is administered
  if (bdose > 0){
    # var to change
    state_Var <- c("odose","totodose")

    # operation of event
    operation <- c("add","add")
    # times of event
    if (breps==1){
      # Value  of change
      change_val1<- (bdose*bw*1000/mw)
      change_val2<- change_val1
      change_arr <- c(change_val1,change_val2)
      #only one bolus dose per day
      if (brep_flag){
        event_times <- head(seq(tstart,tstop,24),-1)
      }else{
        event_times <- c(tstart)
      }

    }else{
      # Value  of change
      change_val1<- (bdose*bw*1000/mw)/totbreps
      change_val2<- change_val1
      change_arr <- c(change_val1,change_val2)
      #multiple bolus doses per day
      if (brep_flag){
        event_times <- unlist(lapply(X = 1:totdays,
                                     FUN = function(x){
                                       head(seq(0,blen,1/breps),-1)+(24*(x-1))
                                       }
                                     )
                              )
      }else{
        #only one day
        event_times <- unlist(lapply(X = 1,
                                     FUN = function(x){
                                       head(seq(0,blen,1/breps),-1)+(24*(x-1))
                                     }
        )
        )
      }

    }

    eventDat <- data.frame(

      var = rep(x = state_Var,each = length(event_times)),
      time = rep(event_times,length(state_Var)),
      value = rep(x = change_arr,each = length(event_times)),
      method = rep(x = operation,each = length(event_times))

    )

    # if drinking water dose is administered
  }else if (ddose >0){
    # var to change
    state_Var <- c("ddose","totddose")
    # Value  of change
    change_val1 <- (ddose*1000*vdw/mw)/dreps
    change_val2 <- change_val1
    change_arr <- c(change_val1,change_val2)
    # operation of event
    operation <- c("add","add")
    # times of event
    event_times <- unlist(lapply(X = 1:totdays,function(x){head(seq(0,24,by = 24/dreps),-1)+24*(x-1)}))

    eventDat <- data.frame(

      var = rep(x = state_Var,each = length(event_times)),
      time = rep(event_times,length(state_Var)),
      value = rep(x = change_arr,each = length(event_times)),
      method = rep(x = operation,each = length(event_times))

    )
    # if inhalation dose is administered
  }else if(bdosev > 0){
    # var to change
    state_Var <- c("odosev","totodosev")
    
    # operation of event
    operation <- c("add","add")
    # times of event
    if (brepsv==1){
      # Value  of change
      change_val1<- (bdosev*bw*1000/mw)
      change_val2<- change_val1
      change_arr <- c(change_val1,change_val2)
      #only one bolus dose per day
      if (brepv_flag){
        event_times <- head(seq(tstart,tstop,24),-1)
      }else{
        event_times <- c(tstart)
      }
      
    }else{
      # Value  of change
      change_val1<- (bdosev*bw*1000/mw)/totbrepsv
      change_val2<- change_val1
      change_arr <- c(change_val1,change_val2)
      #multiple bolus doses per day
      if (brepv_flag){
        event_times <- unlist(lapply(X = 1:totdays,
                                     FUN = function(x){
                                       head(seq(0,blenv,1/brepsv),-1)+(24*(x-1))
                                     }
        )
        )
      }else{
        #only one day
        event_times <- unlist(lapply(X = 1,
                                     FUN = function(x){
                                       head(seq(0,blenv,1/brepsv),-1)+(24*(x-1))
                                     }
        )
        )
      }
      
    }
    
    eventDat <- data.frame(
      
      var = rep(x = state_Var,each = length(event_times)),
      time = rep(event_times,length(state_Var)),
      value = rep(x = change_arr,each = length(event_times)),
      method = rep(x = operation,each = length(event_times))
      
    )
    
  }else if (inhdose >0){
    # var to change
    state_var1 <- "inhswch"
    state_var2 <- "inhswch"
    # Value  of change
    change_val1 <- 1
    change_val2 <- 0
    # operation of event
    operation1 <- "rep"
    operation2 <- "rep"
    # times of event

    #days on which dosing can occue
    event_days<- unlist(lapply(X=1:totdays,function(x){lapply(1:inhdays,function(y){(x-1)*7+y})}))

    event_times1 <- unlist(lapply(event_days,function(x){0+24*(x-1)}))
    event_times1 <- event_times1[event_times1 < tstop]
    event_times2 <- unlist(lapply(event_days,function(x){inhtlen+24*(x-1)}))
    event_times2 <- event_times2[event_times2 < tstop]
    eventDat <- data.frame(
      var = c(rep(x = state_var1,each = length(event_times1)),rep(x = state_var2,each = length(event_times2))),
      time = c(event_times1,event_times2),
      value = c(rep(x = change_val1,each = length(event_times1)),rep(x = change_val2,each = length(event_times2))),
      method = c(rep(x = operation1,each = length(event_times1)),rep(x = operation2,each = length(event_times2)))
    )
  }else if (ivdose >0){
    # var to change
    state_var1 <- "ivswch"
    state_var2 <- "ivswch"
    # Value  of change
    change_val1 <- 1
    change_val2 <- 0
    # operation of event
    operation1 <- "rep"
    operation2 <- "rep"
    # times of event

    #days on which dosing can occue
    #event_days = unlist(lapply(X=1:7,function(x){lapply(1:inhdays,function(y){(x-1)*7+y})}))
    event_days <- unlist(lapply(X=1:totdays,function(x){lapply(1:7,function(y){(x-1)*7+y})}))
    event_times1 <- unlist(lapply(event_days,function(x){0+24*(x-1)}))
    event_times1 <- event_times1[event_times1 < tstop]
    event_times2 <- unlist(lapply(event_days,function(x){ivlen+24*(x-1)}))
    event_times2 <- event_times2[event_times2 < tstop]
    eventDat <- data.frame(
      var = c(rep(x = state_var1,each = length(event_times1)),rep(x = state_var2,each = length(event_times2))),
      time = c(event_times1,event_times2),
      value = c(rep(x = change_val1,each = length(event_times1)),rep(x = change_val2,each = length(event_times2))),
      method = c(rep(x = operation1,each = length(event_times1)),rep(x = operation2,each = length(event_times2)))
    )
  }
  else if(dermlen >0){
    # var to change
    state_var1 <- "drmswch"
    state_var2 <- "drmswch"
    # Value  of change
    change_val1 <- 1
    change_val2 <- 0
    # operation of event
    operation1 <- "rep"
    operation2 <- "rep"
    event_days <- 1:totdays
    #event_days<- unlist(lapply(X=1:totdays,function(x){lapply(1:7,function(y){(x-1)*7+y})}))
    # if (derm_flag){
    #   # times of event
    #   event_days<- unlist(lapply(X=1:totdays,function(x){lapply(1:7,function(y){(x-1)*7+y})}))
    # }else{
    #   # times of event
    #   event_days<- unlist(lapply(X=1:totdays,function(x){lapply(1:2,function(y){(x-1)*7+y})}))
    # }

    event_times1 <- unlist(lapply(event_days,function(x){0+24*(x-1)}))
    event_times1 <- event_times1[event_times1 < tstop]
    event_times2 <- unlist(lapply(event_days,function(x){dermlen+24*(x-1)}))
    event_times2 <- event_times2[event_times2 < tstop]
    print(event_times1)
    eventDat <- data.frame(
      var = c(rep(x = state_var1,each = length(event_times1)),rep(x = state_var2,each = length(event_times2))),
      time = c(event_times1,event_times2),
      value = c(rep(x = change_val1,each = length(event_times1)),rep(x = change_val2,each = length(event_times2))),
      method = c(rep(x = operation1,each = length(event_times1)),rep(x = operation2,each = length(event_times2)))
    )
    
  }

  times <- seq(tstart,tstop,by=0.1)
  eventDat <- eventDat[order(eventDat$time),]

  state <- c(inhswch = 0.0,ainh = 0.0,aexh = 0.0,totodose = 0.0,
             odose = 0.0,totddose = 0.0,ddose = 0.0,odosev = 0.0,
             totodosev = 0.0,alas = 0.0,akent = 0.0,afec = 0.0,
             aabsgut = 0.0,ivswch = 0.0,aiv = 0.0,dermswch = 0.0,
             aderm = 0.0,adermabs = 0.0,adermevap = 0.0,abld = 0.0,
             abfat = 0.0,atfat = 0.0, abskin = 0.0,asc = 0.0,
             ascMgcm2 = 0.0,atskin = 0.0,abmusc = 0.0,atmusc = 0.0,
             abbone = 0.0,atbone = 0.0,abbrn = 0.0,atbrn = 0.0,
             ablng = 0.0,atlng = 0.0,abhrt = 0.0,athrt = 0.0,
             abgi = 0.0,atgi = 0.0,abliv = 0.0,atliv = 0.0,
             abkdn = 0.0,atkdn = 0.0,abrpf = 0.0,atrpf = 0.0,
             abspf = 0.0,atspf = 0.0,ametliv1 = 0.0,
             ametliv2 = 0.0,aclbld = 0.0,auexc = 0.0,
             anabsgut = 0.0,auexcmet = 0.0,
             amet = 0.0,vurine = 1e-10)

  initial_values <- list("evnt_data"= eventDat,
                         "initial_params"= initial_params[params_list$names],
                         "times"=times,
                         "tstop"=tstop,"tstart"=tstart,
                         "state"= state)

  return(initial_values)
}

rapidPBPK_initParms <- function(newParms = NULL) {
  parms <- c(
    mw = 0,
    bdose = 0,
    blen = 0,
    breps = 0,
    totbreps = 0,
    drdose = 0,
    vdw = 0,
    dreps = 0,
    inhdose = 0,
    inhtlen = 0,
    inhdays = 0,
    ivdose = 0,
    ivlen = 0,
    dermrate = 0,
    KPtot = 0,
    Kevap = 0,
    maxcap = 0,
    wsol = 0,
    skarea = 0,
    bdosev = 0,
    blenv = 0,
    brepsv = 0,
    totbrepsv = 0,
    kfec = 0,
    kVtoL = 0,
    kent = 0,
    bw = 0,
    qcc = 0,
    hct = 0,
    vbldc = 0,
    perfc = 0,
    kbld = 0,
    respr = 0,
    tv = 0,
    ds = 0,
    uflw = 0,
    gfr = 0,
    frwsol = 0,
    fatvtbc = 0,
    vfatc = 0,
    qfatc = 0,
    pfat = 0,
    skinvtbc = 0,
    vskinc = 0,
    qskinc = 0,
    pskin = 0,
    muscvtbc = 0,
    vmuscc = 0,
    qmuscc = 0,
    pmusc = 0,
    bonevtbc = 0,
    vbonec = 0,
    qbonec = 0,
    pbone = 0,
    brnvtbc = 0,
    vbrnc = 0,
    qbrnc = 0,
    pbrn = 0,
    lngvtbc = 0,
    vlngc = 0,
    qlngc = 0,
    plng = 0,
    hrtvtbc = 0,
    vhrtc = 0,
    qhrtc = 0,
    phrt = 0,
    givtbc = 0,
    vgic = 0,
    qgic = 0,
    pgi = 0,
    fa = 0,
    ka = 0,
    livvtbc = 0,
    vlivc = 0,
    qalivc = 0,
    qvlivc = 0,
    pliv = 0,
    kdnvtbc = 0,
    vkdnc = 0,
    qkdnc = 0,
    pkdn = 0,
    rpfvtbc = 0,
    vrpfc = 0,
    qrpfc = 0,
    prpf = 0,
    spfvtbc = 0,
    vspfc = 0,
    qspfc = 0,
    pspf = 0,
    res = 0,
    fupls = 0,
    vbld = 0,
    vpls = 0,
    vfat = 0,
    vskin = 0,
    vmusc = 0,
    vbone = 0,
    vbrn = 0,
    vlng = 0,
    vhrt = 0,
    vkdn = 0,
    vgi = 0,
    vliv = 0,
    vrpf = 0,
    vspf = 0,
    total_perf = 0,
    qcp = 0,
    qfat = 0,
    qskin = 0,
    qmusc = 0,
    qbone = 0,
    qbrn = 0,
    qlng = 0,
    qhrt = 0,
    qkdn = 0,
    qvliv = 0,
    qgi = 0,
    qaliv = 0,
    qrpf = 0,
    qspf = 0,
    pafat = 0,
    paskin = 0,
    pamusc = 0,
    pabone = 0,
    pabrn = 0,
    palng = 0,
    pahrt = 0,
    pakdn = 0,
    pagi = 0,
    paliv = 0,
    parpf = 0,
    paspf = 0,
    vkm1 = 0,
    vmaxliv = 0,
    km = 0,
    cinh = 0,
    qalv = 0,
    pair = 1e10,
    fuplsmet = 1,
    vdmet = 1e-10
  )
  
  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      print(names(newParms)[!(names(newParms))%in% c(names(parms))])
      stop("illegal parameter name")
    }
    parms[names(newParms)] <- newParms
  }
  
  parms <- within(as.list(parms), {
  })
  out <- .C("getParms",  as.double(parms),
            out=double(length(parms)),
            as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

rapidPBPK_Outputs <- c(
  "abone",
  "abrn",
  "alng",
  "ahrt",
  "akdn",
  "agi",
  "aliv",
  "arpf",
  "aspf",
  "afat",
  "askin",
  "amusc",
  "cpls",
  "cv",
  "cfat_um",
  "ctfat",
  "cbfat",
  "cskin_um",
  "ctskin",
  "cbskin",
  "cmusc_um",
  "ctmusc",
  "cbmusc",
  "cbone_um",
  "ctbone",
  "cbbone",
  "cbrn_um",
  "ctbrn",
  "cbbrn",
  "clng_um",
  "ctlng",
  "cblng",
  "chrt_um",
  "cthrt",
  "cbhrt",
  "ckdn_um",
  "ctkdn",
  "cbkdn",
  "cgi_um",
  "ctgi",
  "cbgi",
  "cliv_um",
  "ctliv",
  "cbliv",
  "crpf_um",
  "ctrpf",
  "cbrpf",
  "cspf_um",
  "ctspf",
  "cbspf",
  "InstInhDose",
  "InstDermDose",
  "mbal",
  "curine",
  "curinemet"
)

rapidPBPK_initStates <- function(parms, newStates = NULL) {
  Y <- c(
    inhswch = 0.0,
    ainh = 0.0,
    aexh = 0.0,
    totodose = 0.0,
    odose = 0.0,
    totddose = 0.0,
    ddose = 0.0,
    odosev = 0.0,
    totodosev = 0.0,
    alas = 0.0,
    akent = 0.0,
    afec = 0.0,
    aabsgut = 0.0,
    ivswch = 0.0,
    aiv = 0.0,
    dermswch = 0.0,
    aderm = 0.0,
    adermabs = 0.0,
    adermevap = 0.0,
    abld = 0.0,
    abfat = 0.0,
    atfat = 0.0,
    abskin = 0.0,
    asc = 0.0,
    ascMgcm2 = 0.0,
    atskin = 0.0,
    abmusc = 0.0,
    atmusc = 0.0,
    abbone = 0.0,
    atbone = 0.0,
    abbrn = 0.0,
    atbrn = 0.0,
    ablng = 0.0,
    atlng = 0.0,
    abhrt = 0.0,
    athrt = 0.0,
    abgi = 0.0,
    atgi = 0.0,
    abliv = 0.0,
    atliv = 0.0,
    abkdn = 0.0,
    atkdn = 0.0,
    abrpf = 0.0,
    atrpf = 0.0,
    abspf = 0.0,
    atspf = 0.0,
    ametliv1 = 0.0,
    ametliv2 = 0.0,
    aclbld = 0.0,
    auexc = 0.0,
    anabsgut = 0.0,
    auexcmet = 0.0,
    amet = 0.0,
    vurine = 1e-10
  )
  
  if (!is.null(newStates)) {
    if (!all(names(newStates) %in% c(names(Y)))) {
      stop("illegal state variable name in newStates")
    }
    Y[names(newStates)] <- newStates
  }
  
  .C("initState", as.double(Y));
  Y
}

#Update Volume ratios
updateVolumes <- function(session, tissue_volumes){
  input_ids <- c("fat"="ms_vfatc","skin"="ms_vskinc",
                 "muscle"="ms_vmuscc","bone"="ms_vbonec",
                 "brain"="ms_vbrnc","lung"="ms_vlngc",
                 "heart"="ms_vhrtc","gi"="ms_vgic",
                 "liver"="ms_vlivc","kidney"="ms_vkdnc",
                 "rpf"="ms_vrpfc","spf"="ms_vspfc","blood"="ms_vbldc",
                 "bw"="ms_bw")
  tissue_volumes <- isolate(tissue_volumes)
  volumes <- tissue_volumes
  names(volumes)<- lapply(names(tissue_volumes),function(x){input_ids[x]})
  for(elem in names(volumes)){
    if(elem!="ms_bw"){
      volumes[[elem]]<- volumes[[elem]]/(volumes[["ms_bw"]])
    }
    updateNumericInput(session, elem, value = signif(volumes[[elem]],4))
  }
}

#Update tissues Blood flow ratio
updateRatios <- function(session, tissue_ratios){
  tissue_ratios <- isolate(tissue_ratios)
  input_ids <- c("fat"="ms_qfatc","skin"="ms_qskinc",
                 "muscle"="ms_qmuscc","bone"="ms_qbonec",
                 "brain"="ms_qbrnc","lung"="ms_qlngc",
                 "heart"="ms_qhrtc","gi"="ms_qgic",
                 "liver_art"="ms_qalivc","liver_ven"="ms_qvlivc",
                 "kidney"="ms_qkdnc","rpf"="ms_qrpfc","spf"="ms_qspfc",
                 "qc"="ms_qcc")
  ratios <- tissue_ratios
  names(ratios)<- lapply(names(tissue_ratios),function(x){input_ids[x]})
  for(elem in names(ratios)){
    if(elem!="ms_qcc"){
      ratios[[elem]]<- ratios[[elem]]/ratios[["ms_qcc"]]
    }
    updateNumericInput(session, elem, value = signif(ratios[[elem]],4))
  }
}


#Update tissue coefficient when Qsar is being used
updateCoeffs <- function(session, calculatedCoeff){
  names(calculatedCoeff) <- paste("ms_", names(calculatedCoeff), sep = "")

  for(elem in names(calculatedCoeff)){
    updateNumericInput(session, elem, value = calculatedCoeff[[elem]])
  }
}

runMCParallel <- function(mcruns,params_list,states_list,output_list,times_list,event_times_list,progressFunc){
  c1 <- makeCluster(parallel::detectCores()-2, setup_timeout = 0.5)
  registerDoParallel(c1)
  opts <- list(progress = progressFunc)
  cmax_list <- foreach(idx=seq_len(mcruns),params_list,
                       states_list,times_list,
                       event_times_list,output_list,
                       .combine = 'rbind',.inorder = F,
                       .options.snow = opts,
                       .packages = c("deSolve"))%dopar%{
                         params <- params_list[[idx]]
                         state <- states_list[[idx]]
                         times <- times_list[[idx]]
                         event_times <- event_times_list[[idx]]
                         output_var <- output_list[[idx]]
                         # for development, load the dll directly
                         #dyn.load("../../src/plethem.dll")
                         dyn.load(system.file("libs",.Platform$r_arch,paste0("plethem",.Platform$dynlib.ext),package = "plethem"))
                         modelOutput<- deSolve::ode(y = state, times = times,
                                                    method = "lsodes",func = "derivs",
                                                    dllname = "plethem",
                                                    initfunc= "initmod",
                                                    parms = params,
                                                    events=list(func="event",
                                                                time=event_times),
                                                    nout = length(output_var),
                                                    outnames = output_var)
                         #dyn.unload("../../src/plethem.dll")
                         dyn.unload(system.file("libs",.Platform$r_arch,paste0("plethem",.Platform$dynlib.ext),package = "plethem"))
                         modelOutput <- as.data.frame(modelOutput)
                         max_vals <- sapply(modelOutput,max,na.rm = T)
                         return(max_vals)
                       }
  stopCluster(c1)
  return(as.data.frame(cmax_list))
}

### MODULES
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
  modalType  <- "Import New Biomonitoring Data"
  showModal(modalDialog(title = modalType,
                        useSweetAlert(),
                        fluidPage(
                          fluidRow(
                            column(6,
                                   fileInput(ns("btn_import_file"),
                                             "Upload Biomonitoring Data",
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
                                                            selectizeInput(ns("sel_r2rExpo"),"Template Exposure Set",
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
                                                                            "Exposure Range",
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
                   sep = " ",collapse = " ")
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
                     sep=" ",collapse = " ")
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
      query <- sprintf("Update SimulationsSet SET chemvarid = %i, physiovarid = %i, admevarid = %i, extrapolateid = %i, mcruns = %i, num_expos = %i,low_dose_estimate = %f,high_dose_estimate = %f,expovarid = 0 where simid = %i;",
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

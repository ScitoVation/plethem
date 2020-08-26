library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(V8)
library(ggplot2)
library(shinyjs)
library(magrittr)

shinyServer(function(input, output, session) {
  shinyjs::useShinyjs()
  # define the model name once here. It will be used throughout this server file
  # this will make it easier to create new model UI/SERVERS
  model <- "fishPBPK"
  
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
  sim_set <- getAllSetChoices("sim")
  physiovar <-getVariabilitySetChoices("physio")
  chemvar <-getVariabilitySetChoices("chem")
  expovar <-getVariabilitySetChoices("expo")

  parameterSets$expo <- reactiveVal(expo_set)
  parameterSets$physio <- reactiveVal(physio_set)
  parameterSets$chem <- reactiveVal(chem_set)
  parameterSets$sim <- reactiveVal(sim_set)
  parameterSets$physiovar <- reactiveVal(physiovar)
  parameterSets$chemvar <- reactiveVal(chemvar)
  parameterSets$expovar <- reactiveVal(expovar)

  observe({
    exposet <- parameterSets$expo()
    updateSelectizeInput(session,"sel_set_expo",choices = exposet)
    physioset <- parameterSets$physio()
    updateSelectizeInput(session,"sel_set_physio",choices = physioset)
    chemset <- parameterSets$chem()
    updateSelectizeInput(session,"sel_set_chem",choices = chemset)
    physiovar <- parameterSets$physiovar()
    physiovar <- c("None"="0",physiovar)
    updateSelectizeInput(session,"sel_set_physiovar",choices = physiovar)
    chemvar <- parameterSets$chemvar()
    chemvar <- c("None"="0",chemvar)
    updateSelectizeInput(session,"sel_set_chemvar",choices = chemvar)
    expovar <- parameterSets$expovar()
    expovar <- c("None"="0",expovar)
    updateSelectizeInput(session,"sel_set_expovar",choices = expovar)
  })
  # get global variables needed to run the model

  # get the parameter table for physiological and exposure variables.
  query <- sprintf("SELECT Name,Var,Units,ParamType,Variability FROM ParamNames Where Model='%s' AND ParamSet = 'Physiological' AND UIParams = 'TRUE';",
                   model)
  physio_name_df <- mainDbSelect(query)

  query <- sprintf("SELECT Name,Var,Units,ParamType,Variability FROM ParamNames Where Model='%s' AND ParamSet = 'Exposure' AND UIParams = 'TRUE';",
                   model)
  expo_name_df <- mainDbSelect(query)

  query <- sprintf("SELECT Name,Var,Units,ParamType,Variability FROM ParamNames Where Model='%s' AND ParamSet = 'Chemical'AND UIParams = 'TRUE' ;",
                   model)
  chem_name_df <- mainDbSelect(query)

  #### Update the parameter set dropdowns if they exist for physiological and exposure sets
  set_choices <- getAllSetChoices(set_type = "physio")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_physio",choices = set_choices)
    shinyBS::updateButton(session,"btn_use_lifecourse",style = "primary")
    shinyBS::updateButton(session,"btn_useQSAR4Partition",style = "primary")
  }
  set_choices <- getAllSetChoices(set_type = "expo")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_expo",choices = set_choices)
  }
  set_choices <- getAllSetChoices(set_type = "chem")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_chem",choices = set_choices)
    updateSelectizeInput(session,"sel_chem4Partition",choices = set_choices)
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


  compartment_list <-c("fat","liver","kidney","rpf","spf")
  vol_ids <- c("fat"="ms_vfatc",
               "liver"="ms_vlivc","kidney"="ms_vkdnc",
               "rpf"="ms_vrpfc","spf"="ms_vspfc",
               "bw"="ms_bw")
  flow_ids <- c("fat"="ms_qfatc","liver"="ms_qlivc",
                "kidney"="ms_qkdnc",
                "rpf"="ms_qrpfc","spf"="ms_qspfc")
  

  


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
    }
  })
  
  ### Import button current for chemicals only
  # Import a new chemical set from user or main database
   #### Chunk for handling chemical tab
   observeEvent(input$btn_import_chem,{
     
     importParameterSetUI(paste0("chem",input$btn_import_chem),"chem")
     parameterSets$importdat <- callModule(importParameterSet,paste0("chem",input$btn_import_chem),"chem")

   })
   #### Chunk for handling physiological tab
   observeEvent(input$btn_import_physio,{
     importParameterSetUI(input$btn_import_physio,"physio")
     parameterSets$importdat <- callModule(importParameterSet,input$btn_import_physio,"physio")
     
   })


   # update the paramter set dropdown if it is changed
   observe({
     result_vector <- parameterSets$importdat
     if(result_vector()[1]=="Yes"){
       set_type <- result_vector()[2]
       set_id <- result_vector()[3]
       set_list <- getAllSetChoices(set_type)
       parameterSets[[set_type]] <- reactiveVal(set_list)
       updateSelectizeInput(session,paste0("sel_",set_type),choices = set_list, selected = set_id)
       
       
       # updateSelectizeInput(session,paste0("sel_scene_",set_type),choices = set_list)
     }
   })

  #Save a new physiological parameter set
  observeEvent(input$btn_saveas_physio,{
    active_comp <- input$ms_cmplist
    compartment_list <-c("fat","liver","kidney","rpf","spf")
    inactive_comp <- setdiff(compartment_list,active_comp)
    vol_comps <- compartment_list
    perfc <- 1
    total_vol <- sum(unlist(lapply(vol_comps,function(x){input[[vol_ids[x]]]})))
    
    if(abs(total_vol-perfc)>0.03){

      error_text <- sprintf("The total volume of all compartments does not add up to %i %%",
              as.integer(perfc*100))

      shinyWidgets::sendSweetAlert(session,
                                   title = "Invalid Compartment Configuration",
                                   text = error_text,
                                   type = "error")

    }else{
      saveAsParameterSetUI(input$btn_saveas_physio,"physio")
      parameterSets$savedat <- callModule(saveAsParameterSet,
                                          input$btn_saveas_physio,
                                          "physio",isolate(input),
                                          physio_name_df)
    }

  })

  #Save a new exposure parameter set
  observeEvent(input$btn_saveas_expo,{
    
      saveAsParameterSetUI(input$btn_saveas_expo,"expo")
      parameterSets$savedat <- callModule(saveAsParameterSet,
                                          input$btn_saveas_expo,
                                          "expo",isolate(input),
                                          expo_name_df)
    

  })

  #Save a new chemical parameter set
  observeEvent(input$btn_saveas_chem,{
    saveAsParameterSetUI(input$btn_saveas_chem,"chem")
    parameterSets$savedat <- callModule(saveAsParameterSet,input$btn_saveas_chem,"chem",isolate(input),chem_name_df)
  })


  # update the paramter set dropdown if it is changed
  observe({
    result_vector <- parameterSets$savedat
    if(result_vector()[1]=="Yes"){
      set_type <- result_vector()[2]
      set_id <- result_vector()[3]
      set_list <- getAllSetChoices(set_type)
      parameterSets[[set_type]] <- reactiveVal(set_list)
      updateSelectizeInput(session,paste0("sel_",set_type),choices = set_list, selected = set_id)
      
      parameterSets$savedat <- reactiveVal(c("No","",0))
      # updateSelectizeInput(session,paste0("sel_scene_",set_type),choices = set_list)
    }
  })

  #Save/Restore Button function
  observeEvent(input$btn_sverest_physio,{
    physioid <- input$sel_physio
    set_values <- getParameterSet("physio",physioid)
    UI_values <- reactiveValuesToList(input)[paste0("ms_",physio_name_df$Var)]
    names(UI_values) <- gsub("ms_","",names(UI_values))
    saveRestoreParameterSetUI(input$btn_sverest_physio)
    parameterSets$sverestdat <- callModule(saveRestoreParameterSet,
                                           input$btn_sverest_physio,
                                           UI_values,set_values,
                                           physio_name_df,"physio")

  })

  #Save/Restore Button function
  observeEvent(input$btn_sverest_expo,{
    expoid <- input$sel_expo
    set_values <- getParameterSet("expo",expoid)
    UI_values <- reactiveValuesToList(input)[paste0("ms_",expo_name_df$Var)]
    names(UI_values) <- gsub("ms_","",names(UI_values))

    saveRestoreParameterSetUI(input$btn_sverest_expo)
    parameterSets$sverestdat <- callModule(saveRestoreParameterSet,
                                           input$btn_sverest_expo,
                                           UI_values,set_values,
                                           expo_name_df,"expo")
  })

  #Save/Restore Button function
  observeEvent(input$btn_sverest_chem,{
    chemid <- input$sel_chem
    set_values <- getParameterSet("chem",chemid)

    #chem_vars <- subset(chem_name_df$Var,!(chem_name_df$Var %in% c("name","cas","descrp")))
    UI_values <- reactiveValuesToList(input)[paste0("ms_",chem_name_df$Var)]
    names(UI_values) <- gsub("ms_","",names(UI_values))

    saveRestoreParameterSetUI(input$btn_sverest_chem)
    parameterSets$sverestdat <- callModule(saveRestoreParameterSet,
                                           input$btn_sverest_chem,
                                           UI_values,set_values,
                                           chem_name_df,"chem")
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
      }else{
        table_name <- "Exposure"
      }


      # create a data frame for the mapply below
      val_df <- data.frame("var"=result_vector[2],"val"= result_vector[4],stringsAsFactors = FALSE,row.names = NULL)

      # create the query
      query_list <-mapply(function(var,val,tbl_nme,id_nme,id){
        temp <- sprintf("UPDATE %s SET value = %s WHERE %s = %i AND param = '%s';",
                        tbl_nme,val,id_nme,id,var)
        return(temp)
      },
      val_df$Variable,val_df$Current.Value,table_name,id_name,input_id,SIMPLIFY = T)
      lapply(query_list,projectDbUpdate)

    }else if (ops_type == "restore"){
      type <- result_vector[5]
      if (type == "physio"){
        name_data <- physio_name_df
      }else if(type == "chem"){
        name_data <- chem_name_df
      }else{
        name_data <- expo_name_df
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
    param_names <- physio_name_df$Name[which(physio_name_df$Variability == "TRUE")]
    param_vars <- physio_name_df$Var[which(physio_name_df$Variability == "TRUE")]
    names(param_vars) <- param_names
    ns <- paste0("vpe_",input$btn_edit_varphys)
    newEditVariabilityUI(ns)
    parameterSets$vardat <- callModule(newEditVariability,ns,"physio","edit",
                                       param_vars,input$sel_physio_var)
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
    param_names <- chem_name_df$Name[which(chem_name_df$Variability == "TRUE")]
    param_vars <- chem_name_df$Var[which(chem_name_df$Variability == "TRUE")]
    names(param_vars) <- param_names
    ns <- paste0("vce_",input$btn_edit_varchem)
    newEditVariabilityUI(ns)
    parameterSets$vardat <- callModule(newEditVariability,ns,"chem","edit",
                                       param_vars,input$sel_chem_var)
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
    param_names <- expo_name_df$Name[which(expo_name_df$Variability == "TRUE")]
    param_vars <- expo_name_df$Var[which(expo_name_df$Variability == "TRUE")]
    names(param_vars) <- param_names
    ns <- paste0("vee_",input$btn_edit_varexpo)
    newEditVariabilityUI(ns)
    parameterSets$vardat <- callModule(newEditVariability,ns,"expo","edit",
                                       param_vars,input$sel_expo_var)
    ### Variability Tab
  },ignoreInit = T, ignoreNULL = T)
  
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


  observeEvent(input$sel_expo,{
    expoid <- input$sel_expo
    #get the values for inputs
    expo_values <- getParameterSet("expo",expoid)
    params_df <- expo_name_df
    params_df$Val <- expo_values[expo_name_df$Var]
    updateUIInputs(session,params_df)

  },ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$sel_chem,{
    chemid <- input$sel_chem
    #get the values for inputs
    chem_values <- getParameterSet("chem",chemid)
    params_df <- chem_name_df
    params_df$Val <- chem_values[chem_name_df$Var]
    updateUIInputs(session,params_df)

  },ignoreInit = TRUE, ignoreNULL = TRUE)

  ### CODE CHUNK FOR HANDLING SIMULATIONS TAB

  # Save a new simulation
  observeEvent(input$save_sim,{
    if (any(c(input$sim_name,input$sim_descrp)=="")){
      sendSweetAlert(session,"Error",
                     "Need to provide Name and Decription for the simulation",
                     type = "error")

    }else{
      simid <- getNextID("SimulationsSet")
      sim_name <- input$sim_name
      sim_descrp <- input$sim_descrp
      sim_start <- input$sim_start
      sim_dur <- input$sim_dur
      mc_num <- ifelse(input$mc_mode,input$mc_num,0)
      chemid <- as.integer(input$sel_set_chem)
      physioid <- as.integer(input$sel_set_physio)
      expoid <- as.integer(input$sel_set_expo)
      metabid <- 0
      physiovarid <- as.integer(input$sel_set_physiovar)
      chemvarid <- as.integer(input$sel_set_chemvar)
      expovarid <- as.integer(input$sel_set_expovar)
      query <- paste(strwrap(sprintf("INSERT INTO SimulationsSet (simid,name,descrp,expoid,physioid,
                                     chemid,metabid,physiovarid, chemvarid,expovarid,tstart,sim_dur,mc_num) Values
                                     (%d,'%s','%s',%i,%i,%i,%i,%i,%i,%i,%f,%f,%i) ;",
                                     simid,sim_name,sim_descrp,
                                     expoid,physioid,
                                     chemid,metabid,
                                     physiovarid,chemvarid,
                                     expovarid,
                                     sim_start,sim_dur,mc_num),
                             simplify = T),
                     sep = " ",collapse = "")
      projectDbUpdate(query)
      sim_sets <- getAllSetChoices("sim")
      updateSelectizeInput(session,"sel_sim",choices = sim_sets)

      updateTextInput(session,"sim_name",value = "")
      updateTextAreaInput(session,"sim_descrp",value = "")
      sendSweetAlert(session,"Success",
                     sprintf("Simulation saved as %s",sim_name),
                     type = "success")
    }

  })

  observeEvent(input$sel_sim,{
    simid <- as.integer(input$sel_sim)
    # get pertinent data from the database
    # get All values from the simulations database
    query <- sprintf("Select name,descrp,metabid,expoid,physioid,chemid,tstart,sim_dur FROM SimulationsSet Where simid = %i;",
                     simid)

    result <- projectDbSelect(query)
    metabid <- as.integer(result[["metabid"]])
    chemid <- as.integer(result[["chemid"]])
    expoid <- as.integer(result[["expoid"]])
    physioid <- as.integer(result[["physioid"]])
    sim_name <- result[["name"]]
    sim_descrp <- result[["descrp"]]
    tstart <- round(result[["tstart"]],2)
    sim_dur <- round(result[["sim_dur"]],2)
    output$sim_name <- renderText(sim_name)
    output$sim_descrp <- renderText(sim_descrp)
    output$sim_start <- renderText(as.character(tstart))
    output$sim_dur <- renderText(as.character(sim_dur))

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
    metab_data <- getMetabData(metabid,physioid,chemid,model)
    output$sim_metab_type <- renderText(metab_data$Type)
    output$sim_metab_units <- renderText(metab_data$Units)
    output$sim_metab_val <- renderText(as.character(round(metab_data$Value,2)))



  },ignoreInit = TRUE, ignoreNULL =  TRUE)

  # Code chunk to run the simulation.
  results <- reactiveValues(pbpk=NULL,simid = NULL,mode = NULL)
  observeEvent(input$run_sim,{
    simid <- as.integer(input$sel_sim)
    results$simid <- simid
    # get the parameters needed to run the model
    model_params <- getAllParamValuesForModel(simid,model)
    #get total volume
    active_comp <- input$ms_cmplist
    vol_comps <- c(active_comp,"blood")
    total_vol <- sum(unlist(lapply(vol_comps,
                                   function(x){
                                     input[[vol_ids[x]]]
                                     })
                            )
                     )
    query <- sprintf("Select mc_num From SimulationsSet where simid = %i",simid)
    mc_num <- as.integer(projectDbSelect(query)$mc_num)
    model_params$vals[["total_vol"]]<- total_vol
    if (mc_num > 1){
      MC.matrix <- getAllVariabilityValuesForModel(simid,model_params$vals,mc_num)
      query <- sprintf("Select model_var from ResultNames where mode = 'MC' AND model = '%s'",
                       model)
      mc_vars<- mainDbSelect(query)$model_var
      mc_results <- lapply(mc_vars,function(x,n){
        return(x = rep(NA,n))
        },mc_num)
      names(mc_results)<- mc_vars
      for (i in 1:mc_num){
        model_params$vals[colnames(MC.matrix)]<- MC.matrix[i,]
        initial_values <- calculateInitialValues(model_params)
        tempDF <- runFDPBPK(initial_values,model)
        max_list <- unlist(lapply(mc_vars,function(x,data){
          var_name <- gsub("_max","",x)
 
          return(max(data[var_name]))
        },tempDF$pbpk))
        names(max_list)<- mc_vars
        for (x in mc_vars){
          mc_results[[x]][[i]]<- max_list[[x]]
        }
        updateProgressBar(session,"pb",value = i, total = mc_num)
      }
      results$pbpk <- as.data.frame(mc_results)
      results$mode <- "MC"
      updateNavbarPage(session,"menu","output")
    }else{
      #rep_flag <- all_params["rep_flag"]
      #model_params <- all_params["model_params"]
      initial_values <- calculateInitialValues(model_params)

      updateProgressBar(session,"pb",value = 100, total = 100,
                        status = "info")
      tempDF <- runFDPBPK(initial_values,model)
      
      results$pbpk<- tempDF$pbpk
      
      
      results$mode <- "FD"
      updateNavbarPage(session,"menu","output")
    }
    


  })



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

  observeEvent(input$run,{
    active_comp <- input$ms_cmplist
    compartment_list <-c("skin","fat","muscle","bone","brain","lung","heart","gi","liver","kidney","rpf","spf")
    inactive_comp <- setdiff(compartment_list,active_comp)
    vol_comps <- c(active_comp,"blood")
    perfc <- input$ms_perfc
    total_vol <- sum(unlist(lapply(vol_comps,function(x){input[[vol_ids[x]]]})))
    #exposure
    if ("gi" %in% active_comp && !("liver" %in% active_comp)){
      showModal(
        modalDialog(
          tags$h4("Invalid Compartment Configuration"),
          tags$h5("Liver compartment needs to be active if GI compartment is active"),
          title = "Error"
        )
      )
    }else if (length(active_comp) == 0){
      showModal(
        modalDialog(
          tags$h4("Invalid Compartment Configuration"),
          tags$h5("At least one compartment needs to be active for the model to run."),
          title = "Error"
        )
      )
    }else if(abs(total_vol-perfc)>0.03){
      showModal(
        modalDialog(
          tags$h4("Invalid Compartment Configuration"),
          tags$h5("The total volume of all compartments does not add up to 85%"),
          title = "Error"
        )
      )
    }else if((input$ms_bdose>0 || input$ms_drdose>0) && !("gi" %in% active_comp)){
      showModal(
        modalDialog(
          tags$h4("Invalid Compartment Configuration"),
          tags$h5("GI compartment must be active for Oral and Drinking water routes of exposure"),
          title = "Error"
        )
      )
    }else{
      # set volumes of inactive compartments to 1e-8 ( very low)
      sapply(inactive_comp,function(x){updateNumericInput(session,vol_ids[x],value = 1e-8)})
      # set blood flow of inactive compartments to 0 ( very low)
      sapply(inactive_comp,function(x){updateNumericInput(session,flow_ids[x],value = 0)})
      withProgress({
        tempDF <- runPBPKmodel(input, total_vol,perfc)
        results$pbpk<- tempDF$pbpk
      },
      message = "Running Simulation",
      value = 0.75
      )

    }



  }


  )

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
      plot_frame["Model not yet run"]<-rep(0,length(x))
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


  #  Concentration plot Data
  concData <- reactive({
    result <- results$pbpk
    units <- input$r_cplt_type
    simid <- results$simid
    mode <- results$mode
    if(is.null(simid)){
      mw <- 1000 # to keep the multiplier as 1

    }else{
      query <- sprintf("SELECT mc_num,chemid FROM SimulationsSet Where simid = %i ;",
                       simid)
      chemid <- projectDbSelect(query)$chemid
      mc_num <- projectDbSelect(query)$mc_num
      query <- sprintf("Select value FROM Chemical WHERE chemid = %i AND param = 'mw';",
                       chemid)
      mw <- projectDbSelect(query)$value
    }

    #get value multiplier based on concentration units
    if(units == "um"){
      multiplier <- 1000/mw
    }else{
      multiplier <- 1
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
      if(mode == "FD"){
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
      plot_frame["Model not yet run"]<-rep(0,length(x))
    }
    else if(length(values) >0 ){
      for (plt_name in values){
        y<- result[[plt_name]] * multiplier
        plot_frame[[legend_names[plt_name]]] <-y
      }
    }else{
      plot_frame["No Data Selected"]<-rep(0,length(x))
    }
    if (mode == "FD"){
      plot_frame <- reshape2::melt(plot_frame,id.vars = "time")
    }else{
      plot_frame <- reshape2::melt(plot_frame,id.vars = "sample")
    }
    return(plot_frame)

    })
  #Concentration table data
  conc_tble_data <- reactive({
    mode <- results$mode
    plt_data<- concData()
    return(reshapePlotData(plt_data,mode))
  })

  amtData <- reactive({
    result <- results$pbpk
    units <- input$r_aplt_type
    simid <- results$simid
    mode <- results$mode
    
    if(is.null(simid)){
      mw <- 1000 # to keep the multiplier as 1
      
    }else{
      query <- sprintf("SELECT mc_num,chemid FROM SimulationsSet Where simid = %i ;",
                       simid)
      chemid <- projectDbSelect(query)$chemid
      mc_num <- projectDbSelect(query)$mc_num
      query <- sprintf("Select value FROM Chemical WHERE chemid = %i AND param = 'mw';",
                       chemid)
      mw <- projectDbSelect(query)$value
    }
    #get value multiplier based on concentration units
    if(units == "um"){
      multiplier <- 1000/mw
    }else{
      multiplier <- 1
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
      if(mode == "FD"){
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
      plot_frame["Model not yet run"]<-rep(0,length(x))
    }
    else if(length(values) >0 ){
      for (plt_name in values){
        y<- result[[plt_name]] *multiplier
        plot_frame[[legend_names[plt_name]]] <-y
      }
    }else{
      plot_frame["No Data Selected"]<-rep(0,length(x))
    }
    if (mode == "FD"){
      plot_frame <- reshape2::melt(plot_frame,id.vars = "time")
    }else{
      plot_frame <- reshape2::melt(plot_frame,id.vars = "sample")
    }
    return(plot_frame)
  })
  
  #Concentration table data
  amt_tble_data <- reactive({
    mode <- results$mode
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
      plot_frame["Model not yet run"]<-rep(0,length(x))
    }else{
      plot_frame["Mass Balance"]<- result$mbal
    }
    plot_frame <- reshape2::melt(plot_frame,id.vars = "time")

    return(plot_frame)
  })

  # output$concplt <- plotly::renderPlotly(plotly::ggplotly(ggplot()
  #                              +geom_line(data = concData(), aes(x=time,y=value,color = variable))
  #                              +geom_pointrange(data = concDataset(),aes(x = time,y = mean, ymin = mean-sd ,ymax = mean+sd,fill = "Dataset (mg/L)"))
  # 
  #                              +labs(x="Time (h)",y="Concentration")
  #                              +theme(axis.text=element_text(size = 15),axis.title=element_text(size = 25),legend.text=element_text(size=15),legend.title=element_blank())))
  concplt <- reactive({
    if (results$mode == "FD"){
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
                       yaxis = list(title = (ifelse(input$r_cplt_type=="um",'Concentration (\u00B5M)',
                                                    'Concentration (mg/L)'))))
      
    }else{
      plotly::plot_ly()%>%
        plotly::add_trace(data = concData(),
                          y = ~value,color = ~variable,
                          type = "box")%>%
        plotly::layout(yaxis = list(title = (ifelse(input$r_cplt_type=="um",
                                                    'Concentration (\u00B5M)',
                                                    'Concentration (mg/L)'))
                                    )
                       )
    }
  })
  
  amtplt <- reactive({
    if (results$mode == "FD"){
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
      write.csv(reshapePlotData(concData()), file)
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
  # power button to shut down the app
  observeEvent(input$menu,{
    if(input$menu=="stop"){
      shinyWidgets::confirmSweetAlert(session,"close_dialog", "Close Application",
                                   "Any changes will not be saved. Proceed?",type = "info",danger_mode = T)

    }else if(input$menu == "save"){
      shinyWidgets::confirmSweetAlert(session,"save_dialog", "Save Project",
                                      "Unsaved changes will be lost. Proceed?",type = "info",danger_mode = T)
    }
    
  })
  observeEvent(input$close_dialog,{
    if (input$close_dialog){
      stopApp()
    }else{
      updateNavbarPage(session,"menu","setup")
    }
  })
  observeEvent(input$save_dialog,{
    if(input$save_dialog){
      saveProject()
    }else{
      updateNavbarPage(session,"menu","setup")
    }
  })
})

calculateInitialValues <- function(params_list){
  params <- params_list$vals
  params <- params[which(grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",params))]
  params <- lapply(params,function(x){as.numeric(x)})

  initial_params <- within(as.list(params),{
    perfc <- 1
    total_vol <- vlivc + vfatc + vkdnc + vrpfc + vspfc
    #Scaled Tissue Volumes
    vfat <- vfatc*(perfc/total_vol)*bw
    vkdn <- vkdnc*(perfc/total_vol)*bw
    vliv <- vlivc*(perfc/total_vol)*bw
    vrpf <- vrpfc*(perfc/total_vol)*bw
    vspf <- vspfc*(perfc/total_vol)*bw

    #Total Fractional Perfusion
    total_perf <- qfatc+qkdnc+qlivc+qrpfc+qspfc  # This does not include flow to GI since that is a part of liver venous flow

    #Scaled Perfusion
    qfat <- qfatc*(1/total_perf)*qc
    qkdn <- qkdnc*(1/total_perf)*qc
    qliv <- qlivc*(1/total_perf)*qc
    qrpf <- qrpfc*(1/total_perf)*qc
    qspf <- qspfc*(1/total_perf)*qc

    tstop <- tstart+sim_dur
    #calculate gut uptake limit
    gul <- ifelse(qc*pbldw > qrpf,qg,qc*pbldw)
    
  })

  #function for dosing

  mw <- initial_params[["mw"]]
  bw <- initial_params[["bw"]]
  #Chemical Inspiration
  cins <- initial_params[["cins"]]
  tstart <- initial_params[["tstart"]]
  tstop <- initial_params[["tstop"]]
  

  times <- seq(tstart,tstop,by=0.1)
  eventDat <- list("time"= 0)

  state <- c(
    cfat = 0.0,
    cliv = 0.0,
    cspf = 0.0,
    crpf = 0.0,
    ckdn = 0.0,
    cmet = 0.0,
    ains = 0.0,
    insswch = 0.0)

  initial_values <- list("evnt_data"= eventDat,
                         "initial_params"= initial_params[params_list$names],
                         "times"=times,
                         "tstop"=tstop,"tstart"=tstart,
                         "state"= state)

  return(initial_values)
}

fishPBPK_initParms <- function(newParms = NULL) {
  parms <- c(
    bw = 0,
    qc = 0,
    qg = 0,
    vfatc = 0,
    qfatc = 0,
    pfat = 0,
    vlivc = 0,
    qlivc = 0,
    pliv = 0,
    vkdnc = 0,
    qkdnc = 0,
    pkdn = 0,
    vrpfc = 0,
    qrpfc = 0,
    prpf = 0,
    vspfc = 0,
    qspfc = 0,
    pspf = 0,
    frspfkdn = 0.4,
    vfat = 0,
    vkdn = 0,
    vliv = 0,
    vrpf = 0,
    vspf = 0,
    qfat = 0,
    qkdn = 0,
    qliv = 0,
    qrpf = 0,
    qspf = 0,
    vmax = 0,
    km = 1e-10,
    cins = 0,
    pbldw = 1e10,
    gul = 1
  )
  
  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
    parms[names(newParms)] <- newParms
  }
  
  parmsfishPBPK <- within(as.list(parms), {
  })
  out <- .C("getParmsfishPBPK",  as.double(parms),
            out=double(length(parms)),
            as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

fishPBPK_Outputs <- c(
  "cv",
  "ca",
  "mbal"
)

fishPBPK_initStates <- function(parms, newStates = NULL) {
  Y <- c(
    cfat = 0.0,
    cliv = 0.0,
    cspf = 0.0,
    crpf = 0.0,
    ckdn = 0.0,
    cmet = 0.0,
    ains = 0.0,
    insswch = 0.0
  )
  
  if (!is.null(newStates)) {
    if (!all(names(newStates) %in% c(names(Y)))) {
      stop("illegal state variable name in newStates")
    }
    Y[names(newStates)] <- newStates
  }
  
  .C("initStatefishPBPK", as.double(Y));
  Y
}
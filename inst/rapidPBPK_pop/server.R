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
  metab_set <- getAllSetChoices("metab")
  sim_set <- getAllSetChoices("sim")
  physiovar <-getVariabilitySetChoices("physio")
  chemvar <-getVariabilitySetChoices("chem")
  expovar <-getVariabilitySetChoices("expo")

  parameterSets$expo <- reactiveVal(expo_set)
  parameterSets$physio <- reactiveVal(physio_set)
  parameterSets$chem <- reactiveVal(chem_set)
  parameterSets$metab <- reactiveVal(metab_set)
  parameterSets$sim <- reactiveVal(sim_set)
  parameterSets$physiovar <- reactiveVal(physiovar)
  parameterSets$chemvar <- reactiveVal(chemvar)
  parameterSets$expovar <- reactiveVal(expovar)

  # conc_datasets <- c("none",getDatasetNames("conc"))
  # updateSelectizeInput(session,"cplt_data",choices = conc_datasets)

  observe({
    exposet <- parameterSets$expo()
    updateSelectizeInput(session,"sel_set_expo",choices = exposet)
    physioset <- parameterSets$physio()
    updateSelectizeInput(session,"sel_set_physio",choices = physioset)
    chemset <- parameterSets$chem()
    updateSelectizeInput(session,"sel_set_chem",choices = chemset)
    metabset<- parameterSets$metab()
    metabset <- c("Use Chemical Vmax"="0","Use Chemical Vkm1"="1",metabset)
    updateSelectizeInput(session,"sel_set_metab",choices = metabset)
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

  # get the connection to the master database
  #db <- system.file("database/plethemdb.sqlite",package = "plethem.r.package",mustWork = TRUE)
  #master_conn <- RSQLite::dbConnect(RSQLite::SQLite(),db)

  # get the parameter table for physiological and exposure variables.
  query <- sprintf("SELECT Name,Var,Units,ParamType,Variability FROM ParamNames Where Model='%s' AND ParamSet = 'Physiological' AND UIParams = 'TRUE';",
                   model)
  physio_name_df <- mainDbSelect(query)
  # res <- RSQLite::dbSendQuery(master_conn,query)
  # physio_name_df <- RSQLite::dbFetch(res)
  # RSQLite::dbClearResult(res)

  query <- sprintf("SELECT Name,Var,Units,ParamType,Variability FROM ParamNames Where Model='%s' AND ParamSet = 'Exposure' AND UIParams = 'TRUE';",
                   model)
  expo_name_df <- mainDbSelect(query)
  # res <- RSQLite::dbSendQuery(master_conn,query)
  # expo_name_df <- RSQLite::dbFetch(res)
  # RSQLite::dbClearResult(res)

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
  set_choices <- getAllSetChoices(set_type = "metab")
  if (length(set_choices)>0){
    updateSelectizeInput(session,"sel_metab",choices = set_choices)
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


   #paraValueList <- getAllParamValues(isolate(input))
   #param_values_list <-getAllParamValues(isolate(input))




  # ################# Updating Chemical parameter values
  # observeEvent({input$selectedChem },{
  #   if(input$selectedChem != ""){
  #     if(input$useQSar){
  #       updateAwesomeCheckbox(session,"useQSar",value = F)
  #     }
  #       #updateMainChemValues(session, input$selectedChem)
  #
  #     values <- updateMainChemValues(session, input$selectedChem)
  #
  #     for(name in names(values)){
  #      paraValueList[name] <<- values[[name]]
  #      param_values_list[name] <<- values[[name]]
  #      }
  #   }
  # })

  ########### The next code chunk deals with updating select inputs for all parameter sets]
  # Import SEEM, SHEDS-HT, batch exposure, and TRA data
  observeEvent(input$btn_import_expo,{
    importAllExposureDataUI(paste0("allData",input$btn_import_expo))
    parameterSets$importAllData <- callModule(importAllExposureData,
                                              paste0("allData",input$btn_import_expo),
                                              expo_name_df)
  })
  
  # observe({
  #   result_vector <- parameterSets$importAllData
  #   if(result_vector()[1] != "No"){
  #   if(result_vector()[1]=="SEEM"){parameterSets$importSeem <- reactiveVal(c("Yes"))}
  #   result_vector <- parameterSets$importSeem
  #   if(result_vector()[1]=="Yes"){
  #     set_type <- "expo"
  #     set_list <- getAllSetChoices(set_type)
  #     parameterSets[[set_type]] <- reactiveVal(set_list)
  #     updateSelectizeInput(session,paste0("sel_",set_type),
  #                          choices = set_list)
  #     parameterSets$importSeem <- reactiveVal(c("No"))
  #   }}
  # })
  
  # observeEvent(input$btn_batch_upload,{
  #   importBatchExposureUI(paste0("batch",input$btn_batch_upload))
  #   parameterSets$importBatch <- callModule(importBatchExposure,
  #                                           paste0("batch",input$btn_batch_upload),
  #                                           expo_name_df)
  # })
  # observe({
  #   result_vector <- parameterSets$importBatch
  #   if(result_vector()[1]=="Yes"){
  #     set_type <- "expo"
  #     set_list <- getAllSetChoices(set_type)
  #     parameterSets[[set_type]] <- reactiveVal(set_list)
  #     updateSelectizeInput(session,paste0("sel_",set_type),
  #                          choices = set_list)
  #   }
  # })
  
  
  
  
  
  # Import SEEM data
  observeEvent(input$btn_seem_upload,{
    path <-fpath_seem()
    importSEEMDataUI(paste0("seem",input$btn_seem_upload))
    parameterSets$importSeem <- callModule(importSEEMData,paste0("seem",input$btn_seem_upload),
                       path,expo_name_df)
  })
  
  fpath_seem <- reactive({
    fpath <- tcltk::tk_choose.files(multi = F)
    return(fpath)
  })
  
  observe({
    result_vector <- parameterSets$importSeem
    if(result_vector()[1]=="Yes"){
      set_type <- "expo"
      set_list <- getAllSetChoices(set_type)
      parameterSets[[set_type]] <- reactiveVal(set_list)
      updateSelectizeInput(session,paste0("sel_",set_type),
                           choices = set_list)
    }
  })
  
  # Import SHEDS-HT data
  observeEvent(input$btn_sheds_upload,{
    path <-fpath_sheds()
    importShedsDataUI(paste0("sheds",input$btn_sheds_upload))
    parameterSets$importSheds<- callModule(importShedsData,
                                           paste0("sheds",input$btn_sheds_upload),
                                           path,expo_name_df)
  })
  
  fpath_sheds <- reactive({
    fpath <- rstudioapi::selectDirectory("Select SHEDS-HT Folder")
    return(fpath)
  })
  
  observe({
    result_vector <- parameterSets$importSheds
    if(result_vector()[1]=="Yes"){
      set_type <- "expo"
      set_list <- getAllSetChoices(set_type)
      parameterSets[[set_type]] <- reactiveVal(set_list)
      updateSelectizeInput(session,paste0("sel_",set_type),
                           choices = set_list)
    }
  })
  # Import Batch Exposure data
  observeEvent(input$btn_batch_upload,{
    importBatchExposureUI(paste0("batch",input$btn_batch_upload))
    parameterSets$importBatch <- callModule(importBatchExposure,
               paste0("batch",input$btn_batch_upload),
               expo_name_df)
  })
  observe({
    result_vector <- parameterSets$importBatch
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
       if(set_type == "chem"){
         updateSelectizeInput(session,"sel_chem4Partition",choices = set_list)
         
       }
       
       # updateSelectizeInput(session,paste0("sel_scene_",set_type),choices = set_list)
     }
   })
   
   
   
  

  #Save a new physiological parameter set
  observeEvent(input$btn_saveas_physio,{
    active_comp <- input$ms_cmplist
    compartment_list <-c("skin","fat","muscle","bone","brain","lung","heart","gi","liver","kidney","rpf","spf")
    inactive_comp <- setdiff(compartment_list,active_comp)
    vol_comps <- c(active_comp,"blood")
    perfc <- input$ms_perfc
    total_vol <- sum(unlist(lapply(vol_comps,function(x){input[[vol_ids[x]]]})))
    #exposure
    if((input$ms_bdose==0 || input$ms_breps == 0) && input$ms_drdose==0 && input$ms_inhdose==0 && input$ms_ivdose==0){
      showModal(
        modalDialog(
          tags$h4("Invalid Exposure Parameters"),
          tags$h5("Atleast one route of exposure should be active"),
          title = "Error"
        )
      )
    }else if ("gi" %in% active_comp && !("liver" %in% active_comp)){
      shinyWidgets::sendSweetAlert(session,
                                   title = "Invalid Compartment Configuration",
                                   text = "Liver compartment needs to be active if GI compartment is active",
                                   type = "error")

    }else if (length(active_comp) == 0){
      shinyWidgets::sendSweetAlert(session,
                                   title = "Invalid Compartment Configuration",
                                   text = "At least one compartment needs to be active for the model to run",
                                   type = "error")

    }else if(abs(total_vol-perfc)>0.03){

      error_text <- sprintf("The total volume of all compartments does not add up to %i %%",
              as.integer(perfc*100))

      shinyWidgets::sendSweetAlert(session,
                                   title = "Invalid Compartment Configuration",
                                   text = error_text,
                                   type = "error")

    }else if((input$ms_bdose>0 || input$ms_drdose>0) && !("gi" %in% active_comp)){
      showModal(
        modalDialog(
          tags$h4("Invalid Compartment Configuration"),
          tags$h5("GI compartment must be active for Oral and Drinking water routes of exposure"),
          title = "Error"
        )
      )
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
    if((input$ms_bdose==0 || input$ms_breps == 0) && input$ms_drdose==0 && input$ms_inhdose==0 && input$ms_ivdose==0){
      shinyWidgets::sendSweetAlert(session,
                                   title = "Invalid Exposure Parameters",
                                   text = "Atleast one route of exposure should be active",
                                   type = "error")
      # showModal(
      #   modalDialog(
      #     tags$h4("Invalid Exposure Parameters"),
      #     tags$h5("Atleast one route of exposure should be active"),
      #     title = "Error"
      #   )
      # )
    }else{
      saveAsParameterSetUI(input$btn_saveas_expo,"expo")
      parameterSets$savedat <- callModule(saveAsParameterSet,
                                          input$btn_saveas_expo,
                                          "expo",isolate(input),
                                          expo_name_df)
    }

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
      if(set_type == "chem"){
        updateSelectizeInput(session,"sel_chem4Partition",choices = set_list)
      }
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
      # a <- mapply(function(var,org){
      #   print(var)
      #   tempvar <- name_data$ParamType[which(name_data$Var == var, arr.ind = T)]
      #   return(var,tempvar)
      # },table_data$Variable,table_data$Original.Value)

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
    output$physio_var_tble <- DT::renderDT(DT::datatable(dataset))
    
  },ignoreInit = TRUE, ignoreNULL =  TRUE)
  
  observeEvent(input$sel_chem_var,{
    varid <- input$sel_chem_var
    query <- sprintf("Select var_tble from Variability where varid = %d;",as.integer(varid))
    var_data <- projectDbSelect(query)
    dataset <- unserialize(charToRaw(var_data$var_tble))
    output$chem_var_tble <- renderTable(dataset)
    
  },ignoreInit = TRUE, ignoreNULL =  TRUE)
  
  observeEvent(input$sel_expo_var,{
    varid <- input$sel_expo_var
    query <- sprintf("Select var_tble from Variability where varid = %d;",as.integer(varid))
    var_data <- projectDbSelect(query)
    dataset <- unserialize(charToRaw(var_data$var_tble))
    output$expo_var_tble <- renderTable(dataset)
    
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

  # Metabolism is handled in a very different manner than the rest of the sets.

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
                                                                 fluidRow(column(width = 6,
                                                                                 shinyWidgets::radioGroupButtons("metab_type",justified = T,
                                                                                                                 "Select Meatbolism Type",
                                                                                                                 choices = c("Saturable"="m1","Linear"="m2"))
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

  ##Metabolism realated functions
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
    #if(!(is.null(input$metab_csv))){
    ret_dat <- read.csv(metabFile()$datapath,header = T,stringsAsFactors = F)
    #}else{
    # ret_dat <- data.frame("Age"=c(25),"Clearance"=c(0),stringsAsFactors = F)
    #}
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
        updateSelectizeInput(session,paste0("sel_",set_type),choices = set_list, selected = id_num)
        metabset <- c("Use Chemical Vmax"="0","Use Chemical Vkm1"="1",set_list)
        updateSelectizeInput(session,"sel_set_metab",choices = metabset)
        removeModal()
      }


    }
  })

  #### END METABOLISM TAB

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
      metabid <- as.integer(input$sel_set_metab)
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

    })

  # when age and gender are changed, change the type of button to indicate things are out of sync
  observeEvent({input$ms_age ;input$ms_gender; input$ms_cmplist},{
    shinyBS::updateButton(session,"btn_use_lifecourse",style = "warning")
  },ignoreInit = TRUE )

#Qsar models
  observeEvent(input$btn_useQSAR4Partition,
               {
                 shinyBS::updateButton(session,"btn_useQSAR4partition",style = "primary")
                 chemid <- input$sel_chem4Partition
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
#*******************these are not called anywhere.
# These functions update the current values used in the restore paramsparams
  # resetParaValueList <- function(session, paraValueList, volumes, ratios){
  #
  #   for(name in names(volumes)){
  #     paraValueList[name] <<- volumes[[name]]
  #   }
  #   for(name in names(ratios)){
  #     paraValueList[name] <<- ratios[[name]]
  #   }
  #
  #   return(paraValueList)
  # }
  #
  #
  # resetParam_values_list <- function(session, param_values_list, volumes, ratios){
  #   for(name in names(volumes)){
  #     param_values_list[name] <<- volumes[[name]]
  #   }
  #
  #
  #   for(name in names(ratios)){
  #     param_values_list[name] <<- ratios[[name]]
  #   }
  #   return(param_values_list)
  # }



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

  amtData <- reactive({
    result <- results$pbpk
    simid <- results$simid
    mode <- results$mode

    values <- c()
    query <- sprintf("Select model_var,plot_var,name from ResultNames where param_set = 'amt' AND model='%s' AND mode = '%s';",model,mode)
    legend_df <- mainDbSelect(query)
    legend_names <- setNames(legend_df$name,legend_df$model_var)
    var_names <- setNames(legend_df$model_var,legend_df$plot_var)
    plot_vals<- input$aplt_comp
    values <- unlist(lapply(plot_vals,function(x){var_names[x]}))
    names(values)<- NULL
    # 
    # if (exists("plot_frame")){
    #   rm(plot_frame)
    # }
    # # check if model was ever run
    # if (dim(result)[1]==0){
    #   x<- 1:10
    # }else{
    #   x<- result$time
    # }
    # plot_frame<- data.frame(time = x)
    # # select appropriate variables to plot
    # if (dim(result)[1]==0){
    #   plot_frame["Model not yet run"]<-rep(0,length(x))
    # }
    # else if(length(values) >0 ){
    #   for (plt_name in values){
    #     y<- result[[plt_name]]
    #     plot_frame[[legend_names[plt_name]]] <-y
    #   }
    # }else{
    #   plot_frame["No Data Selected"]<-rep(0,length(x))
    # }
    # plot_frame <- reshape2::melt(plot_frame,id.vars = "time")
    # return(plot_frame)
    
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
        y<- result[[plt_name]]
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
                          type = "box")
    }
  })
  
  amtplt <- reactive({
    if (results$mode == "FD"){
      plotly::plot_ly() %>%
        plotly::add_trace(data = amtData(),x =~time,
                          y= ~value,color = ~variable,
                          type = "scatter",mode="lines")
      # plotly::ggplotly(ggplot(amtData(), aes(x=time,y=value,color = variable))+geom_line()
      #                  +labs(x="Time (h)",y="Amount")
      #                  +theme(axis.text=element_text(size = 15),axis.title=element_text(size = 25),legend.text=element_text(size=15),legend.title=element_blank()))
    }else{
      plotly::plot_ly()%>%
        plotly::add_trace(data = amtData(),
                          y = ~value,color = ~variable,
                          type = "box")
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
  output$conctble <- DT::renderDT(reshapePlotData(concData()))
  output$expotble <- DT::renderDT(reshapePlotData(exposureData()))
  output$amttble <- DT::renderDT(reshapePlotData(amtData()))
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
    if(input$menu=="Stop"){
      shinyWidgets::confirmSweetAlert(session,"close_dialog", "Close Application",
                                   "Any changes will be saved. Proceed?",type = "info",danger_mode = T)

    }
  })
  observeEvent(input$close_dialog,{
    if (input$close_dialog){
      saveProject()
      stopApp()
    }else{
      updateNavbarPage(session,"menu","setup")
    }
  })
})

calculateInitialValues <- function(params_list){
  params <- params_list$vals
  brep_flag <- as.logical(params[["brep_flag"]])
  iv_flag <- as.logical(params[["ivrep_flag"]])
 
  params <- params[which(grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",params))]
  params <- lapply(params,function(x){as.numeric(x)})

  initial_params <- within(as.list(params),{

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

    cinh <- (inhdose/24.45)#*1000/mw # converting from  ppm to mg/L(/24.45) and then to umoles/L for the model
    qalv <- (tv-ds)*respr
    pair <- ifelse(pair >0,pair,1E-10)
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

  #inhalation
  inhdose <- initial_params[["inhdose"]]
  inhtlen <- initial_params[["inhtlen"]]
  inhdays <- initial_params[["inhdays"]]

  #iv
  ivdose <- initial_params[["ivdose"]]
  ivlen <- initial_params[["ivlen"]]

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
    print(eventDat$time)
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

  times <- seq(tstart,tstop,by=0.1)
  eventDat <- eventDat[order(eventDat$time),]

  state <- c(
    #exposure related
    inhswch=0,ainh=0,aexh=0,
    totodose=0,odose=0,totddose=0,ddose=0,aabsgut=0,
    ivswch=0,aiv=0,
    #compartments
    abld=0,
    abfat=0,atfat=0,
    abskin=0,atskin=0,
    abmusc=0,atmusc=0,
    abbone=0,atbone=0,
    abbrn=0,atbrn=0,
    ablng=0,atlng=0,
    abhrt=0,athrt=0,
    abgi=0,atgi=0,
    abliv=0,atliv=0,
    abkdn=0,atkdn=0,
    abrpf=0,atrpf=0,
    abspf=0,atspf=0,
    # Clearance
    ametliv1=0,ametliv2=0,aclbld=0,auexc=0,anabsgut=0)

  initial_values <- list("evnt_data"= eventDat,
                         "initial_params"= initial_params[params_list$names],
                         "times"=times,
                         "tstop"=tstop,"tstart"=tstart,
                         "state"= state)

  return(initial_values)
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

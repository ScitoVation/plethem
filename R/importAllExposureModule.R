#' UI for importing all (Batch Exposure, TRA, SEEM, and SHEDS) data module.
#' @description This function is called by the pbpk model to import all (Batch Exposure, TRA, SEEM, and SHEDS) data module estimates. Never called by the user
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
                           fileInput(ns("batchExposure"),
                                     "Select Exposure file",multiple = F,
                                     accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
                           ),
                           shinyBS::bsCollapse(
                             shinyBS::bsCollapsePanel("Oral Exposure",
                                                      DT::DTOutput(ns("oralDT")) ),
                             shinyBS::bsCollapsePanel("Drinking Water Exposure",
                                                      DT::DTOutput(ns("dwDT")) ),
                             shinyBS::bsCollapsePanel("Inhalation Exposure",
                                                      DT::DTOutput(ns("inhDT")) ),
                             shinyBS::bsCollapsePanel("Intravenous Exposure",
                                                      DT::DTOutput(ns("ivDT")) )
                           )),
                  
                  ## Import TRA Data ##
                  tabPanel("TRA",
                           ## Begin ##
                           fileInput(ns("expoFile_upload"),
                                     label = "Upload Exposure Excel File",
                                     multiple = F,
                                     buttonLabel = "Browse"),
                           # pickerInput(ns("sel_expo"),
                           #             label= "Select Exposure",
                           #             width = validateCssUnit("600px"),
                           #             choices = NULL),
                           # fillRow(
                           #   DT::DTOutput("expo_table")
                           # ),
                           pickerInput(ns("sel_export"),"Select exposures to export",
                                       choices = NULL,multiple = T),
                           numericInput(ns("TRA_MW"), "Molecular Weight", 1),
                           numericInput(ns("TRA_inhalation_week"), "Inhalation Doses Per Week", 1),
                           checkboxInput(ns("TRA_repeated_oral"), "Repeated Oral Dose")),
                  
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
                                                            "Population Upper 95th Percentile"="Total_Upper95"))),
                  
                  ## Import SHEDS Data ##
                  tabPanel(title = "SHEDS Data",
                           shinyBS::bsButton(ns("btn_SHEDS_data_file"),
                                             "Select SHEDS Data Folder",
                                             block = T),
                           selectInput(ns("sel_scene"),"Select Scenario",choices = NULL),
                           pickerInput(ns("sel_chem"),"Select Chemical",choices = NULL,multiple = T),
                           pickerInput(ns("sel_cohort"),"Select Cohort",
                                       choices = c("Population"="Total",
                                                   "Males"="Males",
                                                   "Females"="Females"),
                                       multiple = T)
                           # ,
                           # checkboxGroupButtons(ns("ch_expotype"),"Select Exposures",
                           #                      choices = c("Oral","Inhalation"),#,"Dermal"
                           #                      checkIcon = list(
                           #                        yes = icon("ok", 
                           #                                   lib = "glyphicon"))),
                           # prettyCheckbox(ns("ch_var"),"Create Variability Sets from Data",
                           #                fill = T,status = "info",bigger = T)
      ))),
    footer = tagList(modalButton("Dismiss"),
                     shinyBS::bsButton(ns("importAll"),"Import Selected Exposures"))
    ))}

#' Server function for all (Batch Exposure, TRA, SEEM, and SHEDS) data module
#' @description Server function for import all (Batch Exposure, TRA, SEEM, and SHEDS) data module. This function should not be called by the user
#' @param input input object for the UI
#' @param output input object to the UI
#' @param session session object for the module
#' @param expo_name_df dataframe containing variable names for exposure values
#' @export
importAllExposureData <- function(input,output,session,expo_name_df){
  ns <- session$ns
  returnValues <- reactiveValues()
  returnValues$retdata <- c("No")
  file_paths <- reactiveValues(batch = NULL, tra = NULL, seem = NULL, sheds = NULL)
  batch_values <- reactiveValues()
  tra_values <- reactiveValues()
  seems_values <- reactiveValues()
  sheds_values <- reactiveValues()
  
  ## Import DataBase Write Functions ##
  
  write2ExposureSet <- function(name, description){
    queryId <- sprintf("SELECT expoid FROM ExposureSet ORDER BY expoid DESC LIMIT 1;")
    expoid <- projectDbSelect(queryId)$expoid
    if (length(expoid) == 0){expoid = 1}
    else {expoid <- expoid + 1}
    queryUpdate <- sprintf("insert into ExposureSet (expoid, name, descrp) values (%d, '%s', '%s');",
                           expoid, name, description)
    projectDbUpdate(queryUpdate)
    return(expoid)
  }
  
  ## Import Batch Data ##
  observeEvent(input$batchExposure, ignoreInit = TRUE, {
    expo_file <- reactive({   
      input$batchExposure
    })
    data_file_path <- reactive({
      validate(need(input$batchExposure,"No File Uploaded"))
      return(expo_file()$datapath)
    })
    file_paths$batch <- data_file_path
    
    oral_tble <- reactive({
      data <- readxl::read_excel(data_file_path(),sheet = "Oral")
      return(data)
    })
    inh_tble <- reactive({
      data <- readxl::read_excel(data_file_path(),sheet = "Inhalation")
      return(data)
    })
    dw_tble <- reactive({
      data <- readxl::read_excel(data_file_path(),sheet = "Drinking Water")
      return(data)
    })
    iv_tble <- reactive({
      data <- readxl::read_excel(data_file_path(),sheet = "Intravenous")
      return(data)
    })
    

    output$oralDT <- DT::renderDT(DT::datatable(oral_tble(),
                                                autoHideNavigation = T,
                                                fillContainer = T,rownames = F),server = T)
    output$inhDT <- DT::renderDT(DT::datatable(inh_tble(),
                                               autoHideNavigation = T,
                                               fillContainer = T,rownames = F),server = T)
    output$dwDT <- DT::renderDT(DT::datatable(dw_tble(),
                                              autoHideNavigation = T,
                                              fillContainer = T,rownames = F),server = T)
    output$ivDT <- DT::renderDT(DT::datatable(iv_tble(),
                                              autoHideNavigation = T,
                                              fillContainer = T,rownames = F),server = T)
    batch_values$oral_tble <- oral_tble
    batch_values$inh_tble <- inh_tble
    batch_values$dw_tble <- dw_tble
    batch_values$iv_tble <- iv_tble
  })
  
  ## Import TRA Data ##
  observeEvent(input$expoFile_upload , ignoreInit = TRUE,{
    # The selected file
    expoFile <- reactive({
      input$expoFile_upload
    })
    
    file_paths$tra <- expoFile()$datapath
    
    # The user's data, parsed into a data frame
    expoData <- reactive({
      if(!(is.null(input$expoFile_upload))){
        data_path <- expoFile()$datapath
        out_list <- parseTRAFile(data_path)
      }else{
        out_list <- "Nothing Uploaded"
      }
      return(out_list)
    })
    
    observe({
      if(is.list(expoData())){
        output$file_path <- renderText({"File Uploaded"})
        exposureNames <-expoData()$exponames
        # updatePickerInput(session,"sel_expo",
        #                   choices = exposureNames)
        updatePickerInput(session,"sel_export",
                          choices = exposureNames)
        shinyWidgets::updatePickerInput(session,"inh_export",
                                        choices = exposureNames$Inhalation)
        #inhalation data
        inh_colnames <- colnames(expoData()$inh)[c(1,4,7,8,11,12)]
        shinyWidgets::updatePrettyCheckboxGroup(session,
                                                "ch_inh",
                                                choices = inh_colnames,
                                                selected = "Exposure Name")
        #oral data
        oral_colnames <- colnames(expoData()$oral)[c(1,3,5,8,9)]
        shinyWidgets::updatePrettyCheckboxGroup(session,
                                                "ch_oral",
                                                choices = oral_colnames,
                                                selected = "Exposure Name")
        #dermal data
        # dermal_colnames <- colnames(expoData()$dermal)[c(1,3,5,6,8,9)]
        # shinyWidgets::updatePrettyCheckboxGroup(session,
        #                                         "ch_dermal",
        #                                         choices = dermal_colnames,
        #                                         selected = "Exposure Name")
        
      }
      
    })
    
    # observeEvent(input$sel_expo,{
    #   expoid <- input$sel_expo
    #   if(grepl("inh",expoid)){
    #     data<- expoData()$inh
    #     data <- data[which(data$ids == expoid),c(1,4,7,12)]
    #   }else if(grepl("oral",expoid)){
    #     data<- expoData()$oral
    #     data <- data[which(data$ids == expoid),c(1,5,9)]
    #   }else if(grepl("dermal",expoid)){
    #     data <- expoData()$dermal
    #     data <- data[which(data$ids == expoid),c(1,5,10)]
    #   }
    #   output$expo_table <-  DT::renderDT(DT::datatable(data,
    #                                                    options = list(dom = "t")))
    #   
    # },
    # ignoreInit = TRUE,
    # ignoreNULL = TRUE)
    
    output$file_path <- renderText({expoData()})
    tra_values$expoData <- expoData
    tra_values$expoFile <- expoFile
  })

  ## Import SEEMS Data ##
  observeEvent(input$btn_SEEM_data_file, ignoreInit = TRUE, {
    #showModal(modalDialog(title = "Button Works!"))
    fpath <- fpath_seem()
    file_paths$seem <- fpath
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
    seems_values$set_table_name <- set_table_name
    seems_values$id_name <- id_name
    seems_values$id_num <- id_num
    seems_values$vals_table_name <- vals_table_name
    })
  
  fpath_seem <- reactive({
    fpath <- tcltk::tk_choose.files(multi = F)
    return(fpath)
  })

  ## Import SHEDS-HT Data ##
  observeEvent(input$btn_SHEDS_data_file,ignoreInit = TRUE,{
               path <- fpath_sheds() 
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
                file_paths$sheds <- path2output
                observeEvent(input$sel_scene,{
                  scenario <- input$sel_scene
                  chem_list <-list.files(file.path(path2output,scenario))
                  chem_options <- gsub(".csv","",gsub("CAS_","",chem_list))
                  updatePickerInput(session,"sel_chem",choices = chem_options)
                },
                ignoreInit = T,ignoreNULL = T)
                
                })
  
  fpath_sheds <- reactive({
    fpath <- rstudioapi::selectDirectory("Select SHEDS-HT Folder")
    return(fpath)
  })
  
  
  ## Import All Button
  observeEvent(input$importAll,{
    #Batch Working
    #print(paste("batch:", file_paths$batch))
    if (!is.null(file_paths$batch)){
      oral_tble <- isolate(batch_values$oral_tble)
      inh_tble <- isolate(batch_values$inh_tble)
      dw_tble <- isolate(batch_values$dw_tble)
      iv_tble <- isolate(batch_values$iv_tble)
      oral_rows <- input$oralDT_rows_selected
      #print(oral_rows)
      inh_rows <- input$inhDT_rows_selected
      dw_rows <- input$dwDT_rows_selected
      iv_rows <- input$ivDT_rows_selected
      
      if (all(is.null(c(oral_rows,inh_rows,dw_rows,iv_rows)))){
        #print(oral_rows)
        shinyWidgets::sendSweetAlert(session,"No Exposure Selected",type = "error")
      }else{
        # parse Oral exposures and write to database
        for (i in oral_rows){
          print(i)
          data <- as.data.frame(oral_tble()[i,],stringsAsFactors = F)
          print(data)
          colnames(data)<- c("Name","bdose","blen","breps","brep_flag")
          name <- data$Name
          print(name)
          id_num <- getNextID("ExposureSet")
          descrp <- "Imported from batch file"
          query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                           "ExposureSet",
                           "expoid",
                           id_num,
                           name,
                           descrp)
          #print(query)
          projectDbUpdate(query)

          var_names <- expo_name_df$Var
          data2write <- setNames(rep(0,length(var_names)),var_names)
          data2write["expo_sidebar"]<-"oral"
          data2write["bdose"]<- data$bdose
          data2write["blen"]<- data$blen
          data2write["breps"]<- data$breps
          data2write["brep_flag"]<- ifelse(data$brep_flag == "Yes","TRUE","FALSE")
          vals <- paste0("'",as.character(data2write),"'")

          all_values_string <- paste(paste0(sprintf('(%d,',id_num),
                                            sprintf("'%s'",var_names),
                                            ',',vals,')'),
                                     collapse = ", ")
          write_col_names <- sprintf("%s, param, value","expoid")
          query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                           "Exposure",
                           write_col_names,
                           all_values_string)
          #print(query)
          projectDbUpdate(query)


        }
        # parse Oral exposures and write to database
        for (i in dw_rows){
          #  print(i)
          data <- as.data.frame(oral_tble()[i,],stringsAsFactors = F)
          # print(data)
          colnames(data)<- c("Name","drdose","dreps","vdw")
          name <- data$Name
          # print(name)
          id_num <- getNextID("ExposureSet")
          descrp <- "Imported from batch file"
          query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                           "ExposureSet",
                           "expoid",
                           id_num,
                           name,
                           descrp)
          # print(query)
          projectDbUpdate(query)

          var_names <- expo_name_df$Var
          data2write <- setNames(rep(0,length(var_names)),var_names)
          data2write["expo_sidebar"]<-"dw"
          data2write["drdose"]<- data$drdose
          data2write["dreps"]<- data$dreps
          data2write["vdw"]<- data$vdw

          vals <- paste0("'",as.character(data2write),"'")

          all_values_string <- paste(paste0(sprintf('(%d,',id_num),
                                            sprintf("'%s'",var_names),
                                            ',',vals,')'),
                                     collapse = ", ")
          write_col_names <- sprintf("%s, param, value","expoid")
          query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                           "Exposure",
                           write_col_names,
                           all_values_string)
          # print(query)
          projectDbUpdate(query)
        }

        # parse Inhalation exposures and write to database
        for (i in inh_rows){
          #print(i)
          data <- as.data.frame(oral_tble()[i,],stringsAsFactors = F)
          #print(data)
          colnames(data)<- c("Name","inhdose","inhtlen","inhdays")
          name <- data$Name
          #print(name)
          id_num <- getNextID("ExposureSet")
          descrp <- "Imported from batch file"
          query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                           "ExposureSet",
                           "expoid",
                           id_num,
                           name,
                           descrp)
          #print(query)
          projectDbUpdate(query)

          var_names <- expo_name_df$Var
          data2write <- setNames(rep(0,length(var_names)),var_names)
          data2write["expo_sidebar"]<-"inh"
          data2write["inhdose"]<- data$inhdose
          data2write["inhtlen"]<- data$inhtlen
          data2write["inhdays"]<- data$inhdays

          vals <- paste0("'",as.character(data2write),"'")

          all_values_string <- paste(paste0(sprintf('(%d,',id_num),
                                            sprintf("'%s'",var_names),
                                            ',',vals,')'),
                                     collapse = ", ")
          write_col_names <- sprintf("%s, param, value","expoid")
          query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                           "Exposure",
                           write_col_names,
                           all_values_string)
          #print(query)
          projectDbUpdate(query)

        }

        # parse Intravenous exposures and write to database
        for (i in oral_rows){
          # print(i)
          data <- as.data.frame(oral_tble()[i,],stringsAsFactors = F)
          #print(data)
          colnames(data)<- c("Name","ivdose","ivlen","ivrep_flag")
          name <- data$Name
          # print(name)
          id_num <- getNextID("ExposureSet")
          descrp <- "Imported from batch file"
          query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                           "ExposureSet",
                           "expoid",
                           id_num,
                           name,
                           descrp)
          # print(query)
          projectDbUpdate(query)

          var_names <- expo_name_df$Var
          data2write <- setNames(rep(0,length(var_names)),var_names)
          data2write["expo_sidebar"]<-"iv"
          data2write["ivdose"]<- data$ivdose
          data2write["ivlen"]<- data$ivlen
          data2write["ivrep_flag"]<- ifelse(data$ivrep_flag == "Yes","TRUE","FALSE")
          vals <- paste0("'",as.character(data2write),"'")

          all_values_string <- paste(paste0(sprintf('(%d,',id_num),
                                            sprintf("'%s'",var_names),
                                            ',',vals,')'),
                                     collapse = ", ")
          write_col_names <- sprintf("%s, param, value","expoid")
          query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                           "Exposure",
                           write_col_names,
                           all_values_string)
          # print(query)
          projectDbUpdate(query)


        }}}
    #TRA mostly working need to know what to put in database
    if (!is.null(file_paths$tra)) {
      expoFile <- isolate(tra_values$expoFile)
      expoData <- isolate(tra_values$expoData)
      inh_exposure <- data.frame()
      oral_exposure <- data.frame()
      sel_list <- input$sel_export
      for (ids in sel_list){
        if(grepl("inh",ids)){
          data<- expoData()$inh
          data <- as.data.frame(data[which(data$ids == ids),c(1,4,7,12)])
          inh_exposure <- rbind(inh_exposure,data)
        }else if(grepl("oral",ids)){
          data<- expoData()$oral
          data <- data[which(data$ids == ids),c(1,5,9)]
          oral_exposure <- rbind(oral_exposure,data)
        }else if(grepl("dermal",ids)){
          data <- expoData()$dermal
          data <- data[which(data$ids == ids),c(1,5,10)]
        }
        #write.csv(inh_exposure,file.path(base_path,"inhalation_exposure.csv"),row.names = F)
        #write.csv(oral_exposure,file.path(base_path,"oral_exposure.csv"),row.names = F)
      }
      if (nrow(inh_exposure)>0){
      for (n in 1:nrow(inh_exposure)){
         print(inh_exposure[n,])
        print(inh_exposure[n,1])
        expoid <- write2ExposureSet(inh_exposure[n,1], "imported from TRA")
         var_names <- expo_name_df$Var
          data2write <- setNames(rep(0,length(var_names)),var_names)
          data2write["expo_sidebar"]<-"inh"
          data2write["inhdose"]<- inh_exposure[n,4] * 24.45 / input$TRA_MW ## Assuming STP and mg/m^3
          data2write["inhtlen"]<- as.numeric(inh_exposure[n,2]) * as.numeric(inh_exposure[n,3])
          data2write["inhdays"]<- isolate(input$TRA_inhalation_week)
          vals <- paste0("'",as.character(data2write),"'")
          
          all_values_string <- paste(paste0(sprintf('(%d,',expoid),
                                            sprintf("'%s'",var_names),
                                            ',',vals,')'),
                                     collapse = ", ")
          write_col_names <- sprintf("%s, param, value","expoid")
          query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                           "Exposure",
                           write_col_names,
                           all_values_string)
          projectDbUpdate(query)
      }}
      if (nrow(oral_exposure)>0){
      for (n in 1:nrow(oral_exposure)){
        print(oral_exposure[n,])
        # ## Start New
        # var_names <- expo_name_df$Var
        # data2write <- setNames(rep(0,length(var_names)),var_names)
        # data2write["expo_sidebar"]<-"oral"
        # data2write["bdose"]<- data$bdose
        # data2write["blen"]<- data$blen
        # data2write["breps"]<- data$breps
        # data2write["brep_flag"]<- ifelse(data$brep_flag == "Yes","TRUE","FALSE")
        # vals <- paste0("'",as.character(data2write),"'")
        # 
        # all_values_string <- paste(paste0(sprintf('(%d,',id_num),
        #                                   sprintf("'%s'",var_names),
        #                                   ',',vals,')'),
        #                            collapse = ", ")
        # write_col_names <- sprintf("%s, param, value","expoid")
        # query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
        #                  "Exposure",
        #                  write_col_names,
        #                  all_values_string)
        # #print(query)
        # projectDbUpdate(query)
        # ## End New
      }}
    }
    #SEEM Working
    print(paste("seem:", file_paths$seem))
    if (!is.null(file_paths$seem)){
      ## Missing SEEMS Values
      set_table_name <- isolate(seems_values$set_table_name)
      id_name <- isolate(seems_values$id_name)
      id_num <- isolate(seems_values$id_num)
      vals_table_name <- isolate(seems_values$vals_table_name)
      
      chem_list <- input$chems
      query <- sprintf("Select CAS,preferred_name from ChemData where catid == '%s';",
                       input$seem_filter)
      path <- isolate(file_paths$seem)
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
          
        }}}
    #SHEDS Need to know which values from the file to import
    if (!is.null(file_paths$sheds)){
      chem_list <- input$sel_chem
      fpath <- isolate(file_paths$sheds)
    for (each_chem in chem_list){
      file_name <- paste0(fpath, "/", input$sel_scene,"/CAS_",each_chem,".csv")
      print(file_name)
      fileFrame <- read.csv(file_name)
      print(fileFrame)
    }}
  })
  returnValues$retdata<- eventReactive(input$importAll,{return(c("Yes"))})
}

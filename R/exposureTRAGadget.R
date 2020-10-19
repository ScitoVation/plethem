# Gadget UI and server for importing data run using Commercial TRA version
#
exposureTRAGadget <- function(save_flag = F, base_path = NULL){
  ui <- miniPage(
    shinyjs::useShinyjs(),
    gadgetTitleBar("Process Exposures Estimates from TRA",right = miniTitleBarButton("save","Export Selected")),
    miniPage(
      miniContentPanel(
        fillCol(flex = c(2,2,3,2),
                fillRow(
                  fileInput("expoFile_upload",
                            label = "Upload Exposure Excel File",
                            multiple = F,
                            buttonLabel = "Browse")
                ),
                fillRow(
                  pickerInput("sel_expo",
                                 label= "Select Exposure",
                                 width = validateCssUnit("600px"),
                                 choices = NULL)
                ),
                fillRow(
                  DT::DTOutput("expo_table")
                ),
                fillCol(flex = c(8,2),
                        fillRow(
                          pickerInput("sel_export","Select exposures to export",
                                      choices = NULL,multiple = TRUE
                        )
                        )
                )
      )
    )
  )
  #   miniTabstripPanel(id = "menu",
  #                     between = tagList(fileInput("expoFile_upload",
  #                                                 label = "Upload Exposure Excel File",
  #                                                 multiple = F,
  #                                                 buttonLabel = "Browse")),
  #                     miniTabPanel(title = "View",value = "view",
  #                                  miniContentPanel(
  #                                    fillCol(flex = c(2,8),
  #                                            
  #                                            fillRow(
  #                                              selectizeInput("sel_expo",
  #                                                             label= "Select Exposure",
  #                                                             width = validateCssUnit("600px"),
  #                                                             choices = NULL)
  #                                            ),
  #                                            fillRow(
  #                                              DT::DTOutput("expo_table")
  #                                            )
  #                                    )
  #                                    
  #                                  )
  #                     ),
  #                     miniTabPanel(title = "Save Data", value = "save",
  #                                  miniContentPanel(
  #                                    fillCol(flex = c(1,1),
  #                                            fillRow(
  #                                              shinyWidgets::prettyCheckboxGroup("ch_inh",
  #                                                                                "Inhalation Exposure",
  #                                                                                width = validateCssUnit("30%"),
  #                                                                                choices = c("Nothing Uploaded")),
  #                                              shinyWidgets::awesomeCheckboxGroup("ch_oral",
  #                                                                                 "Oral Exposure",
  #                                                                                 choices = c("Nothing Uploaded"))#,
  #                                              # shinyWidgets::awesomeCheckboxGroup("ch_dermal",
  #                                              #                                    "Dermal Exposure",
  #                                              #                                    choices = c("Nothing Uploaded"))
  #                                            ),
  #                                            fillRow(
  #                                              shinyWidgets::pickerInput("inh_export",
  #                                                                        "Select Exposures",
  #                                                                        choices = "",multiple = TRUE
  #                                                                        options = list(`actions-box`=TRUE,
  #                                                                                       `dropupAuto`=TRUE,
  #                                                                                       `selected-text-format` = "count > 1")),
  #                                              shinyWidgets::pickerInput("oral_export",
  #                                                                        "Select Exposures",
  #                                                                        choices = "",multiple = TRUE
  #                                                                        
  #                                                                        options = list("action-box"=TRUE,
  #                                                                                       "dropupAuto"=TRUE))#,
  #                                              # shinyWidgets::pickerInput("dermal_export",
  #                                              #                           "Select Exposures",
  #                                              #                           choices = "",multiple = TRUE
  #                                              #                           options = list('action-box'=TRUE,
  #                                              #                                          'dropupAuto'=TRUE))
  #                                            )
  #                                            
  #                                    )
  #                                    
  #                                    
  #                                  ))
  #   )
  #   
  # )
  # 
  server <- function(input,output,session){
    # The selected file
    expoFile <- reactive({
      input$expoFile_upload
    })
    
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
        updatePickerInput(session,"sel_expo",
                             choices = exposureNames)
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
    
    observeEvent(input$sel_expo,{
      expoid <- input$sel_expo
      if(grepl("inh",expoid)){
        data<- expoData()$inh
        data <- data[which(data$ids == expoid),c(1,4,7,12)]
      }else if(grepl("oral",expoid)){
        data<- expoData()$oral
        data <- data[which(data$ids == expoid),c(1,5,9)]
      }else if(grepl("dermal",expoid)){
        data <- expoData()$dermal
        data <- data[which(data$ids == expoid),c(1,5,10)]
      }
      output$expo_table <-  DT::renderDT(DT::datatable(data,
                                                       options = list(dom = "t")))
      
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE)
    
    output$file_path <- renderText({expoData()})
    observeEvent(input$save,{
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
        write.csv(inh_exposure,file.path(base_path,"inhalation_exposure.csv"),row.names = F)
        write.csv(oral_exposure,file.path(base_path,"oral_exposure.csv"),row.names = F)
        stopApp()
      }
      
    })
  }
  runGadget(ui,server,
            viewer =dialogViewer("TRA",width = 1000,height = 1000))
}


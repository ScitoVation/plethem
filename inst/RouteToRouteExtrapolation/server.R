#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# ROUTE TO ROUTE EXTRAPOLATION

library(shiny)
library(RSQLite)
library(DT)
library(shinyWidgets)
library(magrittr)
library(dplyr)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(readr)
library(tidyr)
library(htmltools)

`%then%` <- shiny:::`%OR%`

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  results <- reactiveValues(pbpk=NULL,simid = NULL,mode = NULL)
  model <- "rapidPBPK"
  mcvals <- reactiveValues(
    csvFile = NULL,
    name = NULL
  )
  bmvals <- reactiveValues(
    csvFile = NULL,
    name = NULL
  )
  shinyjs::useShinyjs()
  dataset <- reactiveValues()
  dataset$savedat <- reactiveVal(c("No","none"))
  
  selectProjModal <- function() {
    modalDialog(
      useShinyjs(),
      title = "Upload Project",
      easyClose = F,#TRUE,
      size = "m",
      tagList(
        tags$style(
          type='text/css',
          '.modal-title {
            text-align: center;
          }'
        ),
        useSweetAlert(),
        fluidRow(
          column(
            12,
            # offset = 2,
            # align = 'center',
            fileInput(
              "rDataFile",
              label = "Select Project file",
              accept = c(".RData", ".Rdata"),
              placeholder = 'Upload .RData file',
              multiple = F
            ),
            pickerInput(
              'simulation',
              'Select Simulation',
              choices = NULL,
              selected = NULL,
              multiple = F
            )
          )
        ),
        fluidRow(
          progressBar(id = "bmProgress",value = 0, status = "success",striped = T)
        )
        # tabsetPanel(
        #   id = 'modalNav',
        #   tabPanel(
        #     title = 'Upload Existing Results',
        #     br(),
        #     textInput(
        #       "mcname",
        #       "Dataset Name",
        #       placeholder = "Enter name for the dataset"
        #     ),
        #     fluidRow(
        #       column(
        #         6,
        #         shinyWidgets::radioGroupButtons(
        #           "type",
        #           "Select Exposure Type",
        #           choices = c("Inhalation", "Oral", "IV", "Dermal")
        #         )
        #       ),
        #       column(
        #         6,
        #         uiOutput(
        #           "unit_ui"
        #         )
        #       )
        #     ),
        #     fileInput(
        #       "csvFile",
        #       label = "Upload Monte Carlo Results",
        #       accept = c("text/csv","text/comma-separated-values",".csv"),
        #       multiple = TRUE
        #     ),
        #     tags$h4("Results are displayed in mg/L")
        #   ),
        #   tabPanel(
        #     title = 'Run Monte Carlo Simulation',
        #     br(),
        #     fileInput(
        #       "rDataFile",
        #       label = "Select Project file",
        #       accept = c(".RData", ".Rdata"),
        #       placeholder = 'Upload .RData file',
        #       multiple = F
        #     ),
        #     pickerInput(
        #       'simulation',
        #       'Select Simulation',
        #       choices = NULL,
        #       selected = NULL,
        #       multiple = F
        #       # ,options = list(
        #       # 'live-search' = TRUE,
        #       # 'actions-box' = TRUE,
        #       # 'selected-text-format' = 'count > 2',
        #       # 'count-selected-text'='{0} simulations selected'
        #       # )
        #     ),
        #     fluidRow(
        #       column(
        #         6,
        #         shinyWidgets::radioGroupButtons(
        #           'tissue',
        #           label = "Select Tissue Type",
        #           choices = c('Plasma', 'Urine')
        #         )
        #       ),
        #       column(
        #         6,
        #         shinyWidgets::radioGroupButtons(
        #           "chemType",
        #           "Select Chemical Type",
        #           choices = c("Parent", "Metabolite")
        #         )
        #       )
        #     ),
        #     fluidRow(
        #       column(
        #         6,
        #         sliderInput(
        #           'mySlider2',
        #           label = 'Exposure Type (Units)',
        #           min = 0,
        #           max = 1000,
        #           value = c(0,1000)
        #         )
        #       ),
        #       column(
        #         6,
        #         numericInput(
        #           'mcNumeric',
        #           label = 'Number of Doses',
        #           min = 20,
        #           max = 50,
        #           value = 25,
        #           step = 1,
        #           width = '156.84px'
        #         ),
        #         uiOutput(
        #           'validNum'
        #         )
        #       )
        #     ),
        #     fluidRow(
        #       progressBar(id = "pb",value = 0, status = "success",striped = T)
        #     )
        #   )
        # )
      ),
      footer= tagList(
        # uiOutput('mcFooter', inline = T),
        shinyjs::disabled(actionButton("runSim","Run")), # Used to be addBM
        modalButton('Cancel')
      )
    )
  }
  setRouteModal <- function() {
    
    expoTypes <- c('Oral', 'Drinking Water', 'Inhalation', 'IV', 'Dermal')
    if(myExpoid == 'oral'){
      myExpoType = 'Oral'
    } else if(myExpoid == 'dw'){
      myExpoType = 'Drinking Water'
    } else if(myExpoid == 'inh'){
      myExpoType = 'Inhalation'
    } else if(myExpoid == 'iv'){
      myExpoType = 'IV'
    } else if(myExpoid == 'derm'){
      myExpoType = 'Dermal'
    }
    
    if(is.null(input$simulation)){
      exposureChoices <- expoTypes
    } else{
      exposureChoices <- setdiff(expoTypes, myExpoType)
    }
    
    modalDialog(
      useShinyjs(),
      title = "Set Route of Exposure",
      easyClose = F,
      size = "l",
      tagList(
        tags$style(
          type='text/css',
          '.modal-title {
            text-align: center;
          }'
        ),
        fluidRow(
          column(
            12,
            pickerInput(
              'exposure',
              'Select exposure',
              choices = exposureChoices,
              options = list(
                title = 'Nothing selected'
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            uiOutput('exposureParams')
          )
        ),
        fluidRow(
          column(
            6,
            numericRangeInput(
              'mySlider2',
              label = 'Exposure Type (Units)',
              # min = 0,
              # max = 1000,
              value = c(0,1000)
            )
          ),
          column(
            6,
            numericInput(
              'mcNumeric',
              label = 'Number of exposures',
              min = 20,
              max = 50,
              value = 25,
              step = 1,
              width = '230px'
            ),
            uiOutput(
              'validNum'
            )
          )
        )
        
        # textInput(
        #   "bmname",
        #   "Dataset Name",
        #   placeholder = "Enter name for the dataset"
        # ),
        # shinyWidgets::radioGroupButtons(
        #   "bmtype",
        #   "Select Type",
        #   choices = c("Parent", "Metabolite")
        # ),
        # fileInput(
        #   "bmFile",
        #   label = "Upload Biomonitoring CSV File",
        #   accept = c("text/csv","text/comma-separated-values",".csv"),
        #   multiple = TRUE
        # ),
        # tags$h4(tags$span(style='color:red', 'IMPORTANT:'), ' Biomonitoring results must be in mg/L', sep = '')
      ),
      footer= tagList(
        shinyjs::disabled(actionButton("runExposure","Run")),
        modalButton("Cancel")
      )
    )
  }
  
  observeEvent(input$exposure,{
    shinyjs::enable('runExposure')
    if(input$exposure == 'Oral'){
      expoParams <- renderUI({
        tagList(
          fluidRow(
            column(
              6,
              numericInput(
                'blen',
                label = 'Total length of dosing (h/day)',
                min = 0,
                max = 24,
                value = 1,
                step = 1
              )
            ),
            column(
              6,
              numericInput(
                'breps',
                label = 'Number of doses',
                min = 0,
                max = 100,
                value = 1,
                step = 1,
                width = '230px'
              )
            )
          )
        )
      })
      
    } else if(input$exposure == 'Drinking Water'){
      expoParams <- renderUI({
        tagList(
          fluidRow(
            column(
              4,
              numericInput(
                'vdw',
                label = 'Volume of drinking water (L)',
                min = 0,
                max = 100,
                value = 1,
                step = 1
              )
            ),
            column(
              4,
              numericInput(
                'dreps',
                label = 'Number of drinking water doses per day',
                min = 0,
                max = 100,
                value = 1,
                step = 1
              )
            ),
            column(
              4,
              style = 'margin-top: 25px',
              awesomeCheckbox(
                'brep_flag',
                label = 'Repeat oral dose',
                value = F
              )
            )
          )
        )
      })
      
    } else if(input$exposure == 'Inhalation'){
      expoParams <- renderUI({
        tagList(
          fluidRow(
            column(
              6,
              numericInput(
                'inhtlen',
                label = 'Length of inhalation dose (h)',
                min = 0,
                max = 24,
                value = 1,
                step = 1
              )
            ),
            column(
              6,
              numericInput(
                'inhdays',
                label = 'Number of doses per week (days)',
                min = 0,
                max = 7,
                value = 1,
                step = 1,
                width = '230px'
              )
            )
          )
        )
      })
    } else if(input$exposure == 'IV'){
      expoParams <- renderUI({
        tagList(
          fluidRow(
            column(
              6,
              numericInput(
                'ivlen',
                label = 'Length of intravenous dose (h/day)',
                min = 0,
                max = 24,
                value = 1,
                step = 1
              )
            ),
            column(
              6,
              style = 'margin-top: 25px',
              awesomeCheckbox(
                'ivrep_flag',
                label = 'Repeat IV dose',
                value = F
              )
            )
          )
        )
      })
      
    } else if(input$exposure == 'Dermal'){
      expoParams <- renderUI({
        tagList(
          fluidRow(
            column(
              4,
              numericInput(
                'dermlen',
                label = 'Length of dermal dosing per day (h)',
                min = 0,
                max = 24,
                value = 1,
                step = 1
              )
            ),
            column(
              4,
              numericInput(
                'skarea',
                label = 'Exposed skin area (cm\U00B2)',
                min = 0,
                max = 100,
                value = 1,
                step = 1
              )
            ),
            column(
              4,
              style = 'margin-top: 25px',
              awesomeCheckbox(
                'dermrep_flag',
                label = 'Repeat dermal dose daily',
                value = F
              )
            )
          )
        )
      })
    } else{
      expoParams <- renderUI({
        br()
      })
    }
    output$exposureParams <- expoParams
  })
  
  mcNum <- reactive({
    validate(
      need(input$mcNumeric > 19, 'Invalid input. Please enter a number 20-50.') %then%
        need(input$mcNumeric < 51, 'Invalid input. Please enter a number 20-50.')
    )
  })
  
  output$validNum <- renderUI({
    mcNum()
  })
  
  observeEvent(input$mcNumeric, {
    if(input$mcNumeric < 20 | input$mcNumeric > 50){
      shinyjs::disable('runExposure')
    } else{
      if(!is.null(input$simulation)){
        shinyjs::enable('runExposure')
      }
    }
  })
  
  observeEvent(input$csvFile, {
    shinyjs::enable("add")
  })
  
  observeEvent(input$rDataFile, {
    # shinyjs::enable('runExposure')
    inFile <- input$rDataFile
    rDFile <- inFile$datapath
    # e = new.env()
    # name <<- load(rDFile, envir = e)
    # data <- e[['name']]
    load(rDFile, envir = .GlobalEnv)
    loadReverseDosimetryProject(rDFile)
    simSet <<- SimulationsSet %>%
      filter(
        physiovarid > 0 |
          chemvarid > 0 |
          expovarid > 0
      ) 
    # simSet2 <<- simSet$name
    updatePickerInput(
      session,
      'simulation',
      selected = 0,
      choices = simSet$name,
      choicesOpt = list(
        subtext = simSet$descrp
      )
    )
    
  })
  
  observeEvent(input$runExposure, {
    confirmSweetAlert(
      session = session,
      inputId = "myconfirmation",
      type = "warning",
      title = "Are you sure you want to run a simulation?",
      text = "This may take a while to complete."
    )
  })
  vol_ids <- c("fat"="vfatc","skin"="vskinc",
               "muscle"="vmuscc","bone"="vbonec",
               "brain"="vbrnc","lung"="vlngc",
               "heart"="vhrtc","gi"="vgic",
               "liver"="vlivc","kidney"="vkdnc",
               "rpf"="vrpfc","spf"="vspfc","blood"="vbldc")
  # ,"bw"="bw")
  
  observeEvent(input$myconfirmation, {
    if(isTRUE(input$myconfirmation)){
      # print(paste('max = ',input$mySlider2[2]))
      # nDoses <- 3#input$mcNumeric
      if(myExpoid == 'oral'){
        whichDose = 'bdose'
        doseName = 'Oral'
        doseUnits = 'mg/kg BW/day'
      } else if(myExpoid == 'dw'){
        whichDose = 'drdose'
        doseName = 'Drinking Water'
        doseUnits = 'mg/L'
      } else if(myExpoid == 'inh'){
        whichDose = 'inhdose'
        doseName = 'Inhalation'
        doseUnits = 'ppm'
      } else if(myExpoid == 'iv'){
        whichDose = 'ivdose'
        doseName = 'IV'
        doseUnits = 'mg/L'
      } else if(myExpoid == 'derm'){
        whichDose = 'dermrate'
        doseName = 'Dermal'
        doseUnits = '\U00B5m/n/cm\U00B2'
      } else if(myExpoid == 'oralv'){
        whichDose = 'bdosev'
        doseName = 'Oral Vehicle'
        doseUnits = 'mg/kg BW/day'
      } else whichDose = 'Something went wrong'
      
      ## observeEvent(input$run_sim,{
      simid <- simSet3$simid[1]
        results$simid <- simid
        # get the parameters needed to run the model
        model_params <<- getAllParamValuesForModel(simid,model)
        #get total volume
        active_comp <- c("skin","fat","muscle","bone","brain","lung","heart","gi","liver","kidney","rpf","spf")
        vol_comps <- c(active_comp,"blood")
        total_vol <- 1#sum( #COME BACK TO #######################################
        #   unlist(
        #     lapply(
        #       vol_comps,
        #       function(x){
        #         input[[vol_ids[x]]]
        #       })
        #   )
        # )
        # test_vol_comps <<- vol_comps
        # test_total_Vol <<- total_vol
        # test_vol_ids <<- vol_ids
        query <- sprintf("Select mc_num From SimulationsSet where simid = %i",simid)
        mc_num <<- as.integer(projectDbSelect(query)$mc_num)
        # print(paste('mc_num: ',mc_num))
        model_params$vals[["total_vol"]]<- total_vol
        # print(total_vol)
        # print(paste(whichDose, ': ', model_params$vals[[whichDose]]))
        if (mc_num > 1){
          MC.matrix <<- getAllVariabilityValuesForModel(simid,model_params$vals,mc_num)
          query <- sprintf("Select model_var from ResultNames where mode = 'MC' AND model = '%s'",
                           model)
          mc_vars<- mainDbSelect(query)$model_var
          mc_results <- lapply(mc_vars,function(x,n){
            return(x = rep(NA,n))
          },mc_num)
          names(mc_results)<- mc_vars
          
          currentDose <- Exposure$vals[[whichDose]]#input$mySlider2[1]
          # if(currentDose == 0){
          #   currentDose = 0.05
          # }
          # maxDose <- input$mySlider2[2]
          # increaseDose <- (input$mySlider2[2]/currentDose)^(1/(nDoses-1))
          # print(increaseDose)
          # for(n in 1:(nDoses)){
            # print(paste('Running monte carlo simulation ', n ))
            # print(currentDose)
            # model_params$vals[[whichDose]] <- currentDose
            
            
            for (i in 1:mc_num){
              model_params$vals[colnames(MC.matrix)]<- MC.matrix[i,]
              initial_values <<- calculateInitialValues(model_params)
              tempDF <- runFDPBPK(initial_values,model)
              max_list <- unlist(lapply(mc_vars,function(x,data){
                var_name <- gsub("_max","",x)
                
                return(max(data[var_name]))
              },tempDF$pbpk))
              names(max_list)<- mc_vars
              for (x in mc_vars){
                mc_results[[x]][[i]]<- max_list[[x]]
              }
              updateProgressBar(session,"pb",value = ((1-1)*mc_num + i), total = mc_num*1)
            }
            
          
          results$pbpk <- as.data.frame(mc_results)
          # MymcResults <<- as.data.frame(mc_results)
          plasmaResults <- as.data.frame(results$pbpk$cpls_max)
          colnames(plasmaResults) = currentDose
          # pr2 <<- plasmaResults
          # plasmaResults <<- plasmaResults %>%
          #   rename(
          #     results$pbpk$cpls_max = 'DOSE'
          #   )
          
          # if(n == 1){
          #   mcResults2 <<- NULL
          # }
          # mcResults2[n] <<- plasmaResults[1]
          # if(n==1){#is.null(mcResults)){
          #   # print('it is null')
            mcResults <<- plasmaResults
          # } else{
          #   # print('something exists')
          #   mcResults <<- cbind(mcResults,plasmaResults)# %>%
          # }
          # mcResults <<- mcResults %>% 
          #   rename(
          #     results$pbpk$cpls_max = 'DOSE'
          #   )
          #   mutate(
          #     xCol = as.data.frame(mc_results['cpls_max'])
          #   )
          results$mode <- "MC"
          # currentDose = currentDose * increaseDose
        # }
      #     updateNavbarPage(session,"menu","output")
        }else{
          initial_values <- calculateInitialValues(model_params)

          updateProgressBar(session,"pb",value = 100, total = 100,
                            status = "info")
          tempDF <- runFDPBPK(initial_values,model)

          results$pbpk<- tempDF$pbpk


          results$mode <- "FD"
      #     updateNavbarPage(session,"menu","output")
        }
      ## })
      
      
      ########################################################################
      # print('Running Monte Carlo')
      # print(paste('Number of doses = ',input$mcNumeric, '!', sep = ''))
        
        updatemenus <- list(
          list(
            active = 0,
            type = 'buttons',
            buttons = list(
              list(
                label = 'Default',
                method = 'relayout',
                args = list(
                  list(
                    yaxis = list(
                      title = paste('Concentrations (mg/L)', sep = ''),
                      type = 'linear'
                    )
                  )
                )
              ),
              list(
                label = 'Log Y-Axis',
                method = 'relayout',
                args = list(
                  list(
                    yaxis = list(
                      title = paste('Log Concentrations (mg/L)', sep = ''),
                      type = 'log'
                    )
                  )
                )
              )
            )
          )
        )
        
        output$Plot1 <- renderPlotly({
          p <- plot_ly(
            stack(mcResults),
            x = ~ind,
            y = ~values,
            type = "box"
          ) %>%
            layout(
              title = paste(input$simulation, 'Monte Carlo Simulation'),
              yaxis = list(
                title = paste('Concentrations (mg/L)', sep = '')
              ),
              xaxis = list(
                title = paste(doseName, ' Exposure (', doseUnits,')', sep = '')
              ),
              margin = m,
              updatemenus = updatemenus
            )
        })
        
        
      removeModal()
    }
    else{
      print('Denied Monte Carlo Simulation')
    }
  })
  
  observeEvent(input$simulation, {
    shinyjs::enable('runSim')
    simSet3 <<- simSet %>%
      filter(
        name == input$simulation
      )
    exposureType <<- Exposure %>%
      filter(
        expoid == simSet3$expoid & 
          param == 'expo_sidebar'
      )
    myExpoid <<- exposureType$value[1]
    if(myExpoid == 'oral'){
      mySliderLabel = 'Oral (mg/kg BW/day)'
    } else if(myExpoid == 'dw'){
      mySliderLabel = 'Drinking Water (mg/L)'
    } else if(myExpoid == 'inh'){
      mySliderLabel = 'Inhalation (ppm)'
    } else if(myExpoid == 'iv'){
      mySliderLabel = 'IV (mg/L)'
    } else if(myExpoid == 'derm'){
      mySliderLabel = 'Dermal (\U00B5m/n/cm\U00B2)'
    } else if(myExpoid == 'oralv'){
      mySliderLabel = 'Oral Vehicle (mg/kg BW/day)'
    } else mySliderLabel = 'Unknown'
    
    # expoTypes <- c('Oral' = 'oral', 'Drinking Water' = 'dw', 'Inhalation' = 'inh', 'IV' = 'iv', 'Dermal' = 'derm')
    # updatePickerInput(
    #   session,
    #   'exposure',
    #   label = 'hello',
    #   selected = NULL,
    #   choices = expoTypes3
    #   # choicesOpt = list(
    #   #   subtext = simSet$descrp
    #   # )
    # )
    updateNumericRangeInput(
      session,
      'mySlider2',
      label = mySliderLabel,
      value = c(0,1000) # This line is needed to update the label because this would create subscript out of bounds error otherwise
    )
    # output$mySlider <- renderUI({
    #   # fluidRow(
    #   sliderInput(
    #     'mySlider',
    #     label = exposureType$value[1],
    #     min = 0,
    #     max = 1000,
    #     value = c(0,1000)
    #   )    
    #   # )
    # })
    
    # mySliderLabel <- simset3$expoid
    # updateNoUiSliderInput(
    #   session,
    #   'mySlider',
    #   label = 'simSet3'
    # )
  })
  
  observeEvent(input$bmFile, {
    shinyjs::enable("runSim")
  })
  
  observeEvent(input$selectProject,{
    showModal(selectProjModal())
  })
  
  observeEvent(input$setRoute, {
    # print(input$simulation)
    # shinyjs::disable('runExposure')
    showModal(setRouteModal())
    # print(input$exposure)
  })
  
  m  = list(
    # l = 10,
    # r = 10,
    b = 80,
    t = 80
    # ,pad = 10
  )
  
  # Actions on Add button in Upload Monte Carlo's Modal
  observeEvent(input$add, {
    mcvals$name <- input$mcname
    filesIn <- input$csvFile
    df_list <- lapply(filesIn$datapath,read_csv) # read each file into a data frame
    mcvals$csvFile <- bind_rows(df_list) # concatenates all of the data frames
    updatemenus <- list(
      list(
        active = 0,
        type = 'buttons',
        buttons = list(
          list(
            label = 'Default',
            method = 'relayout',
            args = list(
              list(
                yaxis = list(
                  title = paste('Concentrations (mg/L)', sep = ''),
                  type = 'linear'
                )
              )
            )
          ),
          list(
            label = 'Log Y-Axis',
            method = 'relayout',
            args = list(
              list(
                yaxis = list(
                  title = paste('Log Concentrations (mg/L)', sep = ''),
                  type = 'log'
                )
              )
            )
          )
        )
      )
    )
    
    output$Plot1 <- renderPlotly({
      p <- plot_ly(
        stack(mcvals$csvFile),
        x = ~ind,
        y = ~values,
        type = "box"
      ) %>%
        layout(
          title = mcvals$name,
          yaxis = list(
            title = paste('Concentrations (mg/L)', sep = '')
          ),
          xaxis = list(
            title = paste(input$type, ' Exposure (',input$unit,')', sep = '')
          ),
          margin = m,
          updatemenus = updatemenus
        )
    })
    
    removeModal()
  })
  
  # Actions on Add button in Upload Biomonitoring's Modal
  observeEvent(input$runSim, {
    
    
    # print(paste('max = ',input$mySlider2[2]))
    # nDoses <- 3#input$mcNumeric
    if(myExpoid == 'oral'){
      whichDose = 'bdose'
      doseName = 'Oral'
      doseUnits = 'mg/kg BW/day'
    } else if(myExpoid == 'dw'){
      whichDose = 'drdose'
      doseName = 'Drinking Water'
      doseUnits = 'mg/L'
    } else if(myExpoid == 'inh'){
      whichDose = 'inhdose'
      doseName = 'Inhalation'
      doseUnits = 'ppm'
    } else if(myExpoid == 'iv'){
      whichDose = 'ivdose'
      doseName = 'IV'
      doseUnits = 'mg/L'
    } else if(myExpoid == 'derm'){
      whichDose = 'dermrate'
      doseName = 'Dermal'
      doseUnits = '\U00B5m/n/cm\U00B2'
    } else if(myExpoid == 'oralv'){
      whichDose = 'bdosev'
      doseName = 'Oral Vehicle'
      doseUnits = 'mg/kg BW/day'
    } else whichDose = 'Something went wrong'
    
    ## observeEvent(input$run_sim,{
    simid <- simSet3$simid[1]
    results$simid <- simid
    # get the parameters needed to run the model
    model_params <<- getAllParamValuesForModel(simid,model)
    #get total volume
    active_comp <- c("skin","fat","muscle","bone","brain","lung","heart","gi","liver","kidney","rpf","spf")
    vol_comps <- c(active_comp,"blood")
    total_vol <- 1#sum( #COME BACK TO #######################################
    #   unlist(
    #     lapply(
    #       vol_comps,
    #       function(x){
    #         input[[vol_ids[x]]]
    #       })
    #   )
    # )
    # test_vol_comps <<- vol_comps
    # test_total_Vol <<- total_vol
    # test_vol_ids <<- vol_ids
    query <- sprintf("Select mc_num From SimulationsSet where simid = %i",simid)
    mc_num <<- as.integer(projectDbSelect(query)$mc_num)
    # print(paste('mc_num: ',mc_num))
    model_params$vals[["total_vol"]]<- total_vol
    # print(total_vol)
    # print(paste(whichDose, ': ', model_params$vals[[whichDose]]))
    if (mc_num > 1){
      MC.matrix <<- getAllVariabilityValuesForModel(simid,model_params$vals,mc_num)
      query <- sprintf("Select model_var from ResultNames where mode = 'MC' AND model = '%s'",
                       model)
      mc_vars<- mainDbSelect(query)$model_var
      mc_results <- lapply(mc_vars,function(x,n){
        return(x = rep(NA,n))
      },mc_num)
      names(mc_results)<- mc_vars
      
      currentDose <<- Exposure$value[which(Exposure$param == whichDose & Exposure$expoid == simid)]#input$mySlider2[1]
      # if(currentDose == 0){
      #   currentDose = 0.05
      # }
      # maxDose <- input$mySlider2[2]
      # increaseDose <- (input$mySlider2[2]/currentDose)^(1/(nDoses-1))
      # print(increaseDose)
      # for(n in 1:(nDoses)){
      # print(paste('Running monte carlo simulation ', n ))
      # print(currentDose)
      # model_params$vals[[whichDose]] <- currentDose
      
      
      for (i in 1:mc_num){
        model_params$vals[colnames(MC.matrix)]<- MC.matrix[i,]
        initial_values <<- calculateInitialValues(model_params)
        tempDF <- runFDPBPK(initial_values,model)
        max_list <- unlist(lapply(mc_vars,function(x,data){
          var_name <- gsub("_max","",x)
          
          return(max(data[var_name]))
        },tempDF$pbpk))
        names(max_list)<- mc_vars
        for (x in mc_vars){
          mc_results[[x]][[i]]<- max_list[[x]]
        }
        updateProgressBar(session,"bmProgress",value = i, total = mc_num)
      }
      
      
      results$pbpk <- as.data.frame(mc_results)
      # MybmResults <<- as.data.frame(mc_results)
      plasmaResults <- as.data.frame(results$pbpk$cpls_max)
      colnames(plasmaResults) = currentDose
      # pr2 <<- plasmaResults
      # plasmaResults <<- plasmaResults %>%
      #   rename(
      #     results$pbpk$cpls_max = 'DOSE'
      #   )
      
      # if(n == 1){
      #   bmResults2 <<- NULL
      # }
      # bmResults2[n] <<- plasmaResults[1]
      # if(n==1){#is.null(bmResults)){
      #   # print('it is null')
      bmResults <<- plasmaResults
      # } else{
      #   # print('something exists')
      #   bmResults <<- cbind(bmResults,plasmaResults)# %>%
      # }
      # bmResults <<- bmResults %>% 
      #   rename(
      #     results$pbpk$cpls_max = 'DOSE'
      #   )
      #   mutate(
      #     xCol = as.data.frame(mc_results['cpls_max'])
      #   )
      results$mode <- "MC"
      # currentDose = currentDose * increaseDose
      # }
      #     updateNavbarPage(session,"menu","output")
    }else{
      initial_values <- calculateInitialValues(model_params)
      
      updateProgressBar(session,"bmProgress",value = 100, total = 100,
                        status = "info")
      tempDF <- runFDPBPK(initial_values,model)
      
      results$pbpk<- tempDF$pbpk
      
      
      results$mode <- "FD"
      #     updateNavbarPage(session,"menu","output")
    }
    ## })
    
    
    updatemenus <- list(
      list(
        active = 0,
        type = 'buttons',
        buttons = list(
          list(
            label = 'Default',
            method = 'relayout',
            args = list(
              list(
                yaxis = list(
                  title = 'Count',
                  type = 'linear'
                )
              )
            )
          ),
          list(
            label = 'Log Y-Axis',
            method = 'relayout',
            args = list(
              list(
                yaxis = list(
                  title = 'Count',
                  type = 'log'
                )
              )
            )
          )
        )
      )
    )
    
    output$Plot3 <- renderPlotly({
      p <- plot_ly(
        x = bmResults[,1],
        type = "histogram"
      ) %>%
        layout(
          title = doseName,
          yaxis = list(
            title = 'Count'
          ),
          xaxis = list(
            title = paste(doseName, ' Concentration (', doseUnits,')', sep = '')
          ),
          margin = m,
          updatemenus = updatemenus
        )
    })
    
    removeModal()
  })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ################################################################################ Old stuff below
    
  #   bmvals$name <- input$bmname
  #   filesIn2 <- input$bmFile
  #   df_list2 <- lapply(filesIn2$datapath,read_csv) # read each file into a data frame
  #   bmvals$csvFile <- bind_rows(df_list2) # concatenates all of the data frames
  #   bmCSV <- data.frame(bmvals$csvFile)
  #   updatemenus <- list(
  #     list(
  #       active = 0,
  #       type = 'buttons',
  #       buttons = list(
  #         list(
  #           label = 'Default',
  #           method = 'relayout',
  #           args = list(
  #             list(
  #               yaxis = list(
  #                 title = 'Count',
  #                 type = 'linear'
  #               )
  #             )
  #           )
  #         ),
  #         list(
  #           label = 'Log Y-Axis',
  #           method = 'relayout',
  #           args = list(
  #             list(
  #               yaxis = list(
  #                 title = 'Count',
  #                 type = 'log'
  #               )
  #             )
  #           )
  #         )
  #       )
  #     )
  #   )
  #   
  #   output$Plot3 <- renderPlotly({
  #     p <- plot_ly(
  #       x = bmCSV[,1],
  #       type = "histogram"
  #     ) %>%
  #       layout(
  #         title = bmvals$name,
  #         yaxis = list(
  #           title = 'Count'
  #         ),
  #         xaxis = list(
  #           title = paste(input$bmtype, ' Concentrations (mg/L)', sep = '')
  #         ),
  #         margin = m,
  #         updatemenus = updatemenus
  #       )
  #   })
  #   
  #   removeModal()
  # })
  
  observeEvent(input$type,{
    if(input$type == "Inhalation"){
      choices = c("ppm","ppb")
    }else if(input$type == "Oral"){
      choices = c("mg/kg/day", "mg/L")
    }else if(input$type == "IV"){
      choices = c("mg/h")
    }else if(input$type == "Dermal"){
      choices = c("mg/cm2")
    }else{
      choices = c("unknown exposure type")
    }
    output$unit_ui <- renderUI({
      shinyWidgets::radioGroupButtons(
        "unit",
        label = "Select Exposure Units",
        choices = choices
      )
    })
  })
  
  observeEvent(input$modalNav, {
    if(input$modalNav == 'Upload Existing Results'){
      if(is.null(input$csvFile)){
        output$mcFooter <- renderUI({
          shinyjs::disabled(actionButton("add","Add Dataset"))
        })
      } else{
        output$mcFooter <- renderUI({
          actionButton("add","Add Dataset")
        })
      }
    } else{ # input$modalNav != 'Upload Existing Results'
      if(is.null(input$rDataFile)){
        output$mcFooter <- renderUI({
          shinyjs::disabled(actionButton("runExposure","Run Simulation"))
        })
      } else{
        output$mcFooter <- renderUI({
          actionButton("runExposure","Run Simulation")
        })
      }
    }
  })
  
  output$toggleSidebar <- reactive({
    input$showpanel
  })
  
  outputOptions(output, "toggleSidebar", suspendWhenHidden = F)
  
  observeEvent(input$btnRTRExtrapolation,{
    ppbFiles <- list.files(pattern = 'PercentilePPB.*csv') # list of all of the files to import
    df_list <- lapply(ppbFiles,read_csv) # read each file into a data frame
    percentileDF <- as.data.frame(bind_rows(df_list)) # concatenates all of the data frames
    
    plotFiles <- list.files(pattern = 'PDFandCDF.*csv') # list of all of the files to import
    plotdf_list <- lapply(plotFiles,read_csv) # read each file into a data frame
    pdfAndCDF <- as.data.frame(bind_rows(plotdf_list)) # concatenates all of the data frames
    
    updatemenus <- list(
      list(
        active = 0,
        type = 'buttons',
        buttons = list(
          list(
            label = 'Log X-Axis',
            method = 'relayout',
            args = list(
              list(
                xaxis = list(
                  title = colnames(pdfAndCDF)[1],
                  type = 'log'
                )
              )
            )
          ),
          list(
            label = 'Linear X-Axis',
            method = 'relayout',
            args = list(
              list(
                xaxis = list(
                  title = colnames(pdfAndCDF)[1],
                  type = 'linear'
                )
              )
            )
          )
        )
      )
    )
    
    output$percentilePPB <- DT::renderDataTable({
      DT::datatable(
        data = percentileDF,
        extensions = 'Buttons',
        class = 'cell-border stripe',
        rownames = F,
        options = list(
          ordering = F,
          autoWidth = T,
          dom='rtB',
          buttons = list(
            list(
              extend = 'csv',
              text = 'Download CSV',
              filename = "Percentile.csv"
            )
          ),
          columnDefs = list(
            list(
              className = 'dt-center',
              targets = c(0,1)
            )
          )
        )
      )
    })
    
    output$PDF <- renderPlotly({
      p <- plot_ly(
        pdfAndCDF,
        x = pdfAndCDF[[1]],
        y = pdfAndCDF[[2]],
        name = 'PDF',
        type = 'scatter',
        mode = 'lines+markers'
      ) %>%
        layout(
          title = 'PDF',
          xaxis = list(
            title = colnames(pdfAndCDF)[1],
            type = 'log'
          ),
          yaxis = list(
            title = 'Probability'
          ),
          margin = m,
          updatemenus = updatemenus
        )
    })
    
    output$CDF <- renderPlotly({
      p <- plot_ly(
        pdfAndCDF,
        x = pdfAndCDF[[1]],
        y = pdfAndCDF[[3]],
        name = 'CDF',
        type = 'scatter',
        mode = 'lines+markers'
      ) %>%
        layout(
          title = 'CDF',
          xaxis = list(
            title = colnames(pdfAndCDF)[1],
            type = 'log'
          ),
          yaxis = list(
            title = 'Cumulative'
          ),
          margin = m,
          updatemenus = updatemenus
        )
    })
    
    revDosDataHeaders <- read.csv(
      'ReverseDosimetryDataHeaders.csv',
      header=FALSE
    )
    revDosData1 <- read.csv(
      'ReverseDosimetryData1.csv',
      header=FALSE
    )
    revDosData2 <- read.csv(
      'ReverseDosimetryData2.csv',
      header=FALSE
    )
    revDosData3 <- read.csv(
      'ReverseDosimetryData3.csv',
      header=FALSE
    )
    row.names(revDosData1) <- pdfAndCDF[[1]]
    row.names(revDosData2) <- list('Adjusted Weighting Factors')
    row.names(revDosData3) <- pdfAndCDF[[1]]
    

    sketch = htmltools::withTags(
      table(
        class = 'display',
        thead(
          style='text-align: right;',
          tr(
            th(colspan = 1, 'Larger than', style='text-align: left; background-color: #f5f5f5'),
            lapply(rep(revDosDataHeaders[[1]], 1), th)
          ),
          tr(
            th(colspan = 1, 'Smaller than or equal to', style='text-align: left; background-color: #f5f5f5'),
            lapply(rep(revDosDataHeaders[[2]], 1), th)
          )
        )
      )
    )
    
    revDosDataTable <- function(userData){
      DT::datatable(
        data = userData,
        container = sketch,
        rownames = T,
        extensions = c('FixedColumns','FixedHeader'),
        options = list(
          ordering = F,
          autoWidth = T,
          fixedColumns = T,
          scrollX = '100%',
          scrollY = 600,
          scrollCollapse = T, # When scrollY is defined, bottom of table won't "float" below the table
          pageLength = 50, # How many rows to display by default
          # ,dom='rt',
          columnDefs = list(
            list(
              width = '160px',
              targets = c(0)
            )
          )
        )
      )
    }
    
    output$revDosData1 <- DT::renderDataTable({
      revDosDataTable(revDosData1)
    })
    
    output$revDosData2 <- DT::renderDataTable({
      revDosDataTable(revDosData2)
    })
    
    output$revDosData3 <- DT::renderDataTable({
      revDosDataTable(revDosData3)
    })
    
    # Creates the dataframe for the user to download
    td1 <- data.frame(t(revDosDataHeaders))
    colnames(td1) = colnames(revDosData1)
    dlTable1 <- bind_rows(td1, revDosData1)
    rownames(dlTable1) = c('Larger than','Smaller than or equal to',pdfAndCDF[[1]])
    
    output$downloadTablerevDosData1 <- downloadHandler(
      filename = "Probability_Table.csv",
      function(file){
        write.table(
          dlTable1,
          file = file,
          sep = ",",
          row.names = T,
          col.names = F
        )
      }
    )
    
    # Creates the dataframe for the user to download
    dlTable2 <- bind_rows(td1, revDosData2)
    rownames(dlTable2) = c('Larger than','Smaller than or equal to','Adjusted Weighting Factors')
    
    output$downloadTablerevDosData2 <- downloadHandler(
      filename = "Adjusted_Weighting_Factors.csv",
      function(file){
        write.table(
          dlTable2,
          file = file,
          sep = ",",
          row.names = T,
          col.names = F
        )
      }
    )
    
    # Creates the dataframe for the user to download
    dlTable3 <- bind_rows(td1, revDosData3)
    rownames(dlTable3) = c('Larger than','Smaller than or equal to',pdfAndCDF[[1]])
    
    output$downloadTablerevDosData3 <- downloadHandler(
      filename = "Weight_ECF.csv",
      function(file){
        write.table(
          dlTable3,
          file = file,
          sep = ",",
          row.names = T,
          col.names = F
        )
      }
    )
    
  })
  
  observeEvent(input$navbar,{
    if (input$navbar == "Quit"){
      stopApp()
    }
  })
  
})

calculateInitialValues <- function(params_list){
  params <- params_list$vals
  brep_flag <- as.logical(params[["brep_flag"]])
  brepv_flag <- as.logical(params[["brepv_flag"]])
  iv_flag <- as.logical(params[["ivrep_flag"]])
  derm_flag <- as.logical(params[["dermrep_flag"]])
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
    print(dermlen)
    print(event_days)
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
  
  state <- c(
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
    anabsgut = 0.0)
  
  initial_values <- list("evnt_data"= eventDat,
                         "initial_params"= initial_params[params_list$names],
                         "times"=times,
                         "tstop"=tstop,"tstart"=tstart,
                         "state"= state)
  
  return(initial_values)
}


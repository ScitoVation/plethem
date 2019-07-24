#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

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
  
  monteCarloModal <- function() {
    modalDialog(
      useShinyjs(),
      # title = "Upload Monte Carlo Results",
      easyClose = F,#TRUE,
      size = "l",
      tagList(
        useSweetAlert(),
        tabsetPanel(
          id = 'modalNav',
          tabPanel(
            title = 'Upload Existing Results',
            br(),
            textInput(
              "mcname",
              "Dataset Name",
              placeholder = "Enter name for the dataset"
            ),
            fluidRow(
              column(
                6,
                shinyWidgets::radioGroupButtons(
                  "type",
                  "Select Exposure Type",
                  choices = c("Inhalation", "Oral", "IV", "Dermal")
                )
              ),
              column(
                6,
                uiOutput(
                  "unit_ui"
                )
              )
            ),
            fileInput(
              "csvFile",
              label = "Select CSV file",
              accept = c("text/csv","text/comma-separated-values",".csv"),
              multiple = TRUE
            ),
            tags$h4("Results in mg/L")
          ),
          tabPanel(
            title = 'Create Monte Carlo Results',
            br(),
            fileInput(
              "rDataFile",
              label = "Select Project file",
              accept = c(".RData", ".Rdata"),
              placeholder = 'Upload .Rdata file',
              multiple = F
            ),
            pickerInput(
              'simulation',
              'Select Simulation',
              choices = NULL,
              selected = NULL,
              multiple = F
              # ,options = list(
                # 'live-search' = TRUE,
                # 'actions-box' = TRUE,
                # 'selected-text-format' = 'count > 2',
                # 'count-selected-text'='{0} simulations selected'
              # )
            ),
            fluidRow(
              column(
                6,
                shinyWidgets::radioGroupButtons(
                  'tissue',
                  label = "Select Tissue Type",
                  choices = c('Plasma', 'Urine')
                )
              ),
              column(
                6,
                shinyWidgets::radioGroupButtons(
                  "chemType",
                  "Select Chemical Type",
                  choices = c("Parent", "Metabolite")
                )
              )
            ),
            fluidRow(
              column(
                6,
                sliderInput(
                  'mySlider2',
                  label = 'Exposure Type (Units)',
                  min = 0,
                  max = 1000,
                  value = c(0,1000)
                )
              ),
              column(
                6,
                numericInput(
                  'mcNumeric',
                  label = 'Number of Doses',
                  min = 20,
                  max = 50,
                  value = 25,
                  step = 1,
                  width = '156.84px'
                ),
                uiOutput(
                  'validNum'
                )
              )
            )
          )
        )
      ),
      footer= tagList(
        uiOutput('mcFooter', inline = T),
        modalButton('Cancel')
      )
    )
  }
  
  biomonitoringModal <- function() {
    modalDialog(
      useShinyjs(),
      title = "Upload Biomonitoring Results",
      easyClose = TRUE,
      size = "l",
      tagList(
        textInput(
          "bmname",
          "Dataset Name",
          placeholder = "Enter name for the dataset"
        ),
        shinyWidgets::radioGroupButtons(
          "bmtype",
          "Select Type",
          choices = c("Parent", "Metabolite")
        ),
        fileInput(
          "bmFile",
          label = "Upload Biomonitoring Results",
          accept = c("text/csv","text/comma-separated-values",".csv"),
          multiple = TRUE
        ),
        tags$h4(tags$span(style='color:red', 'IMPORTANT:'), ' Biomonitoring results must be in mg/L', sep = '')
      ),
      footer= tagList(
        shinyjs::disabled(actionButton("addBM","Add Dataset")),
        modalButton("Cancel")
      )
    )
  }
  
  mcNum <- reactive({
    validate(
      need(input$mcNumeric > 19, 'Invalid input. Please enter a number 20-50.') %then%
        need(input$mcNumeric < 51, 'Invalid input. Please enter a number 20-50.')
    )
  })
  
  output$validNum <- renderUI({
    mcNum()
  })

  observeEvent(input$csvFile, {
    shinyjs::enable("add")
  })
  
  observeEvent(input$rDataFile, {
    shinyjs::enable('addMC')
    inFile <- input$rDataFile
    rDFile <- inFile$datapath
    # e = new.env()
    # name <<- load(rDFile, envir = e)
    # data <- e[['name']]
    load(rDFile, envir = .GlobalEnv)
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
  jsCode2 <- 'shinyjs.swal = function(){swal({
  title: "Are you sure?",
  text: "Once deleted, you will not be able to recover this imaginary file!",
  icon: "warning",
  buttons: true,
  dangerMode: true,
})
  .then((willDelete) => {
  if (willDelete) {
  swal("Poof! Your imaginary file has been deleted!", {
  icon: "success",
  });
  } else {
  swal("Your imaginary file is safe!");
  }
  });}'
  observeEvent(input$addMC, {
    runjs('/*swal({
  title: "Are you sure?",
          text: "Once deleted, you will not be able to recover this imaginary file!",
          icon: "warning",
          buttons: true,
          dangerMode: true,
  })
          .then((willDelete) => {
          if (willDelete) {
          swal("Poof! Your imaginary file has been deleted!", {
          icon: "success",
          });
          } else {
          swal("Your imaginary file is safe!");
          }
          });}*/
          /*var today = new Date(); alert(today);*/
          swal({title: "Hello World!"})
          .then(console.log("Hello"));')
    # js$swal()
    # sendSweetAlert(
    #   session = session,
    #   title = "Are you sure you want to run a simulation?",
    #   text = "This may take several hours to complete.",
    #   type = 'warning',
    #   btn_labels = c('Cancel','Confirm'),
    #   closeOnClickOutside = F
    # )
  })
  
  observeEvent(input$simulation, {
    simSet3 <<- simSet %>%
      filter(
        name == input$simulation
      )
    exposureType <<- Exposure %>%
      filter(
        expoid == simSet3$expoid & 
          param == 'expo_sidebar'
      )
    myExpoid <- exposureType$value[1]
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
    
    updateSliderInput(
      session,
      'mySlider2',
      label = mySliderLabel,
      max = 1000 # This line is needed to update the label because this would create subscript out of bounds error otherwise
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
    shinyjs::enable("addBM")
  })
  
  observeEvent(input$btnUploadMC,{
    showModal(monteCarloModal())
  })
  
  observeEvent(input$btnUploadBMResults, {
    showModal(biomonitoringModal())
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
  observeEvent(input$addBM, {
    bmvals$name <- input$bmname
    filesIn2 <- input$bmFile
    df_list2 <- lapply(filesIn2$datapath,read_csv) # read each file into a data frame
    bmvals$csvFile <- bind_rows(df_list2) # concatenates all of the data frames
    bmCSV <- data.frame(bmvals$csvFile)
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
        x = bmCSV[,1],
        type = "histogram"
      ) %>%
        layout(
          title = bmvals$name,
          yaxis = list(
            title = 'Count'
          ),
          xaxis = list(
            title = paste(input$bmtype, ' Concentrations (mg/L)', sep = '')
          ),
          margin = m,
          updatemenus = updatemenus
        )
    })
    
    removeModal()
  })
  
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
      output$mcFooter <- renderUI({
        shinyjs::disabled(actionButton("add","Add Dataset"))
      })
    } else{
      output$mcFooter <- renderUI({
        shinyjs::disabled(actionButton("addMC","Run Dataset"))
      })
    }
  })
  
  output$toggleSidebar <- reactive({
    input$showpanel
  })
  
  outputOptions(output, "toggleSidebar", suspendWhenHidden = F)
  
  observeEvent(input$btnRunRevDos,{
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

#' Function that runs the httk oral equivalent dose gadaget. 
#' 
#' @export
httkCalcOralEqDose <- function(){
  
  ui <- miniPage(
    gadgetTitleBar(""),
    miniContentPanel(
      fillCol(flex = 1,
        numericInput("ivc","Enter Invitro Concentration (uM)",
                     value = 0,width = validateCssUnit("90%")),
        pickerInput("chem_list","Select Chemical",
                    choices = NULL,
                    options = list(size = 2,
                                   'live-search'= T),
                    width = validateCssUnit("90%")),
        numericInput("quantile","Quantile",0.5,min = 0,max =1,
                     validateCssUnit("90%")),
        tags$h5("Oral Equivalent Dose"),
        textOutput("oraleq")
        )
    )
  )
  
  server <- function(input,output,session){
    vals <- setNames(httk::chem.physical_and_invitro.data$CAS,
                     httk::chem.physical_and_invitro.data$Compound)
    updatePickerInput(session,"chem_list",choices = vals)
    
    observe({
      ivc <- input$ivc
      chem_cas <- input$chem_list
      quantile <- input$quantile
      if(!(is.null(chem_cas))){
        oral_eq <- httk::calc_mc_oral_equiv(ivc,chem.cas = chem_cas,
                                            which.quantile = quantile)
      }else{
        oral_eq <- 0
      }
      
      output$oraleq <- renderText({paste(signif(oral_eq,4),"mg/kg BW/day",sep = " ")})
    })
    observeEvent(input$done,{
      ivc <- input$ivc
      chem_cas <- input$chem_list
      quantile <- input$quantile
      oral_eq <- httk::calc_mc_oral_equiv(ivc,chem.cas = chem_cas,
                               which.quantile = quantile,suppress.messages = T)
      stopApp(returnValue = oral_eq)
    })
    
    
  }
  runGadget(ui,server,viewer =dialogViewer("HTTK Oral Equivalent Dose",
                                           width = 350,height = 400))
}
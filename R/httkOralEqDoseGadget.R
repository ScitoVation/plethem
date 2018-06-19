#' @export
httkCalcOralEqDose <- function(){
  
  ui <- miniPage(
    gadgetTitleBar(""),
    miniContentPanel(
      fillCol(flex = c(1,5,1),
        numericInput("ivc","Enter Invitro Concentration (uM)",
                     value = 0,width = validateCssUnit("90%")),
        pickerInput("chem_list","Select Chemical",
                    choices = NULL,
                    width = validateCssUnit("90%")),
        numericInput("quantile","Quantile",0.5,min = 0,max =1)
        )
    )
  )
  
  server <- function(input,output,session){
    vals <- setNames(chem.physical_and_invitro.data$CAS,
                     chem.physical_and_invitro.data$Compound)
    updatePickerInput(session,"chem_list",choices = vals)
    observeEvent(input$done,{
      ivc <- input$ivc
      chem_cas <- input$chem_list
      quantile <- input$quantile
      oral_eq <- httk::calc_mc_oral_equiv(ivc,chem.cas = chem_cas,
                               which.quantile = quantile)
      stopApp(returnValue = oral_eq)
    })
    
    
  }
  runGadget(ui,server,viewer =dialogViewer("HTTK Oral Equivalent Dose",
                                           width = 250,height = 450))
}
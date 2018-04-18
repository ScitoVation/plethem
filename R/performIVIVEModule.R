# Module for performing single age IVIVE in the chemical tab

#' Shiny module that is called when perform IVIVE button is clicked on the chemical tab of a PBPK model
#' @export
performIVIVEUI<- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(title = "Perform IVIVE",
                        fluidPage(
                          fluidRow(
                            column(4,
                                   selectizeInput(ns("sel_org"),"Select Organism",
                                                  choices = c("Human"="ha","Rat"="ra"))
                                   ),
                            column(4,
                                   numericInput(ns("num_bw"),"Body Weight(kg)",value = 81)
                                   ),
                            column(4,
                                   numericInput(ns("num_livwt"),"Liver Weight(kg)",value = 1.8)
                                   )
                          ),
                          fluidRow(
                            column(3,
                                   numericInput(ns("num_km"),"Michelis-Menten Constant",value = 1)
                                   ),
                            column(3,
                                   numericInput(ns("num_hpgl"),"10^6 Hepatocytes per gram liver", value = 0)
                                   ),
                            column(3,
                                   numericInput(ns("num_mppgl"),"Microsomal protein per gram liver", value = 0)
                                   ),
                            column(3,
                                   numericInput(ns("num_cppgl"),"Cytosolic protein per gram liver",value = 0)
                                   )
                          ),
                          
                          fluidRow(
                                   tabsetPanel(id = ns("heptype"),
                                          tabPanel(title = "Whole Hepatocyte",value = "hep_whole",
                                                   fluidRow(
                                                     column(4,
                                                            numericInput(ns("num_whcl"),"Hepatocyte Clearance",
                                                                         value= 0)
                                                            ),
                                                     column(4,
                                                            selectizeInput(ns("sel_whunit"),label = "Units",
                                                                           choices = c("L/h"="Lh",
                                                                                       "L/h/10^6 Hepatocytes"="LhH",
                                                                                       "\u03BCmol/min/10^6 hepatocytes"="ummH")
                                                                           )
                                                            )
                                                   )                                   
                                                   ),
                                          tabPanel(title = "Sub-cellular",value = "hep_sc",
                                               
                                                     fluidRow(
                                                       column(4,
                                                              numericInput(ns("num_mscl"),
                                                                           label = "Measure Microsomal Clearance",
                                                                           value = 0)
                                                              ),
                                                       column(4,
                                                              selectizeInput(ns("sel_msunit"),label = "Units",
                                                                          choices = list("\u03BCmol/min/mg Protein"="ummmP",
                                                                                         "\u03BCL/min/mg Protein"="ulmmP",
                                                                                         "\u03BCL/h/mg Protein"="ulhmP",
                                                                                         "mL/min/mg Protein"="mlmmP",
                                                                                         "mL/h/mg Protein"="mlhmP"))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(4,
                                                              numericInput(ns("num_cycl"),
                                                                           label = "Measure Cytosolic Clearance",0)

                                                       ),
                                                       column(4,
                                                              selectizeInput(ns("sel_cyunit"),
                                                                          label = "Units",
                                                                          choices = list("\u03BCL/min/mg Protein"="ulmmP",
                                                                                         "\u03BCL/h/mg Protein"="ulhmP",
                                                                                         "mL/min/mg Protein"="mlmmP",
                                                                                         "mL/h/mg Protein"="mlhmP"))
                                                       )
                                                     )
                                                   
                                          ),
                                          tabPanel(title = "S9 Fraction",value = "hep_s9",
                                                   fluidRow(
                                                     column(4,
                                                            numericInput(ns("num_S9cl"),
                                                                         "Measured S9 Fraction Clearance",0)
                                                     ),
                                                     column(4,
                                                            selectizeInput(ns("sel_s9unit"),label = "Units",
                                                                        choices = list("\u03BCL/min/mg Protein"="ulmmP",
                                                                                       "\u03BCL/h/mg Protein"="ulhmP",
                                                                                       "mL/min/mg Protein"="mlmmP",
                                                                                       "mL/h/mg Protein"="mlhmP"))
                                                            )
                                                     )
                                          ),
                                          shinyBS::bsButton(ns("btn_reset_metab"),"Reset All Clearance Values"),
                                          type = "tab"
                                          )
                          )
                        ),size = "l",
                        footer = tagList(
                          modalButton("Cancel"),
                          shinyBS::bsButton(ns("btn_ivive"),"Perform IVIVE",style = "primary")
                        )
                        )
            )
  
  
}

#'server side function for performing IVIVE for a chemical
#'@export
performIVIVE <- function(input,output,session,km){
  returnValues <- reactiveValues()
  returnValues$ret_data <- reactiveVal(c("No",0,0,1))
  ns <- session$ns
  if(km >0){
    updateNumericInput(session,"num_km",value = km)
  }
  observeEvent(input$sel_org,{
    if (input$sel_org == "ha"){
      MPCPPGL <- calcMPCPPGL(25)
      mppgl <- signif(MPCPPGL$MPPGL,4)
      cppgl <- signif(MPCPPGL$CPPGL,4)
      hpgl <- 99
      liver_wt <- signif(getLifecourseLiverVolume(25,"M"),4)
      bw <- signif(getLifecourseBodyWeight(25,"M"),4)
    }else{
      mppgl <- 45
      cppgl <- 91
      hpgl <- 110
      liver_wt <- 0.012
      bw <- 0.3
    }
    updateNumericInput(session,"num_mppgl",value = mppgl)
    updateNumericInput(session,"num_cppgl",value = cppgl)
    updateNumericInput(session,"num_hpgl",value = hpgl)
    updateNumericInput(session,"num_bw",value = bw)
    updateNumericInput(session,"num_livwt",value = liver_wt)
  })
  module_calcs <- function(){
    hepcl_type <- input$heptype
    liver_wt <- input$num_livwt
    hpgl <- input$num_hpgl
    km <- input$num_km
    mpcppgl <- c(input$num_mppgl,input$num_cppgl)
    org <- input$sel_org
    age <- ifelse(org == "ha",25,52)
    bw <- input$num_bw
    vliv <- switch(hepcl_type,
                   "hep_sc"=calculateScaledSCClearance(c(input$num_mscl,input$num_cycl),
                                                       c(input$sel_msunit,input$sel_cyunit),
                                                       org,age,liver_wt,
                                                       km,mpcppgl,
                                                       return_total = T),
                   "hep_s9"=calculateScaledS9Clearance(input$num_s9cl,input$sel_s9unit,
                                                       org,age,liver_wt,
                                                       km,mpcppgl),
                   "hep_whole"=calculateScaledWholeHepClearance(input$num_whcl,
                                                                input$sel_whunit,
                                                                liver_wt,hpgl,km),
                   0
    )
    vmax <- vliv*km*liver_wt/(bw^0.75)
    #print(c(vliv,vmax,km))
    return(c("Yes",vliv,vmax,km))
  }
  #returnValues$ret_data<- 
  returnValues$ret_data<- eventReactive(input$btn_ivive,module_calcs(),ignoreInit = TRUE,ignoreNULL = TRUE)
  
  
  observeEvent(input$btn_ivive,{
    removeModal()
  })
  return(returnValues$ret_data)
}

#' UI for modufying the user chemical database
#' @description The UI function for calling the module that allows the user to modify the local chemical database.
#'   This is a module function and the user will never need to call it
#' @param namespace the namespace for this module
#' @export
chemicalInput <- function(namespace) {
  # Create a namespace function using the provided id
  ns <- NS(namespace)

  tagList(
    conditionalPanel(condition = paste0("input['", ns("modalAction"), "'] == 'new' "),
                     tags$h3("New Chemical", class="text-center pager-header")
    ),
    conditionalPanel(condition = paste0("input['", ns("modalAction"), "'] == 'update' "),
                     tags$h3("Update Chemical", class="text-center pager-header")
    ),
    conditionalPanel(condition = paste0("input['", ns("modalAction"), "'] == 'delete' "),
                     tags$h3("Delete Chemical", class="text-center pager-header")
    ),
    fluidRow(
      column(12,
             radioButtons(ns("modalAction"), "Choose Action", choices = list("New" = 'new', "Update" = 'update', "delete"='delete'),
                          selected = "new", inline = TRUE )
      )
    ),

    conditionalPanel(condition =paste0("input['", ns("modalAction"), "'] != 'new' "),
                     fluidRow(
                       column(12,
                              selectizeInput(ns("choseChem"), "Select a chemical", choices= NULL)
                              #uiOutput(ns("chemSelector"))
                       )
                     )),
    fluidRow(
      column(6,
             textInput(ns("Mname"), label = "New Chemical Name", value = "")),
      column(6,
             numericInput(ns("Mfup"), label = "Fraction Unboud in Palsma", value = 0, 0, 1, 0.001))
    ),
    fluidRow(
      column(6,
             numericInput(ns("Mden"),"Density (g/L)",0,0,1500,1)),
      column(6,
             numericInput(ns("Mmw"),"Molecular Weight",0,0,250,0.01))
    ),
    fluidRow(
      column(6,
             numericInput(ns("Mvpa"),"Vapor Pressure (Pa)",0,0,250,0.01)),
      column(6,
             numericInput(ns("Mdkow"),"logKow in skin at pH5.5",0,0,250,0.01))
    ),
    fluidRow(
      column(6,
             numericInput(ns("Mlkow"),"logKow in Octanol:Water Coefficient",0,0,250,0.01)),
      column(6,
             numericInput(ns("Mwsol"),"Water Solubility (mg/L)",0,0,250,0.01))
    ),
    fluidRow(
      column(6,
             numericInput(ns("Mres"),"Estimated Fraction Resorpbed in Kidney",0,0,1,0.01)),
      column(6,
             numericInput(ns("Mfhprt"),"Fraction of Enterohepatic Circulation",0,0,1,0.01))
    ),
    fluidRow(
      column(6,
             numericInput(ns("Mvmaxc"), paste("Maximum Metabolism Rate (","μM/h/kg)"),0,0,250,0.01)),
      column(6,
             numericInput(ns("Mkm"),"Michelis Menton Constant for Metabolism",0,0,250,0.01))
    ),
    fluidRow(
      column(6,
             textInput(ns("Mcas"), label = "Chemical CAS Registration Number", value = ""))
    ),
    ########end inputField

    actionButton(ns("submitNewChem"), "Submit")

  )

}

#' Server side function for the UI used to edit local chemical database
#' @description The sever function for the chemical module used in PLETHEM. This function interacts with the server function
#'   of the PLETHEM model.Is modifies the local chemical database by adding, deleting or modifying existing chemical data.
#' @param input the input object from the chemical module UI
#' @param output the output object from the chemical module UI
#' @param session the shiny session information where the chemical module is currently called
#' @export
#'
chemical <- function(input, output, sessions){
  source("../../R/QueryHelper.R")

  feedback <- observeEvent({input$submitNewChem},{
    feedback <- "worked"


    chemField <- list("Mname"= input$Mname, "Mfup"=input$Mfup, "Mden"=input$Mden, "Mmw"= input$Mmw,"Mvpa"= input$Mvpa, "Mdkow"= input$Mdkow,  "Mlkow"=input$Mlkow, "Mwsol"= input$Mwsol, "Mres"=  input$Mres, "Mfhprt"= input$Mfhprt, "Mvmaxc" =input$Mvmaxc, "Mkm"=input$Mkm, "Mcas" = input$Mcas)

    if(input$modalAction == 'new'){
      created <- createNewChemical(session, chemField$Mname, chemField$Mcas, chemField$Mfup, chemField$Mden, chemField$Mmw, chemField$Mvpa, chemField$Mdkow, chemField$Mlkow, chemField$Mwsol, chemField$Mres, chemField$Mfhprt, chemField$Mvmaxc, chemField$Mkm)
      showNotification(paste0(sprintf("%s", created)), duration = 5)
    }else if(input$modalAction == 'update'){
      updated <- updateChemical(session, input$choseChem, chemField$Mname, chemField$Mcas, chemField$Mfup, chemField$Mden, chemField$Mmw, chemField$Mvpa, chemField$Mdkow, chemField$Mlkow, chemField$Mwsol, chemField$Mres, chemField$Mfhprt, chemField$Mvmaxc, chemField$Mkm)
      showNotification(paste0(sprintf("%s", updated)), duration = 5)
    }else if(input$modalAction == 'delete'){
      deleted <- deleteChemical(session, input$choseChem)
      showNotification(paste0(sprintf("%s", deleted)), duration = 5)
    }
  })


  observeEvent({
    input$modalAction
  },{chems <- getAllChemicals(session, "modal")
  updateSelectizeInput(session, "choseChem", "Select a chemical", choices= chems, selected = chems[1])
  })

  observeEvent({
    input$submitNewChem
  },{chems <- getAllChemicals(session, "modal")
  updateSelectizeInput(session, "choseChem", "Select a chemical", choices= chems, selected = chems[1])
  })

  observeEvent({
    input$chooseChem
  },{
    getAllChemicals(session, "modal")
  })

  observeEvent({
    input$submitNewChem
  },{getAllChemicals(session, "modal")
  })

  #################Updating Chemical parameter values in Modal
  observeEvent({
    input$modalAction
  },{
    #if(input$modalAction != 'new'){
    updateChemValues(session, input$choseChem, input$modalAction)
    #}
  })

  observeEvent({
    input$choseChem
  },{
    if(input$modalAction != 'new'){
      updateChemValues(session, input$choseChem, input$modalAction)
    }

  })
}



#' Module for editing high throughput reverse dosimetry functions
#' @description The UI for defining HT-IVIVE parameters in the HT-IVIVE project. 
#' It is called by the HT-IVIVE server script when a new row is added or existing row is edited. It is never called directly by the user.
#' @param namespace namespace for the module. This is unique and decided by the project server function
#' @param set_list A list of inputs for the dropdown menus.
#' @export
HT_IVIVEUI <- function(namespace="",set_list = NULL){
  css <- "div .modal-lg {
  width:1000px

  }"

  ns <- NS(namespace)
  showModal(modalDialog(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(css),
      tabsetPanel(id = "setupTabs",
                  tabPanel("Physiological and Chemical Parameters",
                           fluidPage(
                             fluidRow(
                               column(8, offset = 2,
                                      textInput(ns("txt_IVIVE_name"),"Name",
                                                width = validateCssUnit("100%"),
                                                placeholder = "Identifier for HT-IVIVE"))
                               ),
                             
                             fluidRow(
                               column(4,
                                      uiOutput(ns("org_output"))
                                      #$h4("Standard Human")
                                      
                                      ),
                               column(4,
                                      uiOutput(ns("chem_output"))
                                      ),
                               column(4,
                                      numericInput(ns("num_expo"),
                                                   label = "Environmental Exposure (mg/kg/day)",
                                                   value = 0,width = validateCssUnit("100%"))
                                      )
                             ),
                             fluidRow(
                               column(4,
                                      numericInputIcon(ns("num_fupls"),
                                                       label = "Fraction Unbound in Plasma",
                                                       value = 1,min = 0 , max = 1, 
                                                       icon = list("fraction"),
                                                       width = validateCssUnit("100%"))
                               ),
                               column(4,
                                      numericInputIcon(ns("num_km"),
                                                   label = "Michaelis Menten Constant",
                                                   value = 1, min = 0.1,icon = list("\U00B5M"),
                                                   width = validateCssUnit("100%"))
                               ),
                               column(4,
                                      numericInputIcon(ns("num_mw"),"Molecular Weight",value = 1,
                                                       min = 0,
                                                       width = validateCssUnit("100%"),
                                                       icon = list("g/mol"))
                               ),
                             ),
                             fluidRow(
                               
                               

                               column(3,
                                      numericInputIcon(ns("num_bw"),
                                                       label = "Body Weight",
                                                       value = 70,min = 0,
                                                       width = validateCssUnit("100%"),
                                                       icon = list("kg"))
                               ),
                               column(3,
                                      numericInputIcon(ns("num_qc"),
                                                       label = "Cardiac Output",
                                                       value = 421.96,
                                                       icon = list("L/h"),
                                                       width = validateCssUnit("100%"))
                               ),
                               
                               column(3,
                                      numericInputIcon(ns("num_lw"),
                                                       label = "Liver Weight",
                                                       value = 1.820,
                                                       icon = list("kg"),
                                                       width = validateCssUnit("100%"))
                               ),
                               column(3,
                                      numericInputIcon(ns("num_ql"),
                                                       label = "Blood Flow To Liver",
                                                       value = 90,
                                                       icon = list("L/h"),
                                                       width = validateCssUnit("100%"))
                               )
                               ),
                             
                               

                             
                           )
                  ),

                    
                  tabPanel("Invitro POD",
                           fluidRow(
                             column(8,offset = 2,
                                    fluidRow(
                                      column(6,
                                             numericInput(ns("num_ivc"),tags$h4("Invitro POD"),0,
                                                          width = validateCssUnit("100%"))
                                      ),
                                      column(6,
                                             selectInput(ns("sel_ivunit"),tags$h4("Unit"),
                                                         list("\ub5M"="um",
                                                              "mg/L"="mgL"),
                                                         width = validateCssUnit("100%"))
                                      )
                                    )
                             )

                           )
                           ),
                  tabPanel("HT-IVIVE Type",
                           fluidRow(
                             column(12,
                                    radioButtons(ns("rdo_rdtype"),label = tags$h4("HT-IVIVE Type"),inline = TRUE,
                                                 choices = c("Oral Exposure Non Volatile Chemical"="oralnonvol",
                                                             "Oral Exposure Volatile Chemical"="oralvol",
                                                             "Inhalation Exposure Volatile Chemical"="inhvol"))
                             )),
                           fluidRow(
                             column(6,
                                    shinyjs::hidden(numericInput(ns("num_pair"),
                                                                 "Blood-Air Partition Coefficient",
                                                                 0)))
                           )



                  ),
                  tabPanel("Hepatic Clearance",
                           fluidRow(tabsetPanel(id = ns("tab_heptype"),type = "pills",
                                 tabPanel("No Hepatic Clearance",value = "hep_null",
                                          tags$h3("No Hepatic Clearance")),
                                 tabPanel("Subcellular Clearance",value = "hep_sc",
                                          fluidRow(
                                            column(4,offset = 2,
                                                   numericInput(ns("num_mppgl"),
                                                                "Microsomal Protein /g Liver",
                                                                40)
                                            ),
                                            column(4,
                                                   numericInput(ns("num_cppgl"),
                                                                "Cytosolic Protein /g Liver",
                                                                80.7))
                                          ),
                                          fluidRow(
                                           column(6,
                                                  numericInput(ns("num_mscl"),
                                                               label = "Measured Microsomal Clearance",
                                                               step = 0.001,value = 0.0,
                                                               width = validateCssUnit("100%"))),
                                           column(6,
                                                  selectInput(ns("sel_msunit"),label = "Units",
                                                              choices = list("\ub5mol/min/mg Protein"="ummmP",
                                                                             "\ub5L/min/mg Protein"="ulmmP",
                                                                             "\ub5L/h/mg Protein"="ulhmP",
                                                                             "mL/min/mg Protein"="mlmmP",
                                                                             "mL/h/mg Protein"="mlhmP"),
                                                              width = validateCssUnit("100%")))
                                         ),
                                         fluidRow(
                                           column(6,
                                                  numericInput(ns("num_cycl"),
                                                               label = "Measured Cytosolic Clearance",
                                                               step = 0.001,value = 0.0,
                                                               width = validateCssUnit("100%"))),
                                           column(6,
                                                  selectInput(ns("sel_cyunit"),label = "Units",choices = list("\ub5L/min/mg Protein"="ulmmP",
                                                                                                          "\ub5L/h/mg Protein"="ulhmP",
                                                                                                          "mL/min/mg Protein"="mlmmP",
                                                                                                          "mL/h/mg Protein"="mlhmP"),
                                                              width = validateCssUnit("100%")))
                                         )
                                         ),
                                 tabPanel("S9 Fraction Clearance",value="hep_s9",
                                          fluidRow(
                                            column(6,
                                                   numericInput(ns("num_s9ppgl"),
                                                                "S9 Fraction/ g Liver",
                                                                value = 120.7))
                                          ),
                                         fluidRow(
                                           column(6,
                                                  numericInput(ns("num_s9cl"),
                                                               label = "Measured S9 Fraction Clearance",
                                                               value = 0,width = validateCssUnit("100%"))),
                                           column(6,
                                                  selectInput(ns("sel_s9unit"),
                                                              label = "Units",
                                                              choices = list("\ub5L/min/mg Protein"="ulmmP",
                                                                             "\ub5L/h/mg Protein"="ulhmP",
                                                                             "mL/min/mg Protein"="mlmmP",
                                                                             "mL/h/mg Protein"="mlhmP"),
                                                              width = validateCssUnit("100%")))
                                         )),
                                 tabPanel("Whole Hepatocyte Clearance",value ="hep_whole",
                                          fluidRow(
                                            column(4,
                                                   numericInput(ns("num_hpgl"),
                                                                "10^6 Hepatocytes/g Liver",
                                                                137)
                                            )
                                          ),
                                         fluidRow(
                                           column(6,
                                                  numericInput(ns("num_whcl"),
                                                               label = "Measured Whole Hepatocyte Clearance",
                                                               value = 0,width = validateCssUnit("100%"))),
                                           column(6,
                                                  selectInput(ns("sel_whunit"),label = "Units",
                                                              choices = list("L/h"="Lh",
                                                                             "L/h/10^6 Hepatocytes"="lhhep"),
                                                              width = validateCssUnit("100%")))
                                         )),
                                 tabPanel("Enzymatic Clearance",value = "hep_recomb",
                                          fluidRow(
                                            column(12,
                                                   DT::DTOutput(ns("cypDb")))
                                          ),
                                          fluidRow(

                                            column(4,
                                                   fileInput(ns("cypCl_upload"),"Upload CYP Clearance",
                                                             multiple = F,placeholder = "Select CSV File",
                                                             buttonLabel = icon("search"),
                                                             accept = c("text/csv")
                                                             )

                                                  ),
                                            column(2,
                                                   downloadLink(ns("cypCl_temp"),"Template for CYP Clearance Data")
                                                   ),
                                            column(6,
                                                   DT::DTOutput(ns("cypCl"))
                                                   )
                                            )
                                    
                                         )
                               )),
                           fluidRow(column(12,
                                           radioButtons(ns("rdo_cltype"),label = "Clearance Scaling",
                                                        choices = c("Rowland Equation"="cl_eq1",
                                                                    "Restrictive Clearance"="cl_eq2",
                                                                    "Non-restrictive Clearance"="cl_eq3"))
                                           )
                                    )
                  ),
                  tabPanel("Renal Clearance",
                           fluidRow(
                             column(4, offset = 2,
                                      numericInput(ns("num_gfr"),
                                                       label = "Glomerular Filteration Rate (L/h)",
                                                       value = 6.7,
                                                       width = validateCssUnit("100%")
                                                       ),

                                      checkboxInput(ns("ch_rencl"),label = "Include Renal Clearance",width = validateCssUnit("100%")),
                                      tags$h4("Renal Clearence is calculated as a product of Glomerular Filteration Rate measured in L/h
                                               and Fraction of chemical unbound in Blood Plasma")

                                    )
                           )
                           ),
                  tabPanel("Clearance in Blood",
                           fluidRow(
                             column(4, offset = 2,
                                    numericInput(ns("num_pbld"),
                                                 label = "Blood Plasma Partition Coefficient",
                                                 value = 1,width = validateCssUnit("100%")),

                                    numericInput(ns("num_plcl"),
                                                 label = "Measured Plasma Clearance (L/h)",
                                                 value = 0,width = validateCssUnit("100%")))
                           ))

    ),
    title = "Input HT-IVIVE data",
    size = "l",
    easyClose = FALSE,
    fade = TRUE,
    footer=tagList(
      actionButton(ns("ok"),"OK"),
      modalButton("Dismiss")
    )
  )
  )
}

#' server function of high throughput dosimetry
#' @description This function is needed internally by the package to handle the server functions related to
#' adding compounds in the HT-IVIVE UI. It is never intended to be called by the user.
#' @param input input object from the data input UI 
#' @param output output object from the data input UI
#' @param session session in which this module is called
#' @param vals values for clearance 
#' @param type IVIVE type
#' @param chem_list List of imported chemicals in the project
#' @param idx index of the row
#' @param row_selected row selected for editing
#' @export
HT_IVIVE <- function(input,output,session,vals="",type = "",chem_list = list(),idx = 0,row_selected = 0){
  #Get session ID
  ns <- session$ns
 

  # Create a dictionary for input ids to their natural language equivalent
  text_ui_dict <- list("ha"="Human Adult",
                       "ra"="Rat Adult",
                       "oralnonvol"="Oral Exposure Non Volatile Chemical",
                       "oralvol"="Oral Exposure Volatile Chemical",
                       "inhvol"="Inhalation Exposure Volatile Chemical",
                       "um"="\ub5M",
                       "mgL"="mg/L",
                       "ulmmP"="\ub5L/min/mg Protein",
                       "ulhmP"="\ub5L/h/mg Protein",
                       "mlmmP"="mL/min/mg Protein",
                       "mlhmP"="mL/h/mg Protein",
                       "Lh"="L/h",
                       "hep_null"="No Hepatic Clearance",
                       "hep_sc"="Sub Cellular Fraction",
                       "hep_s9"="S9 Fraction",
                       "hep_whole"="Whole Hepatocyte",
                       "hep_recomb"="Recombinant Enzymes")
  project_chems <- getProjectChemicalList()
  chem_set_choices <-c(getAllSetChoices("chem"),"Generic Chemical"=0)
  if (type == "add"){
    
    output$chem_output <- renderUI({
      fluidRow(
        column(12,
               selectizeInput(ns("sel_chem"),
                              label = "Select Chemical",
                              choices = chem_set_choices,
                              options =list(create = function(input){return(0)}))
               )#,
        # column(2,
        #        tags$h5("New"),
        #        dropdownButton(title = "Add Chemical",
        #          fluidPage(
        #            fluidRow(
        #              actionBttn(ns("btn_new_chem"),NULL,icon = icon("plus"),
        #                         style = "material-flat",size = "xs",block = T)
        #            )
        #          )
        #          
        #        )
        #        )
      )
      
      
    })
    
    output$org_output <- renderUI({
      selectInput(ns("sel_org"),label = "Select Organism",
                  choices = list("Human"="ha",
                                 "Rat"="ra"),
                  selected = "ha")
    })
    
    observeEvent(input$btn_new_chem,{
      showModal(modalDialog(title = "Add New Chemical",
        fluidPage(
          fluidRow(
            column(6,offset = 3,
                   textInput(ns("txt_new_chem_name"),NULL,
                             width = "100%",placeholder = "Chemical Name")
            )
          ),
          fluidRow(
            column(6,
                   numericInputIcon(ns("num_new_chem_mw"),"Molecular Weight",value = 0,
                                    min = 0,icon = list("g/mol"))
                   ),
            column(6,
                   numericInputIcon(ns("num_new_chem_km"),"Michaelis-Menten Constant",
                                    value = 1, min = 1, icon= list("\U00B5M")))
          )
        ),
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("btn_new_chem_ok"),"Add",width = "100%")
                         )
        ))
    })
    
   
    
    observeEvent(input$sel_chem,{
      chid <- input$sel_chem
      if(!is.null(chid)){
        if(chid == 0){
          fupls <- 1
          km <- 1
          mw <- 1
        }else{
          fupls <- project_chems[[as.integer(chid)]]["fupls"]
          km <- project_chems[[as.integer(chid)]]["km"]
          mw <- project_chems[[as.integer(chid)]]["mw"]
        }
        
        updateNumericInput(session,"num_fupls",value = as.numeric(fupls))
        updateNumericInput(session,"num_km",value = as.numeric(km))
        updateNumericInputIcon(session,"num_mw",value = as.numeric(mw))
      }
    },ignoreInit = T,ignoreNULL = T,priority = 10)
    
    
    # On Organism Change
    observeEvent(input$sel_org,{
      if(input$sel_org == "ha"){
        # default is to assume adult human age of 25 years
        age <- 25
        bw <- 81.2079
        qcc <- 421.96
        liv_wt <- 1.58
        liv_flw <- 99.5
        gfr <- 8.85
        updateNumericInput(session,"num_mppgl",value = 40)
        updateNumericInput(session,"num_cppgl",value = 80.7)
        updateNumericInput(session,"num_hpgl",value = 137)
        updateNumericInput(session,"num_s9ppgl",value = 120.7)
        updateNumericInput(session,"num_bw",value = bw)
        updateNumericInput(session,"num_lw",value = liv_wt)
        updateNumericInput(session,"num_qc",value = qcc)
        updateNumericInput(session,"num_ql",value =liv_flw)
        updateNumericInputIcon(session,"num_gfr",value =gfr)
        
      }else{
        updateNumericInput(session,"num_mppgl",value = 45)
        updateNumericInput(session,"num_cppgl",value = 91)
        updateNumericInput(session,"num_s9ppgl",value = 136)
        updateNumericInput(session,"num_hpgl",value = 110)
        updateNumericInput(session,"num_bw",value = 0.3518)
        updateNumericInput(session,"num_lw",value = 0.0136)
        updateNumericInput(session,"num_qc",value = 3.11)
        updateNumericInput(session,"num_ql",value =0.5709)
        updateNumericInputIcon(session,"num_gfr",value =0.228)
      }
    },ignoreInit = T,priority = 10)
    
    
  }else{
    # all the updates in this section come from existing data from vals
    # get the row to be edited
    row_data <- vals$m_table[row_selected,]
    
    # get the row key for the vals object. The row key is stored in a hidden rn column
    
    row_number <- vals$m_table[row_selected,]["rn"]
    row_key <- paste0("row_",row_number)
    row_values <- vals[[row_key]]
    chem_name <- names(chem_set_choices)[chem_set_choices == as.integer(row_values$sel_chem)]

    output$chem_output <- renderUI({
      tags$h4(chem_name)
    })
    organism <- switch(row_values$sel_org,
                       "ha"="Human",
                       "ra"="Rat")
    output$org_output <- renderUI({
      tags$h4(organism)
    })
    
    
    
    #update select inputs
    values <- row_values[grep("sel_",names(row_values),value = TRUE)]
    
    lapply(names(values),function(x){
      if (!(x %in% c("chem","org"))){
        updateSelectInput(session,x,selected = values[[x]])
      }
      })
    #update radio buttons
    values <- row_values[grep("rdo_",names(row_values),value = TRUE)]
    lapply(names(values),function(x){updateRadioButtons(session,x,selected = values[[x]])})
    #update tabitems
    values <- row_values[grep("tab_",names(row_values),value = TRUE)]
    lapply(names(values),function(x){updateTabsetPanel(session,x,selected = values[[x]])})
    #update checkbox inputs
    values <- row_values[grep("ch_",names(row_values),value = TRUE)]
    lapply(names(values),function(x){updateCheckboxInput(session,x,value = values[[x]])})
    #update text inputs
    values <- row_values[grep("txt_",names(row_values),value = TRUE)]
    lapply(names(values),function(x){updateTextInput(session,x,value = values[[x]])})
    #update numeric Values
    
    values <- row_values[grep("num_",names(row_values),value = TRUE)]
    lapply(names(values),function(x){updateNumericInput(session,x,value = values[[x]])})

    
  }
  
  
  
  #chem_names <- chem_list$chem_names
  output$org_name <- renderText({return("Standard Human")})
  observe({
    ivive_type <- input$rdo_rdtype
    if(!(is.null(ivive_type)) && (ivive_type == "oralvol" || ivive_type == "inhvol")){
      shinyjs::show("num_pair")
    }else{
      shinyjs::hide("num_pair")
    }
  })
  #Get Cyp Data from main database
  query <- "SELECT name,abundance,isef,fumic,loc FROM CypData;"
  cypdb <- mainDbSelect(query)
  cypOnt<-as.data.frame(getAllCypData(25),stringsAsFactors = F)
  cypdata <- merge(cypdb,cypOnt,by.x = "name",by.y = "Enzymes")
  cypdata[["Ontogeny"]]<- round(cypdata[["Ontogeny"]],0)
  output$cypDb <- DT::renderDT(DT::datatable(cypdata,
                                             caption = "CYP Data",
                                             rownames = NULL,autoHideNavigation = T,editable = T,
                                             colnames = c("Name","Abundance","ISEF","fumic","Location","Ontogeny"),
                                             options= list(dom = "tp",pageLength = 4)),server = T)
  # download handler for CSV file template
  output$cypCl_temp <- downloadHandler(
    filename= function(){return("Cyp_template.csv")},
    content = function(file){
      data <- data.frame("Names"= cypdb[["name"]],
                         "Clearance"= rep(0,length(cypdb[["name"]])),
                         stringsAsFactors = F)
      utils::write.csv(data,file,row.names = F)
    },
    contentType = c("text/csv")
  )
  # The selected file
  cypFile <- reactive({
    input$cypCl_upload
  })

  # The user's data, parsed into a data frame
  cypCl <- reactive({
    if(!(is.null(input$cypCl_upload))){
      ret_dat <- utils::read.csv(cypFile()$datapath)
    }else{
      ret_dat <- data.frame("Names"= cypdb[["name"]],"Clearance"= rep(0,length(cypdb[["name"]])),stringsAsFactors = F)
    }



    return(ret_dat)

  })


  output$cypCl <- DT::renderDT(DT::datatable(cypCl(),
                                             caption = "Clearence in \u00B5L/min/pmol",
                                             rowname = NULL,
                                             options= list(dom = "tp",pageLength = 4)),
                               server = T
  )

  
  # On Chemical Change
  
  
  
  
 
  if (type == "edit"){

    
  }

  observeEvent(input$ok,{
    # chemical Data
    mw <- input$num_mw
    name <- input$txt_IVIVE_name
    chem_name <- names(chem_set_choices)[which(chem_set_choices==as.integer(input$sel_chem))]
    # Type of reverse dosimetry
    rd_type  <- input$rdo_rdtype
    rd_type_name <- text_ui_dict[[rd_type]]
    if(rd_type=="inhvol"){
      stdexposure <- "1 mg/L"
    }else{
      stdexposure <- "1 mg/kg/day"
    }
    # Invitro concentration
    invitro_conc <- input$num_ivc
    invitro_unit <- text_ui_dict[[input$sel_ivunit]]
    invitro_data <- paste(as.character(invitro_conc),invitro_unit,sep = " ")
    # Hepatic Clearance
    hep_type <- input$tab_heptype
    hep_type_name <- text_ui_dict[[hep_type]]
    # if (is.null(hep_type)){
    #   hep_type_name <- "No Hepatic Clearance"
    # }else{
    #   hep_type_name <- text_ui_dict[[hep_type]]
    # }
    # Renal Clearance
    if(input$ch_rencl){
      ren_data <- "Include Renal Clearance"
    }else{
      ren_data <- "No Renal Clearance"
    }
    #Plasma Clearance
    if(input$num_plcl >0){
      pl_data <- "Include Plasma Clearance"
    }else{
      pl_data <- "No Plasma Clearance"
    }
    row_values <- list()

    input_list <- isolate(reactiveValuesToList(input))
    # get all numeric input values
    temp <- sapply(names(input_list),function(x){grepl("num_",x)})
    numeric_param_names_list <- names(temp[temp==TRUE])
    # get all select input and radio input values
    temp <- sapply(names(input_list),function(x){grepl("sel_",x)})
    select_param_names_list <- names(temp[temp==TRUE])
    # get all radio input values
    temp <- sapply(names(input_list),function(x){grepl("rdo_",x)})
    radio_param_names_list <- names(temp[temp==TRUE])
    # get all tab input values
    temp <- sapply(names(input_list),function(x){grepl("tab_",x)})
    tab_param_names_list <- names(temp[temp==TRUE])
    # get all checkbox inputs
    temp <- sapply(names(input_list),function(x){grepl("ch_",x)})
    chkbox_param_names_list <- names(temp[temp==TRUE])
    # get all text inputs
    temp <- sapply(names(input_list),function(x){grepl("txt_",x)})
    txt_param_names_list <- names(temp[temp=TRUE])
    row_values <-c(input_list[numeric_param_names_list],input_list[select_param_names_list],
                   input_list[radio_param_names_list],input_list[tab_param_names_list],
                   input_list[chkbox_param_names_list],input_list[txt_param_names_list],
                   cypCl())
    if (type == "add"){
      chem <- input$sel_chem
      org <- input$sel_org
    }else{
      
      # get the row key for the vals object. The row key is stored in a hidden rn column
      
      row_number <- vals$m_table[row_selected,]["rn"]
      row_key <- paste0("row_",row_number)
      chem <- vals[[row_key]]$sel_chem
      org <- vals[[row_key]]$sel_org
      row_values$sel_chem <- chem
      row_values$sel_org <- org
    }
    org_type_name <- switch(org,
                            "ha"="Human",
                            "ra"="Rat")

    # create the row that will either be added or replace existing row
    data_added<- data.table::data.table("rn"=0,
                                        "Name" = name,
                                        "Chemical"=chem_name,
                                        "Organism"=org_type_name,
                                        "Type" = rd_type_name,
                                        "Standard Exposure"=stdexposure,
                                        "Invitro POD"=invitro_data,
                                        "Hepatic Clearance"=hep_type_name,
                                        "Renal Clearance"=ren_data,
                                        "Plasma Clearance"=pl_data)
    # add a new row if add button was clicked else edit the existing row
    if (type == "add"){
      row_key <- paste0("row_",idx)
      data_added$rn <- idx
      vals$m_table <- rbind(vals$m_table,data_added)
      vals[[row_key]]<- row_values
    }
    else{
      row_number <- vals$m_table[row_selected,]["rn"]
      row_key <- paste0("row_",row_number)
      vals[[row_key]]<- row_values
      data_added$rn <- as.numeric(row_number)
      vals$m_table[row_selected,]<- data_added
    }

    # close the modal
    removeModal()

  })
  return(vals)
}


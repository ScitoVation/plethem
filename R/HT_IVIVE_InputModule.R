#' Module for editing high throughput reverse dosimetry functions
#' @description The UI for defining HT-IVIVE parameters in the HT-IVIVE project. 
#' It is called by the HT-IVIVE server script when a new row is added or existing row is edited. It is never called directly by the user.
#' @param namespace namespace for the module. This is unique and decided by the project server function
#' @export
HT_IVIVEUI <- function(namespace=""){
  css <- "div .modal-lg {
  width:1000px

  }"

  ns <- NS(namespace)
  showModal(modalDialog(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(css),
      tabsetPanel(id = "setupTabs",
                  tabPanel("Physiologcal and Chemical Parameters",
                           fluidPage(
                             fluidRow(
                               column(4,offset = 2,
                                      tags$h5("Oragnism"),
                                      tags$h4("Standard Human")
                                      # selectInput(ns("sel_org"),label = "Select Organism",
                                      #             choices = list("Standard Human"="ha",
                                      #                            "Adult Rat"="ra"),
                                      #             selected = "ha")
                                      ),
                               column(4,
                                      selectInput(ns("sel_chem"),
                                                  label = "Select Chemical",
                                                  choices = list())
                                      )
                             ),
                             fluidRow(
                               column(4,
                                      numericInput(ns("num_bw"),
                                                   label = "Body Weight(kg)",
                                                   value = 70,
                                                   width = validateCssUnit("100%"))
                                      ),
                               column(4,
                                      numericInput(ns("num_qc"),
                                                   label = "Cardiac Output(L/h)",
                                                   value = 421.96,
                                                   width = validateCssUnit("100%"))
                                      ),

                               column(4,
                                      numericInput(ns("num_fup"),
                                                   label = "Fraction unbound in Plasma",
                                                   value = 1,width = validateCssUnit("100%"))
                               )
                               ),
                             fluidRow(
                               column(4,
                                      numericInput(ns("num_km"),
                                                   "Michelis Menten Constant",
                                                   1)
                                      ),

                               column(4,
                                      numericInput(ns("num_lw"),
                                                   label = "Liver Weight(kg)",
                                                   value = 1.820,
                                                   width = validateCssUnit("100%"))
                                      ),
                               column(4,
                                      numericInput(ns("num_ql"),
                                                   label = "Blood Flow To Liver(L/h)",
                                                   value = 90,
                                                   width = validateCssUnit("100%"))
                                      )

                             )
                           )
                  ),

                           # sidebarLayout(
                           #   sidebarPanel(
                           #    fluidRow(
                           #      ,
                           #     # selectInput(ns("sel_chem"),label = "Select Chemical",choices = list())
                           #    )
                           #   ),
                           #   mainPanel(
                           #     fluidRow(
                           #       column(6,
                           #              numericInput(ns("num_pbld"),label = "Blood Plasma Partition Coefficient",value = 1,width = validateCssUnit("100%"))
                           #       ),
                           #       column(6,
                           #              numericInput(ns("num_pair"),label = "Blood Air Partition Coefficient",value = 1,
                           #                           width = validateCssUnit("100%")))
                           #     ),
                           #     fluidRow(
                           #       column(6,
                           #              numericInput(ns("num_fup"),label = "Fraction unbound in Plasma",value = 1,width = validateCssUnit("100%"))),
                           #       column(6,
                           #              numericInput(ns("num_gfr"),label = "Glomerular Filteration Rate",value = 10,width = validateCssUnit("100%")))
                           #     ),
                           #     fluidRow(
                           #       column(6,
                           #              numericInput(ns("num_bw"),label = "Body Weight(kg)",value = 10,width = validateCssUnit("100%"))),
                           #       column(6,
                           #              numericInput(ns("num_qc"),label = "Cardiac Output(L/h)",value = 10,width = validateCssUnit("100%")))
                           #     ),
                           #     fluidRow(
                           #       column(6,
                           #              numericInput(ns("num_lw"),label = "Liver Weight(kg)",value = 10,width = validateCssUnit("100%"))),
                           #       column(6,
                           #              numericInput(ns("num_ql"),label = "Blood Flow To Liver(L/h)",value = 10,width = validateCssUnit("100%")))
                           #     )
                           #   )
                           # )),

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
                                                         list("	\u03BCM"="um",
                                                              "mg/L"="mgL"),
                                                         width = validateCssUnit("100%"))
                                      )
                                    )
                             ),
                             column(6,offset =2,
                                      shiny::actionButton(ns("btn_getIVC"),"Get Invitro POD from MoAViz")
                                      ),
                             column(6, offset = 2,
                                    DT::DTOutput(ns("ivd_tble")))
                             

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
                                                                "Micoromal Protein /gm Liver",
                                                                40)
                                            ),
                                            column(4,
                                                   numericInput(ns("num_cppgl"),
                                                                "Cytosolic Protein /gm Liver",
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
                                                              choices = list("\u03BCmol/min/mg Protein"="ummmP",
                                                                             "\u03BCL/min/mg Protein"="ulmmP",
                                                                             "\u03BCL/h/mg Protein"="ulhmP",
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
                                                  selectInput(ns("sel_cyunit"),label = "Units",choices = list("\u03BCL/min/mg Protein"="ulmmP",
                                                                                                          "\u03BCL/h/mg Protein"="ulhmP",
                                                                                                          "mL/min/mg Protein"="mlmmP",
                                                                                                          "mL/h/mg Protein"="mlhmP"),
                                                              width = validateCssUnit("100%")))
                                         )
                                         ),
                                 tabPanel("S9 Fraction Clearance",value="hep_s9",
                                          fluidRow(
                                            column(6,
                                                   numericInput(ns("num_s9ppgl"),
                                                                "S9 Fraction/ gm Liver",
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
                                                              choices = list("\u03BCL/min/mg Protein"="ulmmP",
                                                                             "\u03BCL/h/mg Protein"="ulhmP",
                                                                             "mL/min/mg Protein"="mlmmP",
                                                                             "mL/h/mg Protein"="mlhmP"),
                                                              width = validateCssUnit("100%")))
                                         )),
                                 tabPanel("Whole Hepatocyte Clearance",value ="hep_whole",
                                          fluidRow(
                                            column(4,
                                                   numericInput(ns("num_hpgl"),
                                                                "10^6 Hepatocytes/gm Liver",
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
                                                   fileInput(ns("cypCl_upload"),"Upload Cyp Clearence",
                                                             multiple = F,placeholder = "Select CSV File",
                                                             buttonLabel = icon("search"),
                                                             accept = c("text/csv")
                                                             )

                                                  ),
                                            column(2,
                                                   downloadLink(ns("cypCl_temp"),"Template for the CSV file")
                                                   ),
                                            column(6,
                                                   DT::DTOutput(ns("cypCl"))
                                                   )
                                            )
                                         # fluidRow(
                                         #   column(6,offset = 4,
                                         #          tags$h4("Clerance in \u03BCL/min/pmol"))
                                         # ),
                                         # fluidRow(
                                         #   column(6,
                                         #          numericInput(ns("num_cyp1a2cl"),label="Measured CYP1A2 Clearance",
                                         #                       value = 0,width = validateCssUnit("100%"))),
                                         #   column(6,
                                         #          numericInput(ns("num_cyp2b6cl"),label="Measured CYP2B6 Clearance",
                                         #                       value = 0,width = validateCssUnit("100%")))
                                         # ),
                                         # fluidRow(
                                         #   column(6,
                                         #          numericInput(ns("num_cyp3a4cl"),label="Measured CYP3A4 Clearance",
                                         #                       value = 0,width = validateCssUnit("100%"))),
                                         #   column(6,
                                         #          numericInput(ns("num_cyp2c19cl"),label="Measured CYP2C19 Clearance",
                                         #                       value = 0,width = validateCssUnit("100%")))
                                         # ),
                                         # fluidRow(
                                         #   column(6,
                                         #          numericInput(ns("num_cyp2c9cl"),label="Measured CYP2C9 Clearance",
                                         #                       value = 0,width = validateCssUnit("100%"))),
                                         #   column(6,
                                         #          numericInput(ns("num_cyp3a5cl"),label="Measured CYP3A5 Clearance",
                                         #                       value = 0,width = validateCssUnit("100%")))
                                         # ),
                                         # fluidRow(
                                         #   column(6,
                                         #          numericInput(ns("num_ces1mcl"),label="Measured CES1M Clearance",
                                         #                       value = 0,width = validateCssUnit("100%"))),
                                         #   column(6,
                                         #          numericInput(ns("num_ces1ccl"),label="Measured CES1C Clearance",
                                         #                       value = 0,width = validateCssUnit("100%")))
                                         # ),
                                         # fluidRow(
                                         #   column(6,
                                         #          numericInput(ns("num_ces2mcl"),label="Measured CES2M Clearance",
                                         #                       value = 0,width = validateCssUnit("100%"))),
                                         #   column(6,
                                         #          numericInput(ns("num_ces2ccl"),label="Measured CES2C Clearance",
                                         #                       value = 0,width = validateCssUnit("100%")))
                                         # )
                                         )
                               )),
                           fluidRow(column(12,
                                           radioButtons(ns("rdo_cltype"),label = "Clearance Scaling",
                                                        choices = c("Rowland Equation"="cl_eq1",
                                                                    "Restrictive Clearance"="cl_eq2",
                                                                    "Non-restrictive Clearance"="cl_eq3"))
                                           )
                                    )
                           # dashboardPage(
                           #   dashboardHeader(disable = TRUE),
                           #   dashboardSidebar(sidebarMenu(id = ns("heptype"),
                           #                                menuItem("Sub cellular Fraction",tabName = "hep_sc"),
                           #                                menuItem("S9 Fraction",tabName = "hep_s9"),
                           #                                menuItem("Whole Hepatocytes",tabName = "hep_whole"),
                           #                                menuItem("Recombinant Enzymes",tabName = "hep_recomb"))),
                           #   dashboardBody(

                             # )
                             # )
                  ),
                  tabPanel("Renal Clearance",
                           fluidRow(
                             column(8, offset = 2,
                                      numericInput(ns("num_gfr"),
                                                   label = "Glomerular Filteration Rate",
                                                   value = 6.7,
                                                   width = validateCssUnit("100%")),

                                      checkboxInput(ns("ch_rencl"),label = "Include Renal Clearance",width = validateCssUnit("100%")),
                                      tags$h4("Renal Clearence is calculated as a product of Glomerular Filteration Rate measured in L/h
                                               and Fraction of chemical unbound in Blood Plasma")

                                    )
                           )
                           ),
                  tabPanel("Clearance in Blood",
                           fluidRow(
                             column(8, offset = 2,
                                    numericInput(ns("num_pbld"),
                                                 label = "Blood Plasma Partition Coefficient",
                                                 value = 1,width = validateCssUnit("100%")),

                                    numericInput(ns("num_plcl"),
                                                 label = "Measured Plasma Clearance",
                                                 value = 0,width = validateCssUnit("100%")))
                           ))

    ),
    title = "HT modal",
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
                       "um"="\u03BCm",
                       "mgL"="mg/L",
                       "ulmmP"="\u03BCL/min/mg Protein",
                       "ulhmP"="\u03BCL/h/mg Protein",
                       "mlmmP"="mL/min/mg Protein",
                       "mlhmP"="mL/h/mg Protein",
                       "Lh"="L/h",
                       "hep_null"="No Hepatic Clearance",
                       "hep_sc"="Sub Cellular Fraction",
                       "hep_s9"="S9 Fraction",
                       "hep_whole"="Whole Hepatocyte",
                       "hep_recomb"="Recombinant Enzymes")
  chem_list <- getProjectChemicalList()
  chem_set_choices <- getAllSetChoices("chem")
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
  # get invitro data from MoAviz if it exists
  conc_table <- eventReactive(input$btn_getIVC,{
    species <- "human"
    chid <- as.integer(input$sel_chem)
    query <- sprintf("Select cas From ChemicalSet Where chemid = %i",chid)
    casrn <-"64-17-5" #projectDbSelect(query)$cas
    url2Call <- sprintf("http://scyld.ciit.org/api/getVitroConc.php?species=human&cas=%s",casrn)
    returned_data<- rjson::fromJSON(file = url2Call)
    
    conc_table <- data.frame()
    if (length(returned_data)>0){
      for (temp_var in returned_data){
        conc_val <- temp_var$concentration
        conc_units <- ifelse(temp_var$concentration_units == "uM","\u00B5M","mg\\L")
        tble_row <- cbind(conc_val,conc_units)
        conc_table <- rbind(conc_table,tble_row)
      }
      colnames(conc_table)<- c("Value","Units")
      return(conc_table)
    }
    
    
    
  })
  output$ivd_tble <- DT::renderDT(DT::datatable(conc_table(),
                                                selection = list(mode = "single")))
  observeEvent(input$ivd_tble_rows_selected,{
    row_idx <- input$ivd_tble_rows_selected
    conc_val <- conc_table()$Value[[row_idx]]
    units <- tolower(conc_table()$Units[[row_idx]])
    updateNumericInput(session,"num_ivc",value = as.numeric(conc_val))
    updateSelectInput(session,"sel_ivunit",selected = units)
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

  # update the chem choices as they might have changed.
  updateSelectInput(session,"sel_chem",choices = chem_set_choices)
  # On Chemical Change
  observeEvent(input$sel_chem,{
    chid <- input$sel_chem
    if(!is.null(chid)){
      #print(chem_list)
      fupls <- chem_list[[as.integer(chid)]]["fupls"]
      km <- chem_list[[as.integer(chid)]]["km"]
      updateNumericInput(session,"num_fup",value = as.numeric(fupls))
      updateNumericInput(session,"num_km",value = as.numeric(km))
      }
    },priority = 10)




  # # Get global table values
  # mgpglTble <- reactive({
  #   if (input$sel_org == "ha"){
  #     MPCPPGL <- calcMPCPPGL(25)
  #     #MPCPPGL <- as.vector(MPCPPGL)
  #     MPPGL <- signif(MPCPPGL$MPPGL,4)
  #     CPPGL <- signif(MPCPPGL$CPPGL,4)
  #   }else{
  #     MPPGL <- 0.0
  #     CPPGL <- 0.0
  #   }
  #   return(data.frame("Fraction"=c("Microsomal","Cytosolic"),
  #                     "Values"=c(MPPGL,CPPGL)))
  # })
  # output$mgpglTble <- DT::renderDT(DT::datatable(mgpglTble(),
  #                                             caption = "Proteins/gm Liver Values",
  #                                             rownames = NULL,
  #                                             editable = T,
  #                                             autoHideNavigation = T,
  #                                             options= list(dom = "t")),server = T)


  #
  if (type == "edit"){

    # all the updates in this section come from existing data from vals
    # get the row to be edited
    row_data <- vals$m_table[row_selected,]

    # get the row key for the vals object. The row key is stored in a hidden rn column

    row_number <- vals$m_table[row_selected,]["rn"]
    row_key <- paste0("row_",row_number)
    row_values <- vals[[row_key]]

    #update numeric Values
    values <- row_values[grep("num_",names(row_values),value = TRUE)]
    lapply(names(values),function(x){updateNumericInput(session,x,value = values[[x]])})
    #update select inputs
    values <- row_values[grep("sel_",names(row_values),value = TRUE)]
    lapply(names(values),function(x){updateSelectInput(session,x,selected = values[[x]])})
    #update radio buttons
    values <- row_values[grep("rdo_",names(row_values),value = TRUE)]
    lapply(names(values),function(x){updateRadioButtons(session,x,selected = values[[x]])})
    #update tabitems
    values <- row_values[grep("tab_",names(row_values),value = TRUE)]
    lapply(names(values),function(x){updateTabsetPanel(session,x,selected = values[[x]])})
    #update checkbox inputs
    values <- row_values[grep("ch_",names(row_values),value = TRUE)]
    lapply(names(values),function(x){updateCheckboxInput(session,x,value = values[[x]])})
  }

  # On Organism Change
  observeEvent(input$sel_org,{
    if(input$sel_org == "ha"){
      # default is to assume adult human age of 25 years
      age = 25
      bw <- 70
      qcc <- 421.96
      liv_wt <- 1.820
      liv_flw <- 90
      gfr <- 6.7
      updateNumericInput(session,"num_bw",value = bw)
      updateNumericInput(session,"num_lw",value = liv_wt)
      updateNumericInput(session,"num_qc",value = qcc)
      updateNumericInput(session,"num_ql",value =liv_flw)
      updateNumericInput(session,"num_gfr",value =gfr)

    }else{
      updateNumericInput(session,"num_bw",value = 0.3518)
      updateNumericInput(session,"num_lw",value = 0.0136)
      updateNumericInput(session,"num_qc",value = 3.11)
      updateNumericInput(session,"num_ql",value =0.5709)
      updateNumericInput(session,"num_gfr",value =0.228)
    }
  })
  observeEvent(input$ok,{
    # chemical Data
    chem <- input$sel_chem
    # Organism data
    org <- input$sel_org
    org_type_name <- "Standard Human"
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
    row_values <-c(input_list[numeric_param_names_list],input_list[select_param_names_list],
                   input_list[radio_param_names_list],input_list[tab_param_names_list],
                   input_list[chkbox_param_names_list],cypCl())

    # create the row that will either be added or replace existing row
    data_added<- data.table::data.table("rn"=0,
                                        "Chemical"=chem_list[[as.integer(chem)]]["names"],
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

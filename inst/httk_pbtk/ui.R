library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(V8)
library(ggplot2)
library(shinyjs)


jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('#'+name);

  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled-comp');
}

shinyjs.enableTab = function(name) {
  var tab = $('#'+name);
  tab.unbind('click.tab');
  tab.removeClass('disabled-comp');
}
"
css <- "div .disabled-comp {
  color: #aaa !important;
  border-color: #a00 !important;
  background-color: #a00 !important;

}"


################################exposure sidebar
expo_sidebar <- shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
  id="ms_expo_sidebar",
  tags$div(actionButton("clear_expo","Reset exposures")),
  menuItem("Oral", tabName = "oral", selected = TRUE),
  menuItem("Intravenous", tabName = "iv")
))

################################compartment sidebar
comp_sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(id="compsidebar",

              menuItem(tags$h5(tags$span(style = "color:white",
                                         "Physiological Parameters")),
                       tabName = "physiolocal_parameters", selected = TRUE),
              menuItem("Blood",tabName="blood"),
              menuItem("Gut",tabName = "gut"),
              menuItem("Liver",tabName = "liver"),
              menuItem("Kidney",tabName = "kidney"),
              menuItem("Lungs",tabName="lung"),
              menuItem("Rest of the body",tabName="rob")

              )

  )


###############################Chemical Sidebar
chem_sidebar <- shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
  id="chem_sidebar",
  selectizeInput("qsarModelChem", "Select A QSAR Model",
                          choices = c("QSAR Model One" = 'one',
                                      "QSAR Model Two" = 'two'),
                          width = "99%"),
  shinyBS::bsButton("qsar4chem_props","Calculate Chemical Params",style = "primary"),
  shinyBS::bsButton("btn_ivive_chem","Perform IVIVE",style = "primary")
)
  #actionButton("newChem", "Modify Chemical DB"),
  #selectizeInput("selectedChem", "Select a chemical", choices = getAllMainChemicals(), width = validateCssUnit("99%")),
  #awesomeCheckbox("useQSar", "Use QSAR Model", value = FALSE),


)


###########################plots-Body
plot_body <- fluidPage(
  tabItem(tabName = "plots",
          fluidRow(
            bsCollapse(id="plts", multiple = TRUE,
                       # bsCollapsePanel(title = "Exposure Plots", style = "primary",
                       #               column(3,
                       #                  wellPanel(
                       #                             checkboxInput("ch_dose", "Instaneous Dose", value = TRUE, width = NULL),
                       #                             checkboxInput("ch_totdose", "Total Dose", value = TRUE, width = NULL)
                       #                  )
                       # ),
                       # column(9,
                       #        tabBox(width=12, height = validateCssUnit("100%"),
                       #               tabPanel("Plot",
                       #                        fluidRow(
                       #                          column(4,offset = 4,
                       #                                 radioButtons("r_expo_type",label = "Select Exposure",inline = TRUE,
                       #                                              choices = c("Active"="act","All"="all"),
                       #                                              selected = "act"))
                       #                        ),
                       #                        fluidRow(
                       #                          plotly::plotlyOutput("exposureplt")
                       #                        )
                       #               ),
                       #               tabPanel("Table",
                       #                        DT::DTOutput("expotble"),
                       #                        downloadButton("expodwnld",label = "Get Data")))
                       # )),
                       bsCollapsePanel(title = "Concentration Plots", style = "primary",
                                 column(3,
                                        tabsetPanel(
                                          tabPanel("Model",value  = "model",
                                                   shinyWidgets::multiInput("cplt_comp",label = tags$h4("Select Compartment"),
                                                              choices = list("Arterial Blood"="art_bld","Venous Blood"="ven_bld",
                                                                             "Plasma"="to_plasma",
                                                                             "Lungs"="to_lng",
                                                                             "Gut"="to_gut",
                                                                             "Liver"="to_liv",
                                                                             "Kidney"="to_kdn",
                                                                             "Rest of the Body"="to_rest"
                                                                             )
                                                   )
                                                   ),
                                          tabPanel("Dataset",value = "dataset",
                                                   shinyWidgets::pickerInput("cplt_data",multiple = FALSE,
                                                                            label = tags$h4("Select Datasets"),
                                                                            choices = c("No Dataset"="none"),#,
                                                                            selected = "none")
                                                   )



                                          )
                                        ),
                                  column(9,
                                         tabBox(width = 12,height = validateCssUnit("100%"),
                                                tabPanel("Plot",
                                                         fluidRow(
                                                           tags$h5(class="text-center",
                                                                   radioButtons("r_cplt_type",label = "Concentration Units",inline = TRUE,
                                                                                choices = c("mg/L"="mgl","\u00B5Molar"="um"),
                                                                                selected = "um")
                                                           )
                                                         ),
                                                         fluidRow(
                                                           plotly::plotlyOutput("concplt")
                                                         )
                                                ),
                                                tabPanel("Table",
                                                         DT::DTOutput("conctble"),
                                                         downloadButton("cdwnld",label ="Get Data"))
                                         ))
                                 ),
                       bsCollapsePanel(title = "Amount Plots", style = "primary",
                                       column(3,
                                              wellPanel(
                                                multiInput("aplt_comp",label = tags$h4("Select Compartment"),
                                                               choices = list("Amount in Gut Lumen"="agutlumen",
                                                                              "Amount Metabolized"="ametab",
                                                                              "Amount in Renal Tubules"="atubules")
                                                )
                                                )

                                              ),

                                       column(9,
                                              tabBox(width = 12,height = validateCssUnit("100%"),
                                                     tabPanel("Plot",
                                                              fluidRow(
                                                                tags$h5(class="text-center",
                                                                        radioButtons("r_aplt_type",label = "Concentration Units",inline = TRUE,
                                                                                     choices = c("mg"="mg","\u00B5moles"="um"))
                                                                )
                                                              ),
                                                              fluidRow(
                                                                plotly::plotlyOutput("amtplt")
                                                              )
                                                     ),
                                                     tabPanel("Table",
                                                              DT::DTOutput("amttble"),
                                                              downloadButton("amwnld",label ="Get Data"))
                                              ))
                                       )
                       # bsCollapsePanel(title = "AUC Plots", style = "primary",
                       #                 column(3,
                       #                        wellPanel(
                       #                          multiInput("aucplt_comp",label = tags$h4("Select Compartment"),
                       #                                         choices = list("Blood"=c("Aretial Blood"="art_bld","Venous Blood"="ven_bld"),
                       #                                                        "Fat"=c("Fat Total"="to_fat","Fat Tissue"="ti_fat","Fat Exchange"="bl_fat"),
                       #                                                        "Skin"=c("Skin Total"="to_skn","Skin Tissue"="ti_skn","Skin Exchange"="bl_skn"),
                       #                                                        "Bone"=c("Bone Total"="bl_skn","Bone Tissue"="ti_bne","Bone Exchange"="bl_bne"),
                       #                                                        "Muscle"=c("Muscle Total"="to_musc","Muscle Tissue"="ti_musc","Muscle Exchange"="bl_musc"),
                       #                                                        "Brain"=c("Brain Total"="to_brn","Brain Tissue"="ti_brn","Brain Exchange"="bl_brn"),
                       #                                                        "Lungs"=c("Lungs Total"="to_lng","Lungs Tissue"="ti_lng","Lungs Exchange"="bl_lng"),
                       #                                                        "Heart"=c("Heart Total"="to_hrt","Heart Tissue"="ti_hrt","Heart Exchange"="bl_hrt"),
                       #                                                        "GI"=c("GI Total"="to_gi","GI Tissue"="ti_gi","GI Exchange"="bl_gi"),
                       #                                                        "Liver"=c("Liver Total"="to_liv","Liver Tissue"="ti_liv","Liver Exchange"="bl_liv"),
                       #                                                        "Kidney"=c("Kidney Total"="to_kdn","Kidney Tissue"="ti_kdn","Kidney Exchange"="bl_kdn"),
                       #                                                        "Rapidly Perused"=c("Rapidly Perused Total"="to_rpf",
                       #                                                                            "Rapidly Perused Tissue"="ti_rpf","Rapidly Perused Exchange"="bl_rpf"),
                       #                                                        "Slowly Perused"=c("Slowly Perused Total"="to_spf",
                       #                                                                           "Slowly Perused Tissue"="ti_spf","Slowly Perused Exchange"="bl_spf")
                       #                                                        )
                       #                          )
                       #                          )
                       #                 ),
                       #                 column(9,
                       #                        tabBox(width = 12, height = validateCssUnit("100%"),
                       #                               tabPanel("Plot",
                       #                                        fluidRow(
                       #                                          plotOutput("aucplt")
                       #                                        )
                       #                               ),
                       #                               tabPanel("Table",
                       #                                        dataTableOutput("auctble"),
                       #                                        downloadButton("aucdwnld",label ="Get Data"))
                       #                        )
                       #                    )
                       #                 ),
                       # bsCollapsePanel(title = "Mass Balance Plots", style = "primary",
                       #                 column(8, offset = 2,
                       #                        tabBox(width = 12, height = validateCssUnit("100%"),
                       #                               tabPanel("Plot",
                       #                                        fluidRow(
                       #                                          plotOutput("balplt")
                       #                                        )
                       #                               ),
                       #                               tabPanel("Table",
                       #                                        DT::DTOutput("baltble"),
                       #                                        downloadButton("cmbaldwnld",label ="Get Data"))
                       #                        ))
                       #                 )
            )
          ))
)


#####################compartment Body
comp_body <- dashboardBody(
  # dropdown(
  #   pickerInput("comp_sel","Select Organism",inline = TRUE,choices = NULL,options= list(title = "Add Compartment")),
  #   actionBttn("add_comp",NULL,style = "material-circle",color = "primary", icon = icon("plus"))
  #
  # ),
  tabItems(
    tabItem(
      tabName = "physiolocal_parameters",
      fluidRow(
        # column(4,
        #        selectInput("ms_Org", label = "Organism",
        #                    choices = list("Human" = "Human","Rat"= "Rat"),
        #                    selected = "Human")
        #        ),
        column(6,
               selectInput("ms_Gender", label = "Gender",
                           choices = list("Male" = "M", "Female" = "F"),
                           selected = "M")
        ),
        column(6,
               numericInput("ms_Age","Age",25,0.5,80,1)
        )
      ),

      fluidRow(
        column(4,
               numericInput("ms_BW","Body Weight (kg)",70,0.1,90,0.1)),
        column(4,
               numericInput("ms_Qcardiacc","Cardiac Output (L/h)",420,1,50,0.1)),
        column(4,
               numericInput("ms_hematocrit","Hematocrit Factor",0.441,0,1,0.001))

      ),
      fluidRow(

        column(4,
               numericInput("ms_Qgfrc","Glomerular Filteration (L/h/kg BW)",min =0 , max =1, value =0.08)
               ),
        column(4,
               numericInput("ms_Fgutabs","Fraction of the Oral Dose Absorbed",1)
               ),
        column(4,
               numericInput("ms_kgutabs","Rate of Absorption into Gut (1/h)",1)
        )
        )
      ),
    tabItem(
      tabName = "gut",
      fluidRow(
        column(6,
               numericInput("ms_Vgutc","Volume Ratio",min =0, max = 1, value =0.0222, step = 0.01)),
        column(6,
               numericInput("ms_Qgutf","Blood Flow Ratio",min =0 , max =1, value =0.1139))
      ),

      fluidRow(class="",
               column(6,
                      numericInput("ms_Kgut2pu","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "liver",
      fluidRow(
        column(6,
               numericInput("ms_Vliverc","Volume Ratio",min =0, max = 1, value =0.0222, step = 0.01)),
        column(6,
               numericInput("ms_Qliverf","Blood Flow Ratio",min =0 , max =1, value =0.1139))
      ),

      fluidRow(class="",
               column(6,
                      numericInput("ms_Kliver2pu","Partition Coefficient",0.5,0,2, 0.01)
                      ),
               column(6,
                      numericInput("ms_million.cells.per.gliver","Hepatocellularity",110)
                      )
               ),
      fluidRow(
        column(6,
               numericInput("ms_Fhep.assay.correction","Hepatocyte Assay Correction",1)
               )
      )
    ),
    tabItem(
      tabName = "blood",
      fluidRow(
        column(6,
               numericInput("ms_Vartc","Arterial Volume Ratio",min =0, max = 1, value =0.0222, step = 0.01)
               ),
        column(6,
               numericInput("ms_Vvenc","Venous Volume Ratio",min =0 , max =1, value =0.1139)
               )
      ),

      fluidRow(class="",
               column(6,
                      numericInput("ms_Krbc2pu","RBC to Plasma Partition Coefficient",0.5,0,2, 0.01)
                      )
               )
    ),
    tabItem(
      tabName = "kidney",
      fluidRow(
        column(6,
               numericInput("ms_Vkidneyc","Volume Ratio",min =0, max = 1, value =0.0222, step = 0.01)),
        column(6,
               numericInput("ms_Qkidneyf","Blood Flow Ratio",min =0 , max =1, value =0.1139))
      ),

      fluidRow(class="",
               column(6,
                      numericInput("ms_Kkidney2pu","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "lung",
      fluidRow(
        column(6,
               numericInput("ms_Vlungc","Volume Ratio",min =0, max = 1, value =0.0222, step = 0.01)),
        column(6,
               numericInput("ms_Qlungf","Blood Flow Ratio",min =0 , max =1, value =0.1139))
      ),

      fluidRow(class="",
               column(6,
                      numericInput("ms_Klung2pu","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "rob",
      fluidRow(
        column(6,
               numericInput("ms_Vrestc","Volume Ratio",min =0, max = 1, value =0.0222, step = 0.01)),
        column(6,
               numericInput("ms_Qrestf","Blood Flow Ratio",min =0 , max =1, value =0.1139))
      ),

      fluidRow(class="",
               column(6,
                      numericInput("ms_Krest2pu","Partition Coefficient",0.5,0,2, 0.01)))
    )
  )
)

####################chemical Body
chem_body <- dashboardBody(
  fluidPage(id="Chemicals",
            tags$h4("Chemical Parameters", class="pager-header"),
            fluidRow(
              column(6,
                     selectInput("ms_Corg","Organism",choices = list("Human"="Human","Rat"="Rat"))
                     ),
              column(6,
                     numericInput("ms_Clint","Instrinsic Clearance",1))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_LogMA","Membrane Affinity",1)),
              column(6,
                     numericInput("ms_MW","Molecular Weight (g/mol)",1,0,250,0.01))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_LogP","Octanol-water partition Coefficient",1,0,250,0.01)),
              column(6,
                     numericInput("ms_LogPwa","Water-air Partition Coefficient",1,0,250,0.01))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_pKa_Accept","Accept Pka",1,0,250,0.01)),
              column(6,
                     numericInput("ms_pKa_Donor","Donor Pka",1,0,250,0.01))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_Clmetabolismc","First Order metabolism in Liver (L/h/kg BW)",1)
                     ),
              column(6,
                     numericInput("ms_Funbound.plasma", label = "Fraction Unbound in Palsma", value = 1, 0, 1, 0.001))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_Rblood2plasma","Ratio of Chemical in Blood to Plasma",1)
              )
            )
            )
  )



######################exposure Body
expo_body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "oral",
      fluidRow(
        column(6,
               numericInput("ms_bdose",label="Daily Oral Dose (mg/kg BW)", value =0, step= 0.01)),
        column(6,
               numericInput("ms_blen",label = "Total length of dosing (h/day)", value = 1, step = 0.1))
        ),
      fluidRow(
        column(6,
               numericInput("ms_breps","Number of Bolus Doses",0,0,100, 1)),
        column(6,
               awesomeCheckbox("ms_brep_flag","Repeat Dose Daily?",value = FALSE))
      )


    ),
    tabItem(tabName ="dw",
            fluidRow(
              column(6,
                     numericInput("ms_drdose",label = "Chemical Concentration (mg/L)", value = 0, step = 0.1)),
              column(6,
                     numericInput("ms_vdw", "Volume of Drinking Water per Bolus (L)", value = 0, step = 0.1))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_dreps", "Number of drinking water doses per day", value = 1, step = 0.1 )
              )
            )
    ),
    tabItem(
      tabName = "inh",
      fluidRow(
        column(4,
               numericInput("ms_inhdose",label="Inhalation Dose (ppm)", value =0, step= 0.01)),
        column(4,
               numericInput("ms_inhtlen",label = "Length of Inhalation Dose (h)",0)),
        column(4,
               numericInput("ms_inhdays",label = "Days of dosing in a week",7))
      )
    ),
    tabItem(
      tabName = "iv",
      fluidRow(
        column(6,
               numericInput("ms_ivdose",label = "IV Dose (mg/h)",value = 0,step = 0.01)
               ),
        column(6,
               numericInput("ms_ivlen",label="Length of IV Dose (h)",0))
      ),
      fluidRow(
        column(6,
               awesomeCheckbox("ms_ivrep_flag","Repeat Dose Daily?",value = FALSE))
      )
    )
  )
)

#################Shiny UI
shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  shinyWidgets::useSweetAlert(),
  theme = shinythemes::shinytheme("spacelab"),
  includeCSS("www/styles.css"),
  shinyjs::extendShinyjs(text = jscode, functions = c("disableTab", "enableTab")),
  shinyjs::inlineCSS(css),
  titlePanel(title="", windowTitle = "PLETHEM rapidPBPK"),
  tags$head(
    tags$meta(charset="utf-8"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$link(rel="icon", type="image/png", href="chem_32.png", sizes="32x32")
  ),
  rintrojs::introjsUI(),
  navbarPage("", id="menu",collapsible = TRUE,
             tabPanel("",value = "Home",icon =icon("home"),
                      includeHTML("www/home.html")),
             tabPanel("Model Setup",value = "setup",
                      icon = icon("flask"),
                      fluidRow(
                        progressBar(id = "pb",value = 0, status = "success",striped = TRUE)
                      ),
                      # fluidRow(
                      #   column(4,
                      #          bsButton("run","Run Simulation", style = "primary", block = TRUE)
                      #   ),
                      #   column(2,
                      #          bsButton("btnSetupIntro", "Need help?", style = "default", size = "medium", block = TRUE)
                      #   )
                      # ),
                      tabsetPanel(id= "modelSetupTabs", type = "tabs",
                                  tabPanel("Exposure",
                                           fluidPage(
                                             bsModal("modalExpoSave",title = NULL,trigger = "btn_save_expo",

                                                     fluidRow(
                                                       column(8,offset = 2,
                                                              tags$h4("Overwrite existing Parameter Set ?")),
                                                       column(2,
                                                              bsButton("exposave_yes","Yes",block = TRUE))
                                                     )),
                                             fluidRow(
                                               column(12,
                                                      div(style = "height:15px")
                                               )
                                             ),
                                             fluidRow(
                                               column(2,
                                                      shinyBS::bsButton("btn_seem_upload",
                                                                        "Import From SEEM Data",
                                                                        block = TRUE))
                                             ),

                                             fluidRow(
                                               column(12,
                                                      div(style = "height:15px")
                                               )
                                             ),
                                             fluidRow(
                                               column(width = 9, offset = 0,
                                                      selectizeInput("sel_expo",NULL,
                                                                     choices = NULL,
                                                                     options= list(placeholder = "Exposure Parameter Set",
                                                                                   openOnFocus = TRUE))),
                                               column(width = 3, offset = 0,
                                                      shinyWidgets::actionGroupButtons(
                                                        c("btn_sverest_expo","btn_saveas_expo"),
                                                        c("Save/Restore","Save As"),
                                                        direction = "horizontal",
                                                        status = "info",
                                                        fullwidth = T

                                                      ))
                                             ),
                                             fluidRow(
                                               column(12,
                                                      div(style = "height:10px")
                                               )
                                             ),
                                             fluidRow(
                                               dashboardPage(
                                                 dashboardHeader(disable = TRUE),
                                                 expo_sidebar,
                                                 expo_body
                                               )
                                             )
                                           )
                                  ),


                                  tabPanel("Chemical",
                                           fluidPage(


                                             fluidRow(
                                               column(12,
                                                      div(style = "height:15px")
                                               )
                                             ),
                                             fluidRow(
                                               column(width = 7, offset = 0,
                                                      selectizeInput("sel_chem",NULL,
                                                                     choices = NULL,
                                                                     options= list(placeholder = "Chemicals",
                                                                                   openOnFocus = TRUE))),
                                               column(width = 5, offset = 0,
                                                      shinyWidgets::actionGroupButtons(
                                                        c("btn_import_chem"),
                                                        c("Import"),
                                                        direction = "horizontal",
                                                        status = "info",
                                                        fullwidth = T

                                                      ))
                                             ),
                                             fluidRow(
                                               column(12,
                                                      div(style = "height:10px")
                                               )
                                             ),
                                           fluidRow(
                                             dashboardPage(
                                               dashboardHeader(disable = TRUE),
                                               chem_sidebar,
                                               chem_body
                                             )
                                           ))

                                  ),


                                  tabPanel("Physiological",


                                           fluidPage(


                                             fluidRow(
                                               column(12,
                                                      div(style = "height:15px")
                                               )
                                             ),
                                             fluidRow(
                                               column(width = 9, offset = 0,
                                                      selectizeInput("sel_physio",NULL,
                                                                  choices = NULL,
                                                                  options= list(placeholder = "Physiological Parameter Set",
                                                                                openOnFocus = TRUE))),
                                               column(width = 3, offset = 0,
                                                      shinyWidgets::actionGroupButtons(
                                                        c("btn_import_physio","btn_sverest_physio","btn_saveas_physio"),
                                                        c("Import","Save/Restore","Save As"),
                                                        direction = "horizontal",
                                                        status = "info",
                                                        fullwidth = T

                                                      ))
                                             ),
                                             fluidRow(
                                               # column(2,
                                               #        shinyBS::tipify(shinyBS::bsButton("btn_use_lifecourse",
                                               #                                   "Calculate Parameters",
                                               #                                   style = "primary",block = TRUE),
                                               #                        "Use Lifecourse Equation to calcualte parameters")
                                               # ),
                                               column(4,
                                                      selectInput("ms_Org", NULL,
                                                                  #label = "Organism",
                                                                  choices = list("Human" = "Human","Rat"= "Rat"),
                                                                  selected = "Human")
                                                      # shinyBS::tipify(selectizeInput("sel_qsar4Partition", NULL,
                                                      #                choices = c("QSAR Model One" = 'one',
                                                      #                            "QSAR Model Two" = 'two')
                                                      #                ),
                                                      #                "Select QSAR model for partition coefficients")
                                                      ),
                                               column(6,
                                                      shinyBS::tipify(selectizeInput("sel_chem4Partition",NULL,
                                                                     choices ="No Chemical Added"
                                                                     ),
                                                                     "Select Chemical for qsar model")
                                                      ),
                                               column(2,
                                                      shinyBS::bsButton("btn_useQSAR4Partition",
                                                                        "Parameterize Model",
                                                                        #"Calculate Partition",
                                                                        style = "primary",
                                                                        block = TRUE)
                                                      )

                                             ),
                                             fluidRow(
                                               column(12,
                                                      div(style = "height:5px")
                                               )
                                             ),
                                             fluidRow(
                                               dashboardPage(
                                                 dashboardHeader(disable = TRUE),
                                                 comp_sidebar,
                                                 comp_body
                                               )
                                             )
                                           )


                                  ),
                                  tabPanel("Uncertanity and Variability",
                                           dashboardPage(
                                             dashboardHeader(disable = TRUE),
                                             dashboardSidebar(
                                               shinydashboard::sidebarMenu(
                                                 menuItem("Physiological",
                                                          tabName = "var_physio"),
                                                 menuItem("Chemical",
                                                          tabName = "var_chem"),
                                                 menuItem("Exposure",
                                                          tabName = "var_expo")
                                               )
                                             ),
                                             dashboardBody(tabItems(
                                               tabItem(tabName = "var_physio",
                                                         fluidRow(
                                                           column(width = 4, offset = 0,
                                                                  selectizeInput("sel_physio_var",NULL,
                                                                                 choices = NULL,
                                                                                 options= list(placeholder = "Population Parameter Set",
                                                                                               openOnFocus = TRUE))),
                                                           column(width = 8, offset = 0,
                                                                  shinyWidgets::actionGroupButtons(
                                                                    c("btn_new_varphys","btn_edit_varphys","btn_import_varphys"),
                                                                    c("New","Edit","Import"),
                                                                    direction = "horizontal",
                                                                    status = "info",
                                                                    fullwidth = TRUE)
                                                                  )
                                                           ),
                                                       fluidRow(
                                                         column(8, offset = 2,
                                                                DT::DTOutput("physio_var_tble")
                                                         )
                                                       )
                                                       ),
                                               tabItem(tabName = "var_chem",
                                                       fluidRow(
                                                         column(width = 4, offset = 0,
                                                                selectizeInput("sel_chem_var",NULL,
                                                                               choices = NULL,
                                                                               options= list(placeholder = "Metabolism Parameter Set",
                                                                                             openOnFocus = TRUE))),
                                                         column(width = 8, offset = 0,
                                                                shinyWidgets::actionGroupButtons(
                                                                  c("btn_new_varchem","btn_edit_varchem","btn_import_varchem"),
                                                                  c("New","Edit","Import"),
                                                                  direction = "horizontal",
                                                                  status = "info",
                                                                  fullwidth = TRUE)
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(8, offset = 2,
                                                                tableOutput("chem_var_tble"))
                                                       )
                                                       ),
                                               tabItem(tabName = "var_expo",
                                                       fluidRow(
                                                         column(width = 4, offset = 0,
                                                                selectizeInput("sel_expo_var",NULL,
                                                                               choices = NULL,
                                                                               options= list(placeholder = "Exposure Parameter Set",
                                                                                             openOnFocus = TRUE))),
                                                         column(width = 8, offset = 0,
                                                                shinyWidgets::actionGroupButtons(
                                                                  c("btn_new_varexpo","btn_edit_varexpo","btn_import_varexpo"),
                                                                  c("New","Edit","Import"),
                                                                  direction = "horizontal",
                                                                  status = "info",
                                                                  fullwidth = TRUE)
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(8, offset = 2,
                                                                tableOutput("expo_var_tble"))
                                                       )
                                               )
                                               )
                                               )
                                           )

                                           ),
                                  # tabPanel("Metabolism",
                                  #
                                  #          fluidPage(
                                  #            fluidRow(
                                  #              column(12,
                                  #                     div(style = "height:10px")
                                  #              )
                                  #            ),
                                  #            fluidRow(
                                  #
                                  #              column(2,
                                  #                     bsButton("btn_metab_upload",
                                  #                              "Upload Metabolism Files",
                                  #
                                  #                              block = TRUE)
                                  #                     )
                                  #            ),
                                  #            fluidRow(
                                  #              column(12,
                                  #                     div(style = "height:10px")
                                  #              )
                                  #            ),
                                  #            fluidRow(
                                  #              column(width = 7, offset = 0,
                                  #                     selectizeInput("sel_metab",NULL,
                                  #                                    choices = NULL,
                                  #                                    options= list(placeholder = "Metabolism Parameter Set",
                                  #                                                  openOnFocus = TRUE))),
                                  #              column(width = 5, offset = 0,
                                  #                     shinyWidgets::actionGroupButtons(
                                  #                       c("btn_sve_metab","btn_saveas_metab"),
                                  #                       c("Save","Save As"),
                                  #                       direction = "horizontal",
                                  #                       status = "info",
                                  #                       fullwidth = T
                                  #
                                  #                     ))
                                  #            ),
                                  #            fluidRow(
                                  #              column(12,
                                  #                     div(style = "height:10px")
                                  #              )
                                  #            ),
                                  #            # fluidRow(
                                  #            #   column(width = 3,
                                  #            #          fileInput("metab_csv","Upload Metabolism Data")),
                                  #            #   column(width = 3,
                                  #            #          downloadLink("metab_template","Template for metabolism file"))
                                  #            #   ),
                                  #            # fluidRow(
                                  #            #   column(width = 4,
                                  #            #          textInput("metab_set_name","Name",
                                  #            #                    placeholder = "Enter the name for this metabolism set")),
                                  #            #   column(width = 8,
                                  #            #          textAreaInput("metab_set_descrp","Description",
                                  #            #                        resize = "none" ,row = 1))
                                  #            #
                                  #            # ),
                                  #            # fluidRow(column(width = 6,
                                  #            #                 shinyWidgets::radioGroupButtons("metab_type",justified = TRUE,
                                  #            #                                                 "Select Meatbolism Type",
                                  #            #                                                 choices = c("VmaxC"="m1","VlivC"="m2"))
                                  #            #                 )
                                  #            #
                                  #            #
                                  #            #
                                  #            #  ),
                                  #            # fluidRow(column(width = 4,
                                  #            #                 shinyWidgets::awesomeCheckbox("use_ref",
                                  #            #                                               "Use clearance at reference age for ages not in the metabolism table",
                                  #            #                                               value = TRUE)
                                  #            #                 ),
                                  #            #          column(width = 4,
                                  #            #                 numericInput("metab_ref_age","Referance age in Years",value = 25, min = 0))
                                  #            # ),
                                  #            fluidRow(column(width = 6, offset = 3,
                                  #                            DT::DTOutput("metab_tble")))
                                  #
                                  #
                                  #          )
                                  #
                                  #         ),
                                  tabPanel("Simulations",
                                           fluidPage(
                                             # fluidRow(tags$h4("")),
                                             # fluidRow(
                                             #
                                             #   column(1,
                                             #          shinyWidgets::actionBttn("add_row",NULL,
                                             #                                   icon = icon("plus"),style = "material-circle",
                                             #                                   block = TRUE)
                                             #   ),
                                             #   column(1,
                                             #          shinyWidgets::actionBttn("rem_row",NULL,
                                             #                                   icon = icon("remove"),style = "material-circle",
                                             #                                   block = TRUE)
                                             #   ),
                                             #   column(10,
                                             #          actionButton("run_row","Run Selected Row",width = validateCssUnit("100%")))
                                             # ),
                                             fluidRow(tags$h4("")),
                                             fluidRow(
                                               column(5,
                                                      shinyWidgets::dropdownButton(
                                                        tagList(
                                                          textInput("sim_name","Name"),
                                                          textAreaInput("sim_descrp","Description",rows = 2,resize = "none"),
                                                          fluidRow(
                                                            column(6,
                                                                   selectizeInput("sel_set_chem","Select Chemical",choices = NULL)
                                                                   ),
                                                            column(6,
                                                                   selectizeInput("sel_set_chemvar","Select Variability",choices = NULL)
                                                                   )
                                                          ),
                                                          fluidRow(
                                                            column(6,
                                                                   selectizeInput("sel_set_expo","Select Exposure",choices = NULL)
                                                            ),
                                                            column(6,
                                                                   selectizeInput("sel_set_expovar","Select Variability",choices = NULL)
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(6,
                                                                   selectizeInput("sel_set_physio","Select Compartment",choices = NULL)
                                                            ),
                                                            column(6,
                                                                   selectizeInput("sel_set_physiovar","Select Variability",choices = NULL)
                                                            )
                                                          ),
                                                          selectizeInput("sel_set_metab","Select Metabolism",choice = NULL),
                                                          fluidRow(
                                                            column(6,
                                                                   numericInput("sim_dur","Simulation Duration (days)",0)
                                                                   ),
                                                            column(6,
                                                                   numericInput("mc_num","Number of Monte Carlo Runs",1000)
                                                                   )
                                                          ),
                                                          checkboxInput("mc_mode","Run Monte Carlo Simulation",T),
                                                          shinyWidgets::actionBttn("save_sim",NULL,
                                                                                   icon = icon("floppy-save",lib = "glyphicon"),
                                                                                   style = "material-circle")
                                                        ),
                                                        icon = icon("plus"),circle = FALSE,
                                                        tooltip = FALSE,size = "default",right = FALSE,
                                                        width = validateCssUnit("100%"),
                                                        label = "Create new simulation",
                                                        status = "default" )),
                                               column(3,
                                                      shinyBS::bsButton("run_sim","Run Simulation",
                                                                               style = "primary",block = TRUE))
                                             ),
                                             fluidRow(
                                               br(),
                                               selectizeInput("sel_sim", "Select a Simulation",
                                                              choices =NULL,  width = validateCssUnit("99%"))
                                               # bsCollapse(id = "sim_selector",multiple = TRUE, open ="Simulations",
                                               #            # bsCollapsePanel("Chemicals",style = "primary",
                                               #            #                 selectizeInput("sim_chem_filter", "Select a chemical",
                                               #            #                                choices = NULL,
                                               #            #                                width = validateCssUnit("99%"))
                                               #            # ),
                                               #            # bsCollapsePanel("Exposure",style = "primary",
                                               #            #                 radioButtons("sim_expo_type",label = NULL,
                                               #            #                              choices = c("Oral"="oral",
                                               #            #                                          "Drinking Water"="dw",
                                               #            #                                          "Inhalation"="inh",
                                               #            #                                          "Intravenous"="derm"),
                                               #            #                              selected = 'oral',
                                               #            #                              inline = FALSE,
                                               #            #                              width = validateCssUnit("100%"))
                                               #            # ),
                                               #            bsCollapsePanel("Simulations",style = "primary",
                                               #                            selectizeInput("sel_sim", "Select a Simulation",
                                               #                                           choices =NULL,  width = validateCssUnit("99%"))
                                               #            )
                                               # )
                                             ),
                                             fluidRow(
                                               wellPanel(id= "Simulationdetails",
                                                         fluidRow(
                                                           column(8,offset =2,
                                                                  tags$h4("Simulation Description"),
                                                                  textOutput("sim_descrp")
                                                           )

                                                         ),
                                                         fluidRow(
                                                           column(6,
                                                                  tags$h4("Simulation Start (h)"),
                                                                  textOutput("sim_start")
                                                           ),
                                                           column(6,
                                                                  tags$h4("Simulation Duration (days)"),
                                                                  textOutput("sim_dur")
                                                                  )
                                                         ),
                                                         fluidRow(
                                                           column(6,
                                                                  tags$h4("Chemical Name"),
                                                                  textOutput("sim_chem")
                                                                  ),
                                                           column(4,
                                                                tags$h4("Exposure Name"),
                                                                textOutput("sim_expo")
                                                                )
                                                         ),
                                                         fluidRow(
                                                           column(4,
                                                                  tags$h4("Age"),
                                                                  textOutput("sim_age")
                                                           ),
                                                           column(4,
                                                                  tags$h4("Gender"),
                                                                  textOutput("sim_gender")
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(4,
                                                                  tags$h4("Metabolism Type"),
                                                                  textOutput("sim_metab_type")
                                                                  ),
                                                           column(4,
                                                                  tags$h4("Value"),
                                                                  textOutput("sim_metab_val")
                                                                  ),
                                                           column(4,
                                                                  tags$h4("Units"),
                                                                  textOutput("sim_metab_units")
                                                                  )

                                                         )

                                             )
                                             )


                      ))
             )),
             tabPanel("Model Output",icon = icon("line-chart"),value = "output",
                      fluidRow(
                        column(2,
                               bsButton("btnAddData","Add Dataset",block = TRUE,style = "primary")
                               ),
                        column(3, offset= 6,
                               downloadButton("downloadModel", "Download Model",class = "btn btn-primary btn-block")
                               ),
                        column(1,
                               shinyBS::bsButton("btnOutputIntro", "Need help?",block = TRUE,style = "info"))
                      ),
                      tabsetPanel(id = "Modeloutput", type = "tabs",
                                  tabPanel("Plots",
                                           plot_body
                                  ),
                                  tabPanel("Parameters",
                                           fluidPage(
                                             fluidRow(
                                                      box(title = "Exposure Parameters",width = 4,
                                                          DT::DTOutput("expo_params_tble")),
                                                      box(title = "Chemical Parameters",width = 4,
                                                          DT::DTOutput("pamamstbl")),
                                                      box(title = "Physiological Parameters",width = 4,
                                                          DT::DTOutput("physio_params_tble"))


                                               # tags$h3("CURRENT PARAMETERS", class="text-center page-header"),
                                               # column(10, offset = 1, id="paramsCurrentValues",
                                               #        dataTableOutput("pamamstbl")
                                               # )
                                             ),
                                             fluidRow(
                                               downloadButton("btn_param_dwnld",
                                                              label = "Download All Paramters")
                                             ))
                                  )

                      )

             ),
             tabPanel( id = "help" , title= "help", value = "Help", icon = icon("info")),
             tabPanel(title = "",value = "Stop",icon=icon("power-off"))
  )
))

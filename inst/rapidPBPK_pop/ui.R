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
  menuItem("Drinking Water", tabName = "dw"),
  menuItem("Inhalation", tabName = "inh"),
  menuItem("Intravenous", tabName = "iv"),
  menuItem("Dermal",tabName = "dermal")
))

################################compartment sidebar
comp_sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(id="compsidebar",
              # tags$div(shinyWidgets::radioGroupButtons("ms_org",
              #                                          label = "Select Organism",
              #                                          choices = c("Human" = "ha","Rat"="ra"),
              #                                          selected = "ha",size = "normal",
              #                                          checkIcon = list(yes = icon('ok',lib ="glyphicon")))),
              menuItem(tags$h5(tags$span(style = "color:white",
                                         "Physiological Parameters")),
                       tabName = "physiolocal_parameters", selected = TRUE),
              tags$div(class = "comp_checkboxes",
                       tags$h4(tags$span(style = "color:white","Model Compartments")),
                       checkboxGroupInput("ms_cmplist",NULL,
                                          selected = c("fat","skin","muscle",
                                                       "bone","brain",
                                                       "lung","heart","gi",
                                                       "liver","kidney","rpf","spf"),
                                          choiceNames = list(tags$div(id = "fat",class = "comp_tabs",
                                                                      menuItem("Fat",tabName = "fat",
                                                                               icon = icon("plus",class = "pull-right"),
                                                                               menuSubItem("Tissue",
                                                                                           tabName = "fat_tissue"),
                                                                               menuSubItem("Compartment",
                                                                                           tabName = "fat_blood")
                                                                      )
                                          ),
                                          tags$div(id = "skin",class = "comp_tabs",
                                                   menuItem("Skin",icon = icon("plus",class = "pull-right"),
                                                            menuSubItem("Tissue",
                                                                        tabName = "skin_tissue"),
                                                            menuSubItem("Compartment",
                                                                        tabName = "skin_blood")
                                                   )
                                          ),
                                          tags$div(id = "muscle",class = "comp_tabs",
                                                   menuItem("Muscle",icon = icon("plus",class = "pull-right"),
                                                            menuSubItem("Tissue",
                                                                        tabName = "muscle_tissue"),
                                                            menuSubItem("Compartment",
                                                                        tabName = "muscle_blood")
                                                   )
                                          ),
                                          tags$div(id = "bone",class = "comp_tabs",
                                                   menuItem("Bone",icon = icon("plus",class = "pull-right"),
                                                            menuSubItem("Tissue",
                                                                        tabName = "bone_tissue"),
                                                            menuSubItem("Compartment",
                                                                        tabName = "bone_blood")
                                                   )
                                          ),
                                          tags$div(id = "brain",class = "comp_tabs",
                                                   menuItem("Brain",icon = icon("plus",class = "pull-right"),
                                                            menuSubItem("Tissue",
                                                                        tabName = "brain_tissue"),
                                                            menuSubItem("Compartment",
                                                                        tabName = "brain_blood")
                                                   )
                                          ),
                                          tags$div(id = "lung",class = "comp_tabs",
                                                   menuItem("Lungs",icon = icon("plus",class = "pull-right"),
                                                            menuSubItem("Tissue",
                                                                        tabName = "lung_tissue"),
                                                            menuSubItem("Compartment",
                                                                        tabName = "lung_blood")
                                                   )
                                          ),
                                          tags$div(id = "heart",class = "comp_tabs",
                                                   menuItem("Heart",icon = icon("plus",class = "pull-right"),
                                                            menuSubItem("Tissue",
                                                                        tabName = "heart_tissue"),
                                                            menuSubItem("Compartment",
                                                                        tabName = "heart_blood")
                                                   )
                                          ),
                                          tags$div(id = "gi",class = "comp_tabs",
                                                   menuItem("GI",icon = icon("plus",class = "pull-right"),
                                                            menuSubItem("Tissue",
                                                                        tabName = "gi_tissue"),
                                                            menuSubItem("Compartment",
                                                                        tabName = "gi_blood")
                                                   )
                                          ),
                                          tags$div(id = "liver",class = "comp_tabs",
                                                   menuItem("Liver",icon = icon("plus",class = "pull-right"),
                                                            menuSubItem("Tissue",
                                                                        tabName = "liver_tissue"),
                                                            menuSubItem("Compartment",
                                                                        tabName = "liver_blood")
                                                   )
                                          ),
                                          tags$div(id = "kidney",class = "comp_tabs",
                                                   menuItem("Kidney",icon = icon("plus",class = "pull-right"),
                                                            menuSubItem("Tissue",
                                                                        tabName = "kidney_tissue"),
                                                            menuSubItem("Compartment",
                                                                        tabName = "kidney_blood")
                                                   )
                                          ),
                                          tags$div(id = "rpf",class = "comp_tabs",
                                                   menuItem("Rapidly Perfused",icon = icon("plus",class = "pull-right"),
                                                            menuSubItem("Tissue",
                                                                        tabName = "rpf_tissue"),
                                                            menuSubItem("Compartment",
                                                                        tabName = "rpf_blood")
                                                   )
                                          ),
                                          tags$div(id = "spf",class = "comp_tabs",
                                                   menuItem("Slowly Perfused",icon = icon("plus",class = "pull-right"),
                                                            menuSubItem("Tissue",
                                                                        tabName = "spf_tissue"),
                                                            menuSubItem("Compartment",
                                                                        tabName = "spf_blood")
                                                   )
                                          )
                                          ),
                                          choiceValues = list("fat","skin","muscle",
                                                              "bone","brain",
                                                              "lung","heart","gi",
                                                              "liver","kidney","rpf","spf")
                                          )
                       )
              )

  )


###############################Chemical Sidebar
chem_sidebar <- shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
  id="chem_sidebar",
  selectizeInput("qsarModelChem", "Select A QSAR Model",
                          choices = c("QSAR model one" = 'one',
                                      "QSAR model two" = 'two'),
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
                       bsCollapsePanel(title = "Exposure Plots", style = "primary",
                                     column(3,
                                        wellPanel(
                                                   checkboxInput("ch_dose", "Instaneous Dose", value = TRUE, width = NULL),
                                                   checkboxInput("ch_totdose", "Total Dose", value = TRUE, width = NULL)
                                        )
                       ),
                       column(9,
                              tabBox(width=12, height = validateCssUnit("100%"),
                                     tabPanel("Plot",
                                              fluidRow(
                                                column(4,offset = 4,
                                                       radioButtons("r_expo_type",label = "Select Exposure",inline = TRUE,
                                                                    choices = c("Active"="act","All"="all"),
                                                                    selected = "act"))
                                              ),
                                              fluidRow(
                                                plotly::plotlyOutput("exposureplt")
                                              )
                                     ),
                                     tabPanel("Table",
                                              DT::DTOutput("expotble"),
                                              downloadButton("expodwnld",label = "Get Data")))
                       )),
                       bsCollapsePanel(title = "Concentration Plots", style = "primary",
                                 column(3,
                                        tabsetPanel(
                                          tabPanel("Model",value  = "model",
                                                   shinyWidgets::multiInput("cplt_comp",label = tags$h4("Select Compartment"),
                                                              choices = list("Arterial Plasma"="art_bld","Venous Plasma"="ven_bld",
                                                                             "Fat Total"="to_fat","Fat Tissue"="ti_fat","Fat Exchange"="bl_fat",
                                                                             "Skin Total"="to_skn","Skin Tissue"="ti_skn","Skin Exchange"="bl_skn",
                                                                             "Bone Total"="to_bne","Bone Tissue"="ti_bne","Bone Exchange"="bl_bne",
                                                                             "Muscle Total"="to_musc","Muscle Tissue"="ti_musc","Muscle Exchange"="bl_musc",
                                                                             "Brain Total"="to_brn","Brain Tissue"="ti_brn","Brain Exchange"="bl_brn",
                                                                             "Lungs Total"="to_lng","Lungs Tissue"="ti_lng","Lungs Exchange"="bl_lng",
                                                                             "Heart Total"="to_hrt","Heart Tissue"="ti_hrt","Heart Exchange"="bl_hrt",
                                                                             "GI Total"="to_gi","GI Tissue"="ti_gi","GI Exchange"="bl_gi",
                                                                             "Liver Total"="to_liv","Liver Tissue"="ti_liv","Liver Exchange"="bl_liv",
                                                                             "Kidney Total"="to_kdn","Kidney Tissue"="ti_kdn","Kidney Exchange"="bl_kdn",
                                                                             "Rapidly Perused Total"="to_rpf",
                                                                             "Rapidly Perused Tissue"="ti_rpf","Rapidly Perused Exchange"="bl_rpf",
                                                                             "Slowly Perused Total"="to_spf",
                                                                             "Slowly Perused Tissue"="ti_spf","Slowly Perused Exchange"="bl_spf"
                                                              )
                                                   )
                                                   ),
                                          tabPanel("Dataset",value = "dataset",
                                                   shinyWidgets::pickerInput("cplt_data",multiple = F,
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
                                         ))),
                       bsCollapsePanel(title = "Amount Plots", style = "primary",
                                       column(3,
                                              wellPanel(
                                                multiInput("aplt_comp",label = tags$h4("Select Compartment"),
                                                               choices = list("Arterial Plasma"="art_bld",
                                                                              "Fat Total"="to_fat","Fat Tissue"="ti_fat","Fat Exchange"="bl_fat",
                                                                              "Skin Total"="to_skn","Skin Tissue"="ti_skn","Skin Exchange"="bl_skn",
                                                                              "Bone Total"="to_bne","Bone Tissue"="ti_bne","Bone Exchange"="bl_bne",
                                                                              "Muscle Total"="to_musc","Muscle Tissue"="ti_musc","Muscle Exchange"="bl_musc",
                                                                              "Brain Total"="to_brn","Brain Tissue"="ti_brn","Brain Exchange"="bl_brn",
                                                                              "Lungs Total"="to_lng","Lungs Tissue"="ti_lng","Lungs Exchange"="bl_lng",
                                                                              "Heart Total"="to_hrt","Heart Tissue"="ti_hrt","Heart Exchange"="bl_hrt",
                                                                              "GI Total"="to_gi","GI Tissue"="ti_gi","GI Exchange"="bl_gi",
                                                                              "Liver Total"="to_liv","Liver Tissue"="ti_liv","Liver Exchange"="bl_liv",
                                                                              "Kidney Total"="to_kdn","Kidney Tissue"="ti_kdn","Kidney Exchange"="bl_kdn",
                                                                              "Rapidly Perused Total"="to_rpf",
                                                                                                  "Rapidly Perused Tissue"="ti_rpf","Rapidly Perused Exchange"="bl_rpf",
                                                                              "Slowly Perused Total"="to_spf",
                                                                                                 "Slowly Perused Tissue"="ti_spf","Slowly Perused Exchange"="bl_spf"
                                                                              )

                                                           ),
                                                selectizeInput("aplt_data",tags$h4("Select Data Sets"),choices = c("a","b","c"))

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
                                       ),
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
                       bsCollapsePanel(title = "Mass Balance Plots", style = "primary",
                                       column(8, offset = 2,
                                              tabBox(width = 12, height = validateCssUnit("100%"),
                                                     tabPanel("Plot",
                                                              fluidRow(
                                                                plotOutput("balplt")
                                                              )
                                                     ),
                                                     tabPanel("Table",
                                                              DT::DTOutput("baltble"),
                                                              downloadButton("cmbaldwnld",label ="Get Data"))
                                              ))
                                       )
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
        column(4,
               selectInput("ms_org", label = "Organism",
                           choices = list("Human" = "ha","Rat"= "ra"),
                           selected = "ha")
               ),
        column(4,
               selectInput("ms_gender", label = "Gender",
                           choices = list("Male" = "M", "Female" = "F"),
                           selected = "M")
        ),
        column(4,
               numericInput("ms_age","Age",25,0.5,80,1)
        )
      ),

      fluidRow(
        column(4,
               numericInput("ms_bw","Body Weight (kg)",70,0.1,90,0.1)),
        column(4,
               numericInput("ms_qcc","Cardiac Output (L/h)",420,1,50,0.1)),
        column(4,
               numericInput("ms_hct","Hematocrit Factor",0.441,0,1,0.001))

      ),
      fluidRow(
        column(4,
               numericInput("ms_vbldc","Fractional Blood Compartment Volume",0.0832,0.01,1,0.01)),
        column(4,
               numericInput("ms_perfc","Total Fractional Perfused Tissue",1,0.75,0.95,0.01)
               ),
        column(4,
               numericInput("ms_kbld","First Order Metabolism in Blood",0,0.75,0.95,0.01))
      ),
      fluidRow(
        column(4,
               numericInput("ms_respr","Respiration Rate (L/h)",min =0 , max =10, value =490)),
        column(4,
               numericInput("ms_tv","Tidal Volume (L)",min =0 , max =10, value =0.623)),
        column(4,
               numericInput("ms_ds","Dead Space (L)",min =0 , max =10, value =0.154))
      ),
      fluidRow(
        column(4,
               numericInput("ms_uflw","Urinary Flow Rate (L/kg/day)",min =0 , max =1, value =0.0214)),
        column(4,
               numericInput("ms_gfr","Glomerular Filteration (L/h)",min =0 , max =1, value =0.08)
               ),
          column(4,
                 numericInput("ms_pair", label = "Plasma-Air Partition Coefficient", value = 1,
                              min = 0)
          )
        
      ),
      fluidRow(
        column(6,
               numericInput("ms_fa","Fraction Absorbed in the Gut Lumen",1)),
        column(6,
               numericInput("ms_ka",label = "Rate of Absorption in Gut Lumen (per hour)", value = 5, step = 0.01))
      )
    ),

    tabItem(
      tabName = "fat_tissue",
      fluidRow(
        column(6,
               numericInput("ms_pafat","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_fatvtbc","Fat Tissue to Total Fat Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "fat_blood",
      fluidRow(
        column(6,
               numericInput("ms_vfatc","Volume Ratio",min =0, max = 1, value =0.1841, step = 0.01)),
        column(6,
               numericInput("ms_qfatc","Blood Flow Ratio",min =0 , max =1, value =0.08226))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_pfat","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "skin_tissue",
      fluidRow(
        column(6,
               numericInput("ms_paskin","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_skinvtbc","Skin Tissue to Total Skin Volume Ratio",0.95,0,1,0.01))
      ),
      fluidRow(
        column(4, 
               numericInput("ms_KPtot","Total Stratum Corneum permeation coefficient",1000,1,1000,1)),
        column(4,
               numericInput("ms_Kevap","Evaporation rate from Stratum Corneum",1000,1,1000,1)),
        column(4,
               numericInput("ms_maxcap","Maximum capacity of Stratum Corneum",1000,1,1000,1))
        
      )
    ),
    tabItem(
      tabName = "skin_blood",
      fluidRow(
        column(6,
               numericInput("ms_vskinc","Volume Ratio",min =0, max = 1, value =0.0553, step = 0.01)),
        column(6,
               numericInput("ms_qskinc","Blood Flow Ratio",min =0 , max =1, value =0.06783))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_pskin","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "muscle_tissue",
      fluidRow(
        column(6,
               numericInput("ms_pamusc","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_muscvtbc","Muscle Tissue to Total Muscle Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "muscle_blood",
      fluidRow(
        column(6,
               numericInput("ms_vmuscc","Volume Ratio",min =0, max = 1, value =0.4576, step = 0.01)),
        column(6,
               numericInput("ms_qmuscc","Blood Flow Ratio",min =0 , max =1, value =0.13711))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_pmusc","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "bone_tissue",
      fluidRow(
        column(6,
               numericInput("ms_pabone","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_bonevtbc","Bone Tissue to Total Bone Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "bone_blood",
      fluidRow(
        column(6,
               numericInput("ms_vbonec","Volume Ratio",min =0, max = 1, value =0.1318, step = 0.01)),
        column(6,
               numericInput("ms_qbonec","Blood Flow Ratio",min =0 , max =1, value =0.1266))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_pbone","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "brain_tissue",
      fluidRow(
        column(6,
               numericInput("ms_pabrn","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_brnvtbc","Brain Tissue to Total Brain Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "brain_blood",
      fluidRow(
        column(6,
               numericInput("ms_vbrnc","Volume Ratio",min =0, max = 1, value =0.0192, step = 0.01)),
        column(6,
               numericInput("ms_qbrnc","Blood Flow Ratio",min =0 , max =1, value =0.098))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_pbrn","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "lung_tissue",
      fluidRow(
        column(6,
               numericInput("ms_palng","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_lngvtbc","Lung Tissue to Total Lung Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "lung_blood",
      fluidRow(
        column(6,
               numericInput("ms_vlngc","Volume Ratio",min =0, max = 1, value =0.0144, step = 0.01)),
        column(6,
               numericInput("ms_qlngc","Blood Flow Ratio",min =0 , max =1, value =0.0234))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_plng","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "heart_tissue",
      fluidRow(
        column(6,
               numericInput("ms_pahrt","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_hrtvtbc","Heart Tissue to Total Heart Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "heart_blood",
      fluidRow(
        column(6,
               numericInput("ms_vhrtc","Volume Ratio",min =0, max = 1, value =0.0051, step = 0.01)),
        column(6,
               numericInput("ms_qhrtc","Blood Flow Ratio",min =0 , max =1, value =0.10536))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_phrt","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "gi_tissue",
      fluidRow(
        column(6,
               numericInput("ms_pagi","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_givtbc","GI Tissue to Total GI Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "gi_blood",
      fluidRow(
        column(6,
               numericInput("ms_vgic","Volume Ratio",min =0, max = 1, value =0.0222, step = 0.01)),
        column(6,
               numericInput("ms_qgic","Blood Flow Ratio",min =0 , max =1, value =0.1139))
      ),

      fluidRow(class="",
               column(6,
                      numericInput("ms_pgi","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "liver_tissue",
      fluidRow(
        column(6,
               numericInput("ms_paliv","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_livvtbc","Liver Tissue to Total Liver Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "liver_blood",
      fluidRow(
        column(6,
               numericInput("ms_vlivc","Volume Ratio",min =0, max = 1, value =0.0225, step = 0.01))
      ),
      fluidRow(
        column(6,
               numericInput("ms_qalivc","Aretrial Blood Flow ratio to liver",min =0 , max =1, value =0.0381)),
        column(6,
               numericInput("ms_qvlivc","Fraction of Venous Liver Flow",min =0 , max =1, value =0.152))

      ),

      fluidRow(class="",
               column(6,
                      numericInput("ms_pliv","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "kidney_tissue",
      fluidRow(
        column(6,
               numericInput("ms_pakdn","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_kdnvtbc","Kidney Tissue to Total Kidney Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "kidney_blood",
      fluidRow(
        column(6,
               numericInput("ms_vkdnc","Volume Ratio",min =0, max = 1, value =0.0046, step = 0.01)),
        column(6,
               numericInput("ms_qkdnc","Blood Flow Ratio",min =0 , max =1, value =0.16886))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_pkdn","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "rpf_tissue",
      fluidRow(
        column(6,
               numericInput("ms_parpf","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_rpfvtbc","Rapidly Perfused Tissue to All Rapidly Perfused Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "rpf_blood",
      fluidRow(
        column(6,
               numericInput("ms_vrpfc","Volume Ratio",min =0, max = 1, value =0.00001, step = 0.01)),
        column(6,
               numericInput("ms_qrpfc","Blood Flow Ratio",min =0 , max =1, value =0))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_prpf","Partition Coefficient",0.5,0,2, 0.01)))
    ),
    tabItem(
      tabName = "spf_tissue",
      fluidRow(
        column(6,
               numericInput("ms_paspf","Pearmeability Area Coefficient",1000,1,1000,1)),
        column(6,
               numericInput("ms_spfvtbc","Slowly Perfused Tissue to All Slowly Perfused Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "spf_blood",
      fluidRow(
        column(6,
               numericInput("ms_vspfc","Volume Ratio",min =0, max = 1, value =0.00001, step = 0.01)),
        column(6,
               numericInput("ms_qspfc","Blood Flow Ratio",min =0 , max =1, value =0))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_pspf","Partition Coefficient",0.5,0,2, 0.01)))
    )
  )
)

####################chemical Body
chem_body <- dashboardBody(
  fluidPage(id="Chemicals",
            tags$h4("Chemical Parameters", class="pager-header"),
            fluidRow(
              column(6,
                     numericInput("ms_den","Density (g/L)",1,0,1500,1)),
              column(6,
                     numericInput("ms_mw","Molecular Weight (g/mol)",1,0,250,0.01))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_vpa","Vapor Pressure (Pa)",1,0,250,0.01)),
              column(6,
                     numericInput("ms_dkow","logKow in skin at pH5.5",1,0,250,0.01))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_lkow","logKow (Octanol:Water Coefficient)",1,0,250,0.01)),
              column(6,
                     numericInput("ms_wsol","Water Solubility (mg/L)",1,0,250,0.01))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_res","Fraction Resorped in Kidney",1,0,0,0.01)),
              column(6,
                     numericInput("ms_fupls", label = "Fraction Unbound in Palsma", value = 1, 0, 1, 0.001))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_vmaxc",paste0("Maximum Metabolism Rate (","μm/h/kg BW^0.75)"),1,0,250,0.01)),
              column(6,
                     numericInput("ms_km","Michaelis Menton Constant for Metabolism (μM)",1,0,250,0.01))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_vkm1c", label = "First Order metabolism in Liver (L/h/kg liver)", value = 1, step = 0.01)
                     ),
              column(6,
                     numericInput("ms_frwsol", label = "Fraction dissolved in water", value = 1,
                                  min=0,max = 1,step = 0.01)
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
               awesomeCheckbox("ms_brep_flag","Repeat Dose Daily?",value = F))
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
               awesomeCheckbox("ms_ivrep_flag","Repeat Dose Daily?",value = F))
      )
    ),
    tabItem(
      tabName = "dermal",
      fluidRow(
        column(6,
               numericInput("ms_dermrate","Dermal deposition rate (mg/cm2/h)",0,step = 0.1)),
        column(6,
               numericInput("ms_skarea","Skin Area (cm2)",0,step = 0.1))
        ),
      fluidRow(
        column(6, 
               numericInput("ms_dermlen","Length of dermal dosing (h)",0.1,step=0.01)),
        column(6,
               awesomeCheckbox("ms_dermrep_flag","Repeat Dose Daily?",value = F))
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
  shinyjs::extendShinyjs(text = jscode),
  shinyjs::inlineCSS(css),
  titlePanel(title="", windowTitle = "PLETHEM rapidPBPK"),
  tags$head(
    tags$meta(charset="utf-8"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$link(rel="icon", type="image/png", href="chem_32.png", sizes="32x32")
  ),
  #rintrojs::introjsUI(),
  navbarPage("", id="menu",collapsible = TRUE,
             tabPanel("",value = "Home",icon =icon("home"),
                      includeHTML("www/home.html")),
             tabPanel("Model Setup",value = "setup",
                      icon = icon("flask"),
                      fluidRow(
                        progressBar(id = "pb",value = 0, status = "success",striped = T)
                      ),
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
                                             # fluidRow(
                                             #   column(2,
                                             #          shinyBS::bsButton("btn_seem_upload",
                                             #                            "Import From SEEM Data",
                                             #                            block = T)),
                                             #   column(2,
                                             #          shinyBS::bsButton("btn_sheds_upload",
                                             #                            "Import SHEDS-HT results",
                                             #                            block = T)),
                                             #   column(2,
                                             #          shinyBS::bsButton("btn_batch_upload",
                                             #                            "Import batch exposure file",
                                             #                            block = T))
                                             # ),

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
                                                                                   openOnFocus = T))),
                                               column(width = 3, offset = 0,
                                                      shinyWidgets::actionGroupButtons(
                                                        c("btn_import_expo","btn_sverest_expo","btn_saveas_expo"),
                                                        c("Import Data","Save/Restore","Save As"),
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
                                                                                   openOnFocus = T))),
                                               column(width = 5, offset = 0,
                                                      shinyWidgets::actionGroupButtons(
                                                        c("btn_import_chem","btn_sverest_chem","btn_saveas_chem"),
                                                        c("Import","Save/Restore","Save As"),
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
                                                                                openOnFocus = T))),
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
                                               column(2,
                                                      shinyBS::tipify(shinyBS::bsButton("btn_use_lifecourse",
                                                                                 "Calculate Parameters",
                                                                                 style = "primary",block = TRUE),
                                                                      "Use Lifecourse Equation to calcualte parameters")
                                               ),
                                               column(4,
                                                      shinyBS::tipify(selectizeInput("sel_qsar4Partition", NULL,
                                                                     choices = c("QSAR model one" = 'one',
                                                                                 "QSAR model two" = 'two')
                                                                     ),
                                                                     "Select QSAR model for partition coefficients")
                                                      ),
                                               column(4,
                                                      shinyBS::tipify(selectizeInput("sel_chem4Partition",NULL,
                                                                     choices ="No Chemical Added"
                                                                     ),
                                                                     "Select Chemical for qsar model")
                                                      ),
                                               column(2,
                                                      shinyBS::bsButton("btn_useQSAR4Partition",
                                                                        "Calculate Partition",style = "primary",
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
                                             dashboardHeader(disable = T),
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
                                                                                               openOnFocus = T))),
                                                           column(width = 8, offset = 0,
                                                                  shinyWidgets::actionGroupButtons(
                                                                    c("btn_new_varphys","btn_edit_varphys","btn_import_varphys"),
                                                                    c("New","Edit","Import"),
                                                                    direction = "horizontal",
                                                                    status = "info",
                                                                    fullwidth = T)
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
                                                                                             openOnFocus = T))),
                                                         column(width = 8, offset = 0,
                                                                shinyWidgets::actionGroupButtons(
                                                                  c("btn_new_varchem","btn_edit_varchem","btn_import_varchem"),
                                                                  c("New","Edit","Import"),
                                                                  direction = "horizontal",
                                                                  status = "info",
                                                                  fullwidth = T)
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
                                                                                             openOnFocus = T))),
                                                         column(width = 8, offset = 0,
                                                                shinyWidgets::actionGroupButtons(
                                                                  c("btn_new_varexpo","btn_edit_varexpo","btn_import_varexpo"),
                                                                  c("New","Edit","Import"),
                                                                  direction = "horizontal",
                                                                  status = "info",
                                                                  fullwidth = T)
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
                                           # fluidPage(
                                           #   
                                           #   fluidRow(
                                           #     column(12,
                                           #            div(style = "height:15px")
                                           #     )
                                           #   ),
                                           #   fluidRow(tags$h5("Physiology")),
                                           #   fluidRow(
                                           #     column(width = 9, offset = 0,
                                           #            selectizeInput("sel_mc_physio",NULL,
                                           #                           choices = NULL,
                                           #                           options= list(placeholder = "Population Parameter Set",
                                           #                                         openOnFocus = T))),
                                           #     column(width = 3, offset = 0,
                                           #            shinyWidgets::actionGroupButtons(
                                           #              c("btn_import_pop","btn_sverest_pop","btn_saveas_pop"),
                                           #              c("Import","Save/Restore","Save As"),
                                           #              direction = "horizontal",
                                           #              status = "info",
                                           #              fullwidth = T
                                           #              
                                           #            ))
                                           #   ),
                                           #   fluidRow(
                                           #     pickerInput("param_names",label = "Select Parameters to assign variability",
                                           #                 choices = NULL,multiple = T,
                                           #                 options = list('selected-text-format' = "count > 3"))
                                           #     
                                           #   )
                                           #   
                                           # 
                                           #   
                                           # )
                                           ),
                                  tabPanel("Metabolism",

                                           fluidPage(
                                             fluidRow(
                                               column(12,
                                                      div(style = "height:10px")
                                               )
                                             ),
                                             fluidRow(

                                               column(2,
                                                      bsButton("btn_metab_upload",
                                                               "Upload Metabolism Files",

                                                               block = T)
                                                      )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      div(style = "height:10px")
                                               )
                                             ),
                                             fluidRow(
                                               column(width = 7, offset = 0,
                                                      selectizeInput("sel_metab",NULL,
                                                                     choices = NULL,
                                                                     options= list(placeholder = "Metabolism Parameter Set",
                                                                                   openOnFocus = T))),
                                               column(width = 5, offset = 0,
                                                      shinyWidgets::actionGroupButtons(
                                                        c("btn_sve_metab","btn_saveas_metab"),
                                                        c("Save","Save As"),
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
                                             # fluidRow(
                                             #   column(width = 3,
                                             #          fileInput("metab_csv","Upload Metabolism Data")),
                                             #   column(width = 3,
                                             #          downloadLink("metab_template","Template for metabolism file"))
                                             #   ),
                                             # fluidRow(
                                             #   column(width = 4,
                                             #          textInput("metab_set_name","Name",
                                             #                    placeholder = "Enter the name for this metabolism set")),
                                             #   column(width = 8,
                                             #          textAreaInput("metab_set_descrp","Description",
                                             #                        resize = "none" ,row = 1))
                                             #
                                             # ),
                                             # fluidRow(column(width = 6,
                                             #                 shinyWidgets::radioGroupButtons("metab_type",justified = T,
                                             #                                                 "Select Meatbolism Type",
                                             #                                                 choices = c("VmaxC"="m1","VlivC"="m2"))
                                             #                 )
                                             #
                                             #
                                             #
                                             #  ),
                                             # fluidRow(column(width = 4,
                                             #                 shinyWidgets::awesomeCheckbox("use_ref",
                                             #                                               "Use clearance at reference age for ages not in the metabolism table",
                                             #                                               value = T)
                                             #                 ),
                                             #          column(width = 4,
                                             #                 numericInput("metab_ref_age","Referance age in Years",value = 25, min = 0))
                                             # ),
                                             fluidRow(column(width = 6, offset = 3,
                                                             DT::DTOutput("metab_tble")))


                                           )

                                          ),
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
                                                            column(4,
                                                                   numericInput("sim_start","Simulation Start Time (h)",0)
                                                                   ),
                                                            column(4,
                                                                   numericInput("sim_dur","Simulation Duration (h)",0)
                                                                   ),
                                                            column(4,
                                                                   numericInput("mc_num","Number of Montecarlo Runs",1000)
                                                                   )
                                                          ),
                                                          checkboxInput("mc_mode","Run Monte Carlo Simulation",T),
                                                          shinyWidgets::actionBttn("save_sim",NULL,
                                                                                   icon = icon("floppy-save",lib = "glyphicon"),
                                                                                   style = "material-circle")
                                                        ),
                                                        icon = icon("plus"),circle = F, 
                                                        tooltip = F,size = "default",right = F,
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
                                               wellPanel(id= "Simulationdetails1",
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
                                                                  tags$h4("Simulation Duration (h)"),
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
                                                      box(title = "All Parameters",width = 4,
                                                          DT::DTOutput("chem_params_tble")),
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



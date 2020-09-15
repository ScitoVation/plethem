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

shinyjs.reset = function(){history.go(0)}
"
css <- "div .disabled-comp {
  color: #aaa !important;
  border-color: #a00 !important;
  background-color: #a00 !important;

}"


################################exposure sidebar
expo_sidebar <- shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
  id="ms_expo_sidebar",
  tags$div(
    tipify(
      actionButton("clear_expo","Reset Exposures"),
      "Reset all exposure values to zero. This is needed before a new set can be created",
      placement = "right")
    ),
  menuItem("Oral", tabName = "oral", selected = TRUE),
  menuItem("Drinking Water", tabName = "dw"),
  menuItem("Oral Exposure with Vehicle",tabName = "oralv"),
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
  pickerInput("qsarModelChem", "Select A QSAR Model",
              choices = c("QSAR Model One" = 'one'),
              #"Unified QSAR model" = 'two'),
              width = "99%"),
  tipify(shinyBS::bsButton("qsar4chem_props",
                           "Estimate Fraction Dissolved",
                           style = "primary"),
         "Estimate fraction dissoved in liquid phase of plasma using QSAR Models."
  )

)



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
                                       fluidRow(
                                         column(2,
                                                bsButton("btnAddData","Add Dataset",block = TRUE,style = "primary")
                                         )
                                       ),
                                       fluidRow(
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
                                                                                                   "Rapidly Perfused Total"="to_rpf",
                                                                                                   "Rapidly Perfused Tissue"="ti_rpf","Rapidly Perfused Exchange"="bl_rpf",
                                                                                                   "Slowly Perfused Total"="to_spf",
                                                                                                   "Slowly Perfused Tissue"="ti_spf","Slowly Perfused Exchange"="bl_spf",
                                                                                                   "Parent Urinary Concentration"="urine","Metabolite Urinary Concentration"="meturine"
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
                                                ))
                                       )
                                       ),
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

                                                )
                                                #selectizeInput("aplt_data",tags$h4("Select Data Sets"),choices = c("a","b","c"))

                                              )
                                       ),
                                       column(9,
                                              tabBox(width = 12,height = validateCssUnit("100%"),
                                                     tabPanel("Plot",
                                                              fluidRow(
                                                                tags$h5(class="text-center",
                                                                        radioButtons("r_aplt_type",label = "Amount Units",inline = TRUE,
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
        column(6,
               numericInput("ms_vbldc","Fractional Blood Compartment Volume",0.0832,0.01,1,0.01)),
        column(6,
               numericInput("ms_perfc","Total Fractional Perfused Tissue",1,0.75,0.95,0.01)
        )
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
        column(6,
               numericInput("ms_uflwc","Urinary Flow Rate (L/kg/day)",min =0 , max =1, value =0.0214)),
        column(6,
               numericInput("ms_gfr","Glomerular Filtration (L/h)",min =0 , max =1, value =0.08)
        )

      )
    ),

    tabItem(
      tabName = "fat_tissue",
      fluidRow(
        column(6,
               numericInput("ms_fatvtbc","Fat Tissue to Total Fat Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "fat_blood",
      fluidRow(
        column(6,
               numericInput("ms_vfatc","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.1841, step = 0.01)),
        column(6,
               numericInput("ms_qfatc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.08226))
      )
    ),
    tabItem(
      tabName = "skin_tissue",
      fluidRow(
        column(6,
               numericInput("ms_skinvtbc","Skin Tissue to Total Skin Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "skin_blood",
      fluidRow(
        column(6,
               numericInput("ms_vskinc","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.0553, step = 0.01)),
        column(6,
               numericInput("ms_qskinc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.06783))
      )
    ),
    tabItem(
      tabName = "muscle_tissue",
      fluidRow(
        column(6,
               numericInput("ms_muscvtbc","Muscle Tissue to Total Muscle Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "muscle_blood",
      fluidRow(
        column(6,
               numericInput("ms_vmuscc","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.4576, step = 0.01)),
        column(6,
               numericInput("ms_qmuscc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.13711))
      )
    ),
    tabItem(
      tabName = "bone_tissue",
      fluidRow(
        column(6,
               numericInput("ms_bonevtbc","Bone Tissue to Total Bone Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "bone_blood",
      fluidRow(
        column(6,
               numericInput("ms_vbonec","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.1318, step = 0.01)),
        column(6,
               numericInput("ms_qbonec","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.1266))
      )
    ),
    tabItem(
      tabName = "brain_tissue",
      fluidRow(
        column(6,
               numericInput("ms_brnvtbc","Brain Tissue to Total Brain Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "brain_blood",
      fluidRow(
        column(6,
               numericInput("ms_vbrnc","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.0192, step = 0.01)),
        column(6,
               numericInput("ms_qbrnc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.098))
      )
    ),
    tabItem(
      tabName = "lung_tissue",
      fluidRow(
        column(6,
               numericInput("ms_lngvtbc","Lung Tissue to Total Lung Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "lung_blood",
      fluidRow(
        column(6,
               numericInput("ms_vlngc","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.0144, step = 0.01)),
        column(6,
               numericInput("ms_qlngc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.0234))
      )
    ),
    tabItem(
      tabName = "heart_tissue",
      fluidRow(
        column(6,
               numericInput("ms_hrtvtbc","Heart Tissue to Total Heart Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "heart_blood",
      fluidRow(
        column(6,
               numericInput("ms_vhrtc","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.0051, step = 0.01)),
        column(6,
               numericInput("ms_qhrtc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.10536))
      )
    ),
    tabItem(
      tabName = "gi_tissue",
      fluidRow(
        column(6,
               numericInput("ms_givtbc","GI Tissue to Total GI Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "gi_blood",
      fluidRow(
        column(6,
               numericInput("ms_vgic","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.0222, step = 0.01)),
        column(6,
               numericInput("ms_qgic","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.1139))
      )
    ),
    tabItem(
      tabName = "liver_tissue",
      fluidRow(
        column(6,
               numericInput("ms_livvtbc","Liver Tissue to Total Liver Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "liver_blood",
      fluidRow(
        column(6,
               numericInput("ms_vlivc","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.0225, step = 0.01))
      ),
      fluidRow(
        column(6,
               numericInput("ms_qalivc","Arterial Blood Flow to Liver (Fraction of Cardiac Output)",min =0 , max =1, value =0.0381)),
        column(6,
               numericInput("ms_qvlivc","Fraction of Venous Liver Flow",min =0 , max =1, value =0.152))

      )
    ),
    tabItem(
      tabName = "kidney_tissue",
      fluidRow(
        column(6,
               numericInput("ms_kdnvtbc","Kidney Tissue to Total Kidney Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "kidney_blood",
      fluidRow(
        column(6,
               numericInput("ms_vkdnc","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.0046, step = 0.01)),
        column(6,
               numericInput("ms_qkdnc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.16886))
      )
    ),
    tabItem(
      tabName = "rpf_tissue",
      fluidRow(
        column(6,
               numericInput("ms_rpfvtbc","Rapidly Perfused Tissue to All Rapidly Perfused Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "rpf_blood",
      fluidRow(
        column(6,
               numericInput("ms_vrpfc","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.00001, step = 0.01)),
        column(6,
               numericInput("ms_qrpfc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0))
      )
    ),
    tabItem(
      tabName = "spf_tissue",
      fluidRow(
        column(6,
               numericInput("ms_spfvtbc","Slowly Perfused Tissue to All Slowly Perfused Volume Ratio",0.95,0,1,0.01))
      )
    ),
    tabItem(
      tabName = "spf_blood",
      fluidRow(
        column(6,
               numericInput("ms_vspfc","Volume (Fraction of Body Weight)",min =0, max = 1, value =0.00001, step = 0.01)),
        column(6,
               numericInput("ms_qspfc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0))
      )
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
                     numericInput("ms_vpa","Vapor Pressure at 25 \U00B0 C (Pa)",1,0,250,0.01)),
              column(6,
                     numericInput("ms_dkow","logKow in Skin at pH5.5",1,0,250,0.01))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_lkow","logKow (Octanol: Water Coefficient)",1,0,250,0.01)),
              column(6,
                     numericInput("ms_wsol","Water Solubility (mg/L)",1,0,250,0.01))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_res","Fraction Reabsorbed in Kidney",1,0,0,0.01)),
              column(6,
                     numericInput("ms_fupls", label = "Fraction Unbound in Plasma", value = 1, 0, 1, 0.001))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_frwsol", label = "Fraction Dissolved in Liquid Phase of Plasma", value = 1,
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
               numericInput("ms_blen",label = "Duration of Exposure per Day (h/day)", value = 1, step = 0.1))
      ),
      fluidRow(
        column(6,
               numericInput("ms_breps","Number of Boluses per Day",1,0,100, 1)),
        column(6,
               awesomeCheckbox("ms_brep_flag","Repeat Exposure Daily?",value = F))
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
                     numericInput("ms_dreps", "Number of Drinking Water Boluses per Day", value = 1, step = 0.1 )
              )
            )
    ),
    tabItem(
      tabName = "oralv",
      fluidRow(
        column(6,
               numericInput("ms_bdosev",label="Daily Oral Dose (mg/kg BW)", value =0, step= 0.01)),
        column(6,
               numericInput("ms_blenv",label = "Duration of Exposure per Day (h/day)", value = 1, step = 0.1))
      ),
      fluidRow(
        column(6,
               numericInput("ms_brepsv","Number of Boluses per Day",1,0,100, 1)),
        column(6,
               awesomeCheckbox("ms_brepv_flag","Repeat Exposure Daily?",value = F))
      )


    ),
    tabItem(
      tabName = "inh",
      fluidRow(
        column(4,
               numericInput("ms_inhdose",label="Inhalation Exposure (ppm)", value =0, step= 0.01)),
        column(4,
               numericInput("ms_inhtlen",label = "Duration of Inhalation Exposure (h/day)",8)),
        column(4,
               numericInput("ms_inhdays",label = "Exposure Days in a Week",7))
      )
    ),
    tabItem(
      tabName = "iv",
      fluidRow(
        column(6,
               numericInput("ms_ivdose",label = "IV Infusion (mg/h)",value = 0,step = 0.01)
        ),
        column(6,
               numericInput("ms_ivlen",label="Duration of IV Infusion (h)",0))
      ),
      fluidRow(
        column(6,
               awesomeCheckbox("ms_ivrep_flag","Repeat Exposure Daily?",value = F))
      )
    ),
    tabItem(
      tabName = "dermal",
      fluidRow(
        column(6,
               numericInput("ms_dermrate","Dermal Deposition Rate (mg/cm\U00B2/h)",0,step = 0.1)),
        column(6,
               numericInput("ms_skarea","Exposed Surface Area (cm\U00B2)",0,step = 0.1))
      ),
      fluidRow(
        column(6,
               numericInput("ms_dermlen","Duration of Dermal Exposure per Day (h/day)",0.1,step=0.01)),
        column(6,
               awesomeCheckbox("ms_dermrep_flag","Repeat Exposure Daily?",value = F))
      )

    )


  )
)


#################Shiny UI
shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  shinyWidgets::useSweetAlert(),
  theme = shinythemes::shinytheme("spacelab"),
  #shinythemes::themeSelector(),
  includeCSS("www/styles.css"),
  shinyjs::extendShinyjs(text = jscode, functions = c("disableTab", "enableTab", "reset")),
  shinyjs::inlineCSS(css),
  titlePanel(title="", windowTitle = "PLETHEM rapidPBPK"),
  tags$head(
    tags$meta(charset="utf-8"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$link(rel="icon", type="image/png", href="chem_32.png", sizes="32x32")
  ),
  #rintrojs::introjsUI(),
  navbarPage("", id="menu",collapsible = TRUE,selected = "home",
             navbarMenu("File",
                        tabPanel(title= "New",value = "new",icon = icon("plus")),
                        tabPanel(title = "Load",value = "load",icon = icon("floppy-open",lib = "glyphicon" )),
                        hidden(tabPanel(title = "Save",value = "save",icon = icon("floppy-save",lib = "glyphicon"))),
                        #tabPanel( id = "help" , title= "help", value = "Help", icon = icon("info")),
                        tabPanel(title = "Quit",value = "stop",icon=icon("power-off")),
                        "----",
                        tabPanel("About",value = "home",icon = icon("question"),
                                 includeHTML("www/home.html")
                        )
             ),
             tabPanel("Model Setup",value = "setup",
                      icon = icon("flask"),
                      fluidRow(
                        progressBar(id = "pb",value = 0, status = "success",striped = T)
                      ),
                      tabsetPanel(id= "modelSetupTabs", type = "tabs",
                                  tabPanel("Exposure",value = "expo",
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
                                               column(12,
                                                      div(style = "height:15px")
                                               )
                                             ),
                                             fluidRow(
                                               column(width = 7, offset = 0,
                                                      selectizeInput("sel_expo",NULL,
                                                                     choices = NULL,
                                                                     options= list(placeholder = "Exposure Parameter Set",
                                                                                   openOnFocus = T))),
                                               column(width = 5, offset = 0,
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


                                  tabPanel("Chemical",value = "chem",
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


                                  tabPanel("Physiological",value= "physio",


                                           fluidPage(


                                             fluidRow(
                                               column(12,
                                                      div(style = "height:15px")
                                               )
                                             ),
                                             fluidRow(
                                               column(width = 7, offset = 0,
                                                      selectizeInput("sel_physio",NULL,
                                                                     choices = NULL,
                                                                     options= list(placeholder = "Physiological Parameter Set",
                                                                                   openOnFocus = T))),
                                               column(width = 5, offset = 0,
                                                      shinyWidgets::actionGroupButtons(
                                                        c("btn_import_physio","btn_sverest_physio","btn_saveas_physio"),
                                                        c("Import","Save/Restore","Save As"),
                                                        direction = "horizontal",
                                                        status = "info",
                                                        fullwidth = T

                                                      ))
                                             ),
                                             fluidRow(
                                               column(4,
                                                      shinyBS::popify(shinyBS::bsButton("btn_use_lifecourse",
                                                                                        "Calculate Physiological Parameters",
                                                                                        style = "primary",block = TRUE),
                                                                      "Lifecourse Equations",
                                                                      "Lifecourse equations within PLETHEM are used to calculate blood flows, tissue volumes, respiration parameters and body weight for a given age and gender. Only works for Humans")
                                               )
                                             ),
                                             fluidRow(
                                               shinyBS::bsAlert("physio_header_alert")
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
                                  tabPanel("ADME",value = "adme",
                                           fluidPage(
                                             fluidRow(
                                               column(12,
                                                      div(style = "height:10px")
                                               )
                                             ),
                                             fluidRow(
                                               column(width = 8, offset = 0,
                                                      selectizeInput("sel_adme",NULL,
                                                                     choices = NULL,
                                                                     options= list(placeholder = "ADME",
                                                                                   openOnFocus = T))),
                                               column(width = 4, offset = 0,
                                                      shinyWidgets::actionGroupButtons(
                                                        c("btn_sverest_adme","btn_saveas_adme"),
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
                                               column(3,
                                                      selectizeInput("sel_expo4adme",choices= NULL,label= NULL,
                                                                     options = list(placeholder = "Select Exposure"))
                                               ),
                                               column(3,
                                                      selectizeInput("sel_chem4adme",choices = NULL,label = NULL,
                                                                     options = list(placeholder = "Select Parent"))
                                               ),
                                               column(3,
                                                      selectizeInput("sel_metabolite4adme",choices = NULL, label=NULL,
                                                                     options = list(placeholder = "Select Metabolite"))

                                               ),
                                               column(3,
                                                      selectizeInput("sel_physio4adme",choices=NULL,label = NULL,
                                                                     options = list(placeholder = "Select Physiology"))
                                               )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      div(style = "height:15px")
                                               )
                                             ),
                                             fluidRow(
                                               tabsetPanel(id = "adme_tabs",type = "pills",
                                                           tabPanel("Absorption",
                                                                    fluidPage(
                                                                      fluidRow(
                                                                        column(12,
                                                                               div(style = "height:5px")
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        column(4,
                                                                               numericInput("ms_fa","Fraction Absorbed in Gut Lumen",
                                                                                            value = 1, min = 0, max = 1)
                                                                        ),
                                                                        column(4,
                                                                               numericInput("ms_ka","Rate of Absorption in Gut Lumen (/h)",
                                                                                            value = 5,min = 0)),
                                                                        column(4,
                                                                               numericInput("ms_kVtoL","Tranfer Rate from Vehicle to Gut Lumen (/h)",
                                                                                            value = 1, min = 0)
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        column(4,
                                                                               numericInput("ms_KPtot","Total Stratum Corneum Permeation Coefficient (cm\U00B2/h)",
                                                                                            value = 1000,min = 0)),
                                                                        column(4,
                                                                               numericInput("ms_maxcap","Maximum Capacity of the Startum Corneum (mg/cm\U00B2)",
                                                                                            value = 1000, min =0)),
                                                                        column(4,
                                                                               numericInput("ms_Kevap","Evaporation Rate from Stratum Corneum",
                                                                                            value = 1000,min = 0))
                                                                      ),
                                                                      fluidRow(
                                                                        column(4,
                                                                               numericInput("ms_pair","Plasma-Air Partition Coefficient",value = 0,min = 0))
                                                                      )
                                                                    )
                                                           ),
                                                           tabPanel("Distribution",
                                                                    fluidPage(
                                                                      fluidRow(
                                                                        column(12,
                                                                               div(style = "height:5px")
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        column(4,
                                                                               shinyBS::tipify(
                                                                                 pickerInput("sel_qsar4Partition", NULL,
                                                                                             choices = c("QSAR Model One" = 'one',
                                                                                                         "Unified QSAR model" = 'two')
                                                                                 ),
                                                                                 "Select QSAR model for partition coefficients")
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
                                                                        dashboardPage(dashboardHeader(disable = T),
                                                                                      dashboardSidebar(sidebarMenu(id = "dist_comps",
                                                                                                                   menuItem("Fat",
                                                                                                                            tabName = "fat",selected = T),
                                                                                                                   menuItem("Skin",
                                                                                                                            tabName = "skn"),
                                                                                                                   menuItem("Muscle",
                                                                                                                            tabName = "msc"),
                                                                                                                   menuItem("Bone",
                                                                                                                            tabName = "bne"),
                                                                                                                   menuItem("Lung",
                                                                                                                            tabName = "lng"),
                                                                                                                   menuItem("Heart",
                                                                                                                            tabName = "hrt"),
                                                                                                                   menuItem("GI",
                                                                                                                            tabName = "gi"),
                                                                                                                   menuItem("Liver",
                                                                                                                            tabName = "liv"),
                                                                                                                   menuItem("Kidney",
                                                                                                                            tabName = "kdn"),
                                                                                                                   menuItem("Rapidly Perfused Tissue",
                                                                                                                            tabName = "rpf"),
                                                                                                                   menuItem("Slowly Perfused Tissue",
                                                                                                                            tabName = "spf")
                                                                                      )

                                                                                      ),
                                                                                      dashboardBody(tabItems(
                                                                                        tabItem(tabName = "fat",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_pfat",label = "Fat Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_pafat",label = "Fat Permeability Coefficient",value = 1000))
                                                                                                )
                                                                                        ),
                                                                                        tabItem(tabName = "skn",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_pskin",label = "Skin Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_paskin",label = "Skin Permeability Coefficient",value = 1000))
                                                                                                )

                                                                                        ),
                                                                                        tabItem(tabName = "msc",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_pmusc",label = "Muscle Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_pamusc",label = "Muscle Permeability Coefficient",value = 1000))
                                                                                                )
                                                                                        ),
                                                                                        tabItem(tabName = "bne",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_pbone",label = "Bone Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_pabone",label = "Bone Permeability Coefficient",value = 1000))
                                                                                                )
                                                                                        ),
                                                                                        tabItem(tabName = "brn",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_pbrn",label = "Brain Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_pabrn",label = "Brain Permeability Coefficient",value = 1000))
                                                                                                )
                                                                                        ),
                                                                                        tabItem(tabName = "lng",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_plng",label = "Lung Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_palng",label = "Lung Permeability Coefficient",value = 1000))
                                                                                                )
                                                                                        ),
                                                                                        tabItem(tabName = "hrt",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_phrt",label = "Heart Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_pahrt",label = "Heart Permeability Coefficient",value = 1000))
                                                                                                )
                                                                                        ),
                                                                                        tabItem(tabName = "gi",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_pgi",label = "GI Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_pagi",label = "GI Permeability Coefficient",value = 1000))
                                                                                                )
                                                                                        ),
                                                                                        tabItem(tabName = "liv",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_pliv",label = "Liver Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_paliv",label = "Liver Permeability Coefficient",value = 1000))
                                                                                                )
                                                                                        ),
                                                                                        tabItem(tabName = "kdn",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_pkdn",label = "Kidney Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_pakdn",label = "Kidney Permeability Coefficient",value = 1000))
                                                                                                )
                                                                                        ),
                                                                                        tabItem(tabName = "rpf",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_prpf",label = "RPF Tissue Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_parpf",label = "RPF Tissue Permeability Coefficient",value = 1000))
                                                                                                )
                                                                                        ),
                                                                                        tabItem(tabName = "spf",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         numericInput("ms_pspf",label = "SPF Tissue Partition Coefficient",value = 0.5)
                                                                                                  ),
                                                                                                  column(6,
                                                                                                         numericInput("ms_paspf",label = "SPF Tissue Permeability Coefficient",value = 1000))
                                                                                                )
                                                                                        )
                                                                                      )),skin = "green"
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        tags$hr(style = "border: 1px dashed black;")
                                                                      )
                                                                    )),
                                                           tabPanel("Metabolism",
                                                                    fluidPage(
                                                                      fluidRow(
                                                                        column(12,
                                                                               div(style = "height:10px")
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        column(4,
                                                                               bsButton("btn_metab_upload",
                                                                                        "Upload Age-based Metabolism Data",
                                                                                        block = T)
                                                                        ),
                                                                        column(6,
                                                                               selectizeInput("sel_metabfiles",choices = NULL,label = NULL)
                                                                        ),
                                                                        column(2,
                                                                               actionBttn("btn_use_age","Apply Data"))
                                                                      ),
                                                                      fluidRow(
                                                                        tags$hr(style = "border: 1px dashed black;")
                                                                      ),

                                                                      fluidRow(

                                                                        column(6,
                                                                               numericInput("ms_km","Michaelis-Menten Constant for Metabolism (μM)",1,0,250,0.01)
                                                                        ),
                                                                        column(6,
                                                                               shinyBS::bsButton("btn_ivive_chem",
                                                                                                 "Perform IVIVE",block = F)
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        column(6,
                                                                               numericInput("ms_vmaxc",paste0("Maximum Metabolism Rate (","μmol/h/kg BW^0.75)"),1,0,250,0.01)
                                                                        ),
                                                                        column(6,
                                                                               numericInput("ms_vkm1c", label = "First Order Metabolism in Liver (L/h/kg liver)", value = 1, step = 0.01)
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        tags$hr(style = "border: 1px dashed black;")
                                                                      ),
                                                                      fluidRow(
                                                                        column(6,
                                                                               numericInput("ms_kent",label = "Rate of Metabolism in the Gut lumen", value = 5, step = 0.01)
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        tags$hr(style = "border: 1px dashed black;")
                                                                      ),
                                                                      fluidRow(
                                                                        column(6,
                                                                               numericInput("ms_kbld","First Order Metabolism in Blood",0,0.75,0.95,0.01)
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        column(6,
                                                                               numericInput("ms_fuplsmet","Fraction unbound in plasma of the metabolite",1,0,1,0.01,width = validateCssUnit("100%"))),
                                                                        column(6,
                                                                               numericInput("ms_vdmetc","Fractional volume of distribution for the metabolite",0.6,0,1,0.01, width = validateCssUnit("100%")))
                                                                      )
                                                                    )),
                                                           tabPanel("Excretion",
                                                                    fluidPage(
                                                                      fluidRow(
                                                                        column(4,
                                                                               numericInput("ms_kfec","Rate of Fecal Excretion",value = 0, min = 0))
                                                                      )
                                                                    ))
                                               )
                                             )
                                           )

                                  ),
                                  tabPanel("Uncertainty and Variability", value = "variability",
                                           dashboardPage(
                                             dashboardHeader(disable = T),
                                             dashboardSidebar(
                                               shinydashboard::sidebarMenu(
                                                 menuItem("Chemical",
                                                          tabName = "var_chem",selected = T),
                                                 menuItem("Exposure",
                                                          tabName = "var_expo"),
                                                 menuItem("Physiological",
                                                          tabName = "var_physio"),
                                                 menuItem("ADME",
                                                          tabName = "var_adme")



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
                                                                               options= list(placeholder = "Chemical Parameter Set",
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
                                                                DT::DTOutput("chem_var_tble"))
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
                                                                DT::DTOutput("expo_var_tble"))
                                                       )
                                               ),
                                               tabItem(tabName = "var_adme",
                                                       fluidRow(
                                                         column(width = 4, offset = 0,
                                                                selectizeInput("sel_adme_var",NULL,
                                                                               choices = NULL,
                                                                               options= list(placeholder = "ADME Parameter Set",
                                                                                             openOnFocus = T))),
                                                         column(width = 8, offset = 0,
                                                                shinyWidgets::actionGroupButtons(
                                                                  c("btn_new_varadme","btn_edit_varadme","btn_import_varadme"),
                                                                  c("New","Edit","Import"),
                                                                  direction = "horizontal",
                                                                  status = "info",
                                                                  fullwidth = T)
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(8, offset = 2,
                                                                DT::DTOutput("adme_var_tble"))
                                                       )
                                               )
                                             )
                                             )
                                           )
                                  ),
                                  tabPanel("Biomonitoring Data",value ="biomdata",
                                           fluidPage(
                                             fluidRow(
                                               column(12,
                                                      div(style = "height:10px")
                                               )
                                             ),
                                             fluidRow(
                                               column(width = 8, offset = 0,
                                                      selectizeInput("sel_biom",NULL,
                                                                     choices = NULL,
                                                                     options= list(placeholder = "Biomonitoring Data",
                                                                                   openOnFocus = T))),
                                               column(width = 4, offset = 0,
                                                      shinyWidgets::actionGroupButtons(
                                                        c("btn_edit_biom","btn_new_biom"),
                                                        c("Edit","New"),
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
                                             #   column(4, offset = 1,
                                             #          tags$h4("Tissue"),
                                             #          textOutput("biom_tissue")
                                             #          ),
                                             #   column(4, offet = 2,
                                             #          tags$h4("Chemical"),
                                             #          textOutput("biom_chem")
                                             #          )
                                             #   ),
                                             fluidRow(
                                               plotly::plotlyOutput("biom_hist")
                                             )
                                           )
                                           ),
                                  tabPanel("Simulations",
                                           fluidPage(
                                             fluidRow(
                                               tags$br()
                                             ),

                                             fluidRow(
                                               br(),
                                               column(8,
                                                      selectizeInput("sel_sim",NULL,choices =NULL,
                                                                     width = validateCssUnit("99%"))
                                                      ),
                                               column(4,
                                                      actionGroupButtons(c("btn_edit_sim","btn_new_sim","btn_run_sim"),status = "info",
                                                                         labels = c("Edit","New","Run"),fullwidth = T)
                                               )


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
                                                           column(3,
                                                                  tags$h4("Simulation Type"),
                                                                  textOutput("sim_type")
                                                                  ),
                                                           column(3,
                                                                  tags$h4("Simulation Start (h)"),
                                                                  textOutput("sim_start")
                                                           ),
                                                           column(3,
                                                                  tags$h4("Simulation Duration"),
                                                                  textOutput("sim_dur")
                                                           ),
                                                           column(3,
                                                                  tags$h4("Duration Units"),
                                                                  textOutput("dur_units"))
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

                        column(3, offset= 6,
                               downloadButton("downloadModel", "Download Model",class = "btn btn-primary btn-block")
                        ),
                        column(
                          3,
                          actionGroupButtons(c("btn_dlHESI"),status = "btn btn-primary btn-block",
                                             labels = c("Download HESI"),fullwidth = T)
                        )
                      ),
                      tabsetPanel(id = "Modeloutput", type = "tabs",
                                  tabPanel("Plots",value = "plots",
                                           plot_body
                                  ),
                                  tabPanel("Parameters",value = "params",
                                           fluidPage(
                                             fluidRow(
                                               box(title = "Exposure Parameters",width = 4,
                                                   DT::DTOutput("expo_params_tble")),
                                               box(title = "Chemical Specific Parameters",width = 4,
                                                   DT::DTOutput("chem_params_tble")),
                                               box(title = "Physiological Parameters",width = 4,
                                                   DT::DTOutput("physio_params_tble"))

                                             ),
                                             fluidRow(
                                               downloadButton("btn_param_dwnld",
                                                              label = "Download All Paramters")
                                             ))
                                  ),
                                  tabPanel("Non-compartmental Analysis", value = "nca",
                                           fluidPage(
                                             fluidRow(
                                               DT::DTOutput("tble_ncavals")
                                             )
                                           )),
                                  tabPanel("Distribution Plots",value = "cdfpdf",
                                           fluidPage(
                                             bsCollapse(id = "revdos_plts",multiple = TRUE,
                                                        open = "cdfplt",
                                                        bsCollapsePanel("Cumulative Distribution Function",value = "cdfplt",
                                                                        plotlyOutput("CDF")
                                                        ),
                                                        bsCollapsePanel("Probability Distribution Function", value = "pdfplt",
                                                                        plotlyOutput("PDF")
                                                        )
                                             )
                                           )
                                  ),
                                  tabPanel("Exposure Estimates",value = "percentile",
                                           fluidPage(
                                             fluidRow(
                                               DT::DTOutput("expo_estimate" )
                                             )
                                           ))

                      )

             )

  )
))

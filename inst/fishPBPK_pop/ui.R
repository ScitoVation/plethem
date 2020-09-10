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
  menuItem("Inspiration Through Gills", tabName = "ins",selected = T)
))

################################compartment sidebar
comp_sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(id="compsidebar",
              # tags$div(shinyWidgets::radioGroupButtons("ms_org",
              #                                          label = "Select Organism",
              #                                          choices = c("Human" = "ha","Rat"="ra"),
              #                                          selected = "ha",size = "normal",
              #                                          checkIcon = list(yes = icon('ok',lib ="glyphicon")))),
              menuItem("Physiological Parameters",
                       tabName = "physiolocal_parameters", selected = TRUE),
              menuItem("Fat",tabName = "fat"),
              menuItem("Liver",tabName = "liv"),
              menuItem("Kidney",tabName = "kdn"),
              menuItem("Poorly Perfused Tissue",tabName = "spf"),
              menuItem("Richly Perfused Tissue",tabName = "rpf")
  )


  )


###############################Chemical Sidebar
chem_sidebar <- shinydashboard::dashboardSidebar()


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
                                                              choices = list("Total Fat"="fat",
                                                                             "Total Liver"="liv",
                                                                             "Total Kidney"="kdn",
                                                                             "Total Rapidly Perfused"="rpf",
                                                                             "Total Slowly Perfused "="spf",
                                                                             "Total Metabolite"="met"
                                                              )
                                                   )
                                                   ),
                                          tabPanel("Dataset",value = "dataset",
                                                   shinyWidgets::pickerInput("cplt_data",multiple = F,
                                                                            label = tags$h4("Select Datasets"),
                                                                            choices = c("No Dataset"="none"),
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
                           choices = list("Trout" = "tr","Catfish"="cf"),
                           selected = "tr")
               ),
        column(4,
               numericInput("ms_bw","Body Weight (kg)",1,0.1,90,0.1)),
        column(4,
               numericInput("ms_qc","Cardiac Output (L/h)",2.06,1,50,0.1))
      ),
      fluidRow(
        column(4,
               numericInput("ms_qg","Effective respiratory rate (L/h)",
                            min =0 , max =10, value =7.2)),
        column(4,
               numericInput("ms_pbldw", label = "Blood-Water Partition Coefficient",
                            value = 5.17, min = 0)

      ))),
    tabItem(
      tabName = "fat",
      fluidRow(
        column(6,
               numericInput("ms_vfatc","Volume (L)",min =0, max = 1, value =0.098, step = 0.01)),
        column(6,
               numericInput("ms_qfatc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.085))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_pfat","Partition Coefficient",1,0,2, 0.01)))
    ),
    tabItem(
      tabName = "liv",
      fluidRow(
        column(6,
               numericInput("ms_vlivc","Volume (L)",min =0, max = 1, value =0.012, step = 0.01)),

              column(6,
               numericInput("ms_qlivc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1,
                            value =0.029))


      ),

      fluidRow(class="",
               column(6,
                      numericInput("ms_pliv","Partition Coefficient",1,0,2, 0.01)))
    ),
    tabItem(
      tabName = "kdn",
      fluidRow(
        column(6,
               numericInput("ms_vkdnc","Volume (L)",min =0, max = 1, value =0.009, step = 0.01)),
        column(6,
               numericInput("ms_qkdnc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.056))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_pkdn","Partition Coefficient",1,0,2, 0.01)))
    ),
    tabItem(
      tabName = "rpf",
      fluidRow(
        column(6,
               numericInput("ms_vrpfc","Volume (L)",min =0, max = 1, value =0.063, step = 0.01)),
        column(6,
               numericInput("ms_qrpfc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.23))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_prpf","Partition Coefficient",1,0,2, 0.01)))
    ),
    tabItem(
      tabName = "spf",
      fluidRow(
        column(4,
               numericInput("ms_vspfc","Volume (L)",min =0, max = 1, value =0.818, step = 0.01)),
        column(4,
               numericInput("ms_qspfc","Blood Flow (Fraction of Cardiac Output)",min =0 , max =1, value =0.6))
      ),
      fluidRow(class="",
               column(6,
                      numericInput("ms_pspf","Partition Coefficient",1,0,2, 0.01)))
    )
  )
)

####################chemical Body
chem_body <- dashboardBody(
  fluidPage(id="Chemicals",
            tags$h4("Chemical Parameters", class="pager-header"),
            fluidRow(

              column(6,
                     numericInput("ms_mw","Molecular Weight (g/mol)",167.8,0,250,0.01))
            ),
            fluidRow(
              column(6,
                     numericInput("ms_vmax",paste0("Maximum Metabolism Rate (","mg/h)"),0,0,250,0.01)),
              column(6,
                     numericInput("ms_km","Michaelis-Menten Constant for Metabolism (mg/L)",0,0,250,0.01))
            ),
            # fluidRow(
            #   column(6,
            #          numericInput("ms_intrinsicClearance", "Intrinsic Clearance (L/h)", 0,250,0.01))
            #   ),
            # fluidRow(
            #   column(6,
            #          checkboxInput("ms_useIntrinsicClearance", paste0("","Use Intrinsic Clearance",sep="\n"), value = FALSE))
            # )
  ))



######################exposure Body
expo_body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "ins",
      fluidRow(
        column(4,
               numericInput("ms_cins",label="Concentration in Water (mg/L)",
                            value =1.062,
                            step= 0.01)
               )

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
                                               )
                                               )
                                               )
                                           )


                                          ),
                                  tabPanel("Simulations",
                                           fluidPage(
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
                                                          fluidRow(
                                                            column(4,
                                                                   numericInput("sim_start","Simulation Start Time (h)",0)
                                                                   ),
                                                            column(4,
                                                                   numericInput("sim_dur","Simulation Duration (h)",0)
                                                                   ),
                                                            column(4,
                                                                   numericInput("mc_num","Number of Monte Carlo Runs",1000)
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
                                                         )#,
                                                         # fluidRow(
                                                         #   column(4,
                                                         #          tags$h4("Age"),
                                                         #          textOutput("sim_age")
                                                         #   ),
                                                         #   column(4,
                                                         #          tags$h4("Gender"),
                                                         #          textOutput("sim_gender")
                                                         #   )
                                                         # ),
                                                         # fluidRow(
                                                         #   column(4,
                                                         #          tags$h4("Metabolism Type"),
                                                         #          textOutput("sim_metab_type")
                                                         #          ),
                                                         #   column(4,
                                                         #          tags$h4("Value"),
                                                         #          textOutput("sim_metab_val")
                                                         #          ),
                                                         #   column(4,
                                                         #          tags$h4("Units"),
                                                         #          textOutput("sim_metab_units")
                                                         #          )
                                                         #
                                                         # )

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
                                                      box(title = "Chemical Specific Parameters",width = 4,
                                                          DT::DTOutput("chem_params_tble")),
                                                      box(title = "Physiological Parameters",width = 4,
                                                          DT::DTOutput("physio_params_tble"))
                                             ),
                                             fluidRow(
                                               downloadButton("btn_param_dwnld",
                                                              label = "Download All Paramters")
                                             ))
                                  )

                      )

             ),
             tabPanel( id = "help" , title= "help", value = "Help", icon = icon("info")),
             tabPanel(title = "Quit",value = "stop",icon=icon("power-off"))
  )
))

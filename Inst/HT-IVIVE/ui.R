# library(shiny)
# library(shinydashboard)
# library(shinythemes)
# #library(shinyBS)

shinyUI(fluidPage(
  withMathJax(),
  shinyWidgets::useSweetAlert(),
  theme = shinythemes::shinytheme("lumen"),
  titlePanel(title = "",windowTitle = "High Throuput IVIVE"),
  navbarPage("",id="navMenu",
             tabPanel(title = "Setup",
                      icon= icon("flask"),
                      fluidPage(
                        fluidRow(
                          column(4,offset = 2,
                                 shinyBS::bsButton("btn_import_chem","Import Chemical",style = "primary",
                                                   type = "action",block = TRUE)),
                          column(4,
                                 shinyBS::bsButton("run","Perform HT-IVIVE",style="info",
                                                   type = "action",block = TRUE))
                        ),
                        fluidRow(tags$h4("")),
                        fluidRow(
                          column(3,offset = 1,
                                 shinyBS::bsButton("add_row","Add New Row",block = TRUE)
                          ),
                          column(3,offset = 1,
                                 shinyBS::bsButton("edit_row","Edit Selected Row",block = TRUE)
                          ),
                          column(3,offset = 1,
                                 shinyBS::bsButton("remove_row","Remove Selected Row",block = TRUE))
                        ),
                        fluidRow(
                          DT::dataTableOutput("master_table")
                        )
                      )
             ),
             tabPanel(title = "Results",
                      fluidRow(
                        DT::dataTableOutput("result_table")
                      )),
             tabPanel(title = NULL,icon = icon("power-off"),
                      value = "stop")
  )
))

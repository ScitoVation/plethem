library(shiny)
library(RSQLite)
library(DT)
library(shinyWidgets)
library(shinyBS)
library(plotly)
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

shinyUI(
  tagList(
    tags$head(
      tags$link(
        rel='icon',
        href='cropped-ScitoVation_icon-32x32.png',
        # href="https://www.scitovation.com/wp-content/uploads/2019/02/cropped-ScitoVation_icon-32x32.png",
        sizes="32x32"
      )
    ),
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    ")),
    useSweetAlert(),
    useShinyjs(),
    includeCSS("www/styles.css"),
    navbarPage(
      title = tags$img(
        height = 25,
        src = 'sciv_logo_transparent.png'
        # src = "https://www.scitovation.com/wp-content/themes/scitovation/assets/images/logo/ScitoVation-Pioneers_in_Chemical_Safety_Assessment.svg"
      ),
      id = 'navbar',
      position = 'fixed-top',
      windowTitle = 'PLETHEM: Reverse Dosimetry',
      fluid = T,
      collapsible = T,
      tabPanel(
        title = 'Inputs',
        icon = icon('line-chart'),
        tags$style(
          type='text/css',
          'body {padding-top: 50px; #background-color: #db6e00;}
          #rDataFile_progress {margin-bottom:0;}'
        ),
        div(
          Id = 'mySidenav',
          class = 'sidenav',
          style = 'width: 300px; padding-top: 80px;',
          align = 'center',
          bsButton(
            'btnUploadMC',
            'Add Monte Carlo Simulation',
            block = TRUE,
            style = 'primary',
            width = '80%'
          ),
          br(),
          bsButton(
            'btnUploadBMResults',
            'Add Biomonitoring Results',
            block = TRUE,
            style = 'primary',
            width = '80%'
          ),
          br(),
          bsButton(
            'btnRunRevDos',
            'Run Reverse Dosimetry',
            block = TRUE,
            style = 'primary',
            width = '80%'
          )
        ),
        div(
          Id = 'main',
          style = 'margin-left:300px; padding: 10px;',
          span(
            style='font-size:30px;cursor:pointer',
            onclick='toggleNav()',
            HTML('&#9776;')
          ),
          fluidRow(
            column(
              12,
              wellPanel(
                align='center',
                tags$h3('Monte Carlo Results'),
                plotlyOutput("Plot1", height = "600px")
              ),
              wellPanel(
                align='center',
                tags$h3('Biomonitoring Results'),
                plotlyOutput("Plot3", height = 600)
              )
            )
          )
        ),
        tags$script(
          'function toggleNav() {
          if(document.getElementById("mySidenav").style.width !== "0px"){
            document.getElementById("mySidenav").style.width = "0";
            document.getElementById("main").style.marginLeft= "0";
          } else{
            document.getElementById("mySidenav").style.width = "300px";
            document.getElementById("main").style.marginLeft = "300px";
          }

          // Triggers a resize event
          var evt = document.createEvent("UIEvents");
          evt.initUIEvent("resize", true, false, window, 0);
          window.dispatchEvent(evt);
          }
        '
        )
      ),
      tabPanel(
        title = 'Output',
        style = 'padding-top: 5px;',
        icon = icon('table'),
        tabsetPanel(
          id = 'Modeloutput',
          type = 'tabs',
          tabPanel(
            'Plots',
            fluidPage(
              fluidRow(
                column(
                  12,
                  br(),
                  wellPanel(
                    align='center',
                    tags$h3('Probability Density Function'),
                    plotlyOutput("PDF", height = "600px")
                  ),
                  wellPanel(
                    align='center',
                    tags$h3('Cumulative Density Function'),
                    plotlyOutput("CDF", height = "600px")
                  )
                )
              )
            )
          ),
          tabPanel(
            'Results',
            fluidPage(
              fluidRow(
                column(
                  8,
                  offset = 2,
                  align = 'center',
                  DT::dataTableOutput('percentilePPB',width = 400)
                )
              )
            )
          ),
          tabPanel(
            'Data',
            fluidPage(
              fluidRow(
                column(
                  12,
                  br(),
                  wellPanel(
                    tags$style('.datatables th {text-align: right;} h3 {text-align: center;}'),
                    tags$h3('Probability Table'),
                    DT::dataTableOutput('revDosData1'),
                    downloadButton('downloadTablerevDosData1',label = "Download Table")
                  ),
                  wellPanel(
                    tags$h3('Adjusted Weighting Factors from Worksheet "Measured blood conc"'),
                    DT::dataTableOutput('revDosData2'),
                    downloadButton('downloadTablerevDosData2',label = "Download Table")
                  ),
                  wellPanel(
                    tags$h3('Weight the ECFs with the Adjusted Weighting Factors'),
                    DT::dataTableOutput('revDosData3'),
                    downloadButton('downloadTablerevDosData3',label = "Download Table")
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel(
        title = 'About',
        style = 'padding-top: 5px;',
        icon = icon('info-circle'),
        fluidPage(
          fluidRow(
            column(
              12,
              align = 'Center',
              # Application title
              h2('PLETHEM: Reverse Dosimetry')
            )
          ),
          fluidRow(
            column(
              8,
              offset = 2,
              
              h4(
                p('PLETHEM: Reverse Dosimetry is a user-interface module used
                to perform reverse dosimetry calculations for estimating exposure concentrations. 
                The module requires the user to input two files containing the predicted biomarker 
                concentrations from a Monte Carlo simulation and the biomonitoring results.'
                )
              )
            )
          )
        )
      ),
      tabPanel(title = "Quit",icon = icon("power-off"))
    )
  )
)


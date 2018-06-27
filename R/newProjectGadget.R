#' @export
newProjectGadget <- function(){
  ui <- miniPage(
    gadgetTitleBar("Create new project"),
    miniContentPanel(
      fillCol(flex = 1,
              fluidRow(
                textInput("pname","Enter Project Name",width = validateCssUnit("90%")),
     
                selectInput("mname","Select Model",
                            choices = list("PLETHEM" = list("rapidPBPK"="rapidPBPK"),
                                           "HTTK" = list("PBTK"="httk_pbtk")
                                           ),
                            width = validateCssUnit("90%")
                            ),
                selectInput("wtype","Select Workflow Type",
                            choices=list("Monte Carlo"="MC",
                                         "Forward Dosimetry"="FD"),
                            width = validateCssUnit("90%"))
              ))
    )
  )
  server <- function(input,output,session){
    observeEvent(input$done,{
      pname <- input$pname
      mname <- input$mname
      mtype <- "PBPK"
      wtype <- input$wtype
      code <- sprintf("newProject(name = '%s', type = '%s', model = '%s', mode = '%s')",
              pname,
              mtype,
              mname,
              wtype)
      sendToConsole(code,T)
      stopApp()
    })
    
  }
  runGadget(ui,server,viewer =dialogViewer("Start a new PLETHEM Project",width = 400,height = 400))
}
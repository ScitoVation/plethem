#' @export
#' @importFrom rstudioapi sendToConsole
addChemsToHTTK <- function(){
  
  ui <- miniPage(
    gadgetTitleBar("Add Chemicals to HTTK"),
    miniContentPanel(
      shinyjs::useShinyjs(),
      fillCol(flex = c(2,2,2,1,6),
        fillRow(
          textInput("cname","Compound Name",placeholder = "Enter Name",
                    value = "Demo Chem",
                    width = validateCssUnit("90%")),
          textInput("casnm","CAS Number",placeholder = "00-00-0001",
                    value = "00-00-00001",
                    width = validateCssUnit("90%")),
          selectInput("org","Select Organism",choices = c("Human","Rat"),
                      width = validateCssUnit("90%"))
          ),
        fillRow(
            numericInput("mw","Molecular Weight",0,width = validateCssUnit("90%")),
            numericInput("logp","LogP",0.1,width = validateCssUnit("90%"))
            ),
        fillRow(
          numericInput("clint","Intrinic Clearance",0,width = validateCssUnit("90%")),
          numericInput("fupls","Fraction Unbound in Plasma",1,width = validateCssUnit("90%"))
        ),
        fillRow(
          checkboxInput("show_code","Show Code",value = F)
        ),
        fillRow(
          shinyjs::hidden(textAreaInput("code_output","HTTK Add Chemical Function",
                        width = validateCssUnit("100%"),rows = 6))
          )
        )
      )
    )
  server <- function(input,output,session){
    observe({
      org <- input$org
      name <- input$cname
      cas <-input$casnm
      mw <- input$mw
      logp <- input$logp
      clint <- input$clint
      fupls <- input$fupls
      DF_code <- sprintf("data2add <- data.frame('Compound' = c('%s'), 'CAS' = c('%s'),'MW' = c(%f),'logP' = c(%f),'Clint' = c(%f), 'Funbound.plasma' = c(%f),stringsAsFactors = F)",
                         name,cas,mw,logp,clint,fupls)
      name_code <- "data_list <- setNames(colnames(data2add),colnames(data2add))"
      httk_table <- 'chem.physical_and_invitro.data'
      add_code <- sprintf("%s <- add_chemtable(data2add,data_list,%s,reference = 'None',species = '%s', overwrite = T)",
                          httk_table,httk_table,org)
      final_code <- paste(DF_code,";\n",name_code,";\n",add_code)

      updateTextAreaInput(session,"code_output",value = final_code)
    })
    observeEvent(input$done,{
      code <- input$code_output
      sendToConsole(code,T)
      stopApp()
    })
    observeEvent(input$show_code,{
      if(input$show_code){
        shinyjs::show("code_output")
      }else{
        shinyjs::hide("code_output")
      }
    })
  }
  runGadget(ui,server,viewer =dialogViewer("IVIVE",width = 800,height = 800))
}
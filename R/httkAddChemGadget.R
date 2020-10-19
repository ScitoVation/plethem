#' Function that adds chemical to httk package chem list.
#' @description internal function that interfaces with httk package
#' @import httk
#' @importFrom rstudioapi sendToConsole
#' @examples
#' \dontrun{
#' addChemsToHTTK()
#' }
#' @export
addChemsToHTTK <- function(){
  
  ui <- miniPage(
    gadgetTitleBar("Add Chemical Data to HTTK"),
    miniContentPanel(
      shinyjs::useShinyjs(),
      fillCol(flex = c(2,2,2,2,1,6),
        fillRow(
          pickerInput("sel_chem","",
                      choices = c("New Chemical",
                                  httk::chem.physical_and_invitro.data$Compound),
                      options = list(size = 10,
                                     'live-search'=T,
                                     title = "Select Chemical"),
                      selected = "New Chemical",
                      width = validateCssUnit("90%")
                      ),
          selectInput("org","Select Organism",choices = c("Human","Rat"),
                      width = validateCssUnit("90%"))
                
        ),
        fillRow(
          textInput("cname","Compound Name",placeholder = "Enter Name",
                    value = "New Chemical",
                    width = validateCssUnit("90%")),
          textInput("casnm","CAS Number",placeholder = "00-00-0001",
                    value = "00-00-00001",
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
          checkboxInput("show_code","Show Code",value = FALSE
        ),
        fillRow(
          shinyjs::hidden(textAreaInput("code_output","HTTK Add Chemical Function",
                        width = validateCssUnit("100%"),rows = 6))
          )
        )
      )
    )
  server <- function(input,output,session){
    observeEvent({
      input$sel_chem
      input$org
      },
      {
        chem_name <- input$sel_chem
        org <- input$org
        updateTextInput(session,"cname",value = chem_name)
        if (chem_name == "New Chemical"){
          casn <- "00-00-0001"
          fupls <- 1
          logp <- 0.1
          clint <- 0
          mw <- 0
        }else{
          temp <- httk::chem.physical_and_invitro.data
          row_data <- temp[which(temp$Compound == chem_name),]
          mw <- row_data$MW
          casn <- row_data$CAS
          logp <- row_data$logP
          fupls <- row_data[[paste0(org,".Funbound.plasma")]]
          clint <- row_data[[paste0(org,".Clint")]]
        }
        updateTextInput(session,"casnm",value = casn)
        updateNumericInput(session,"mw",value = mw)
        updateNumericInput(session,"logp",value = logp)
        updateNumericInput(session,"clint",value = ifelse(is.na(clint),0,clint))
        updateNumericInput(session,"fupls",value = ifelse(is.na(fupls),0,fupls))
                           
    },ignoreInit = TRUE
    
    observe({
      org <- input$org
      name <- input$cname
      cas <-input$casnm
      mw <- input$mw
      logp <- input$logp
      clint <- input$clint
      fupls <- input$fupls
      DF_code <- sprintf("data2add <- data.frame('Compound' = c('%s'), 'CAS' = c('%s'),'MW' = c(%f),'logP' = c(%f),'Clint' = c(%f), 'Funbound.plasma' = c(%f),stringsAsFactors = FALSE",
                         name,cas,mw,logp,clint,fupls)
      name_code <- "data_list <- setNames(colnames(data2add),colnames(data2add))"
      httk_table <- 'chem.physical_and_invitro.data'
      add_code <- sprintf("%s <- add_chemtable(data2add,data_list,%s,reference = 'None',species = '%s', overwrite = TRUE",
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
  runGadget(ui,server,viewer =dialogViewer("HTTK Chemical Data",width = 800,height = 800))
}
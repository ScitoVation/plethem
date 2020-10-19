shinyServer(function(input,output,session){
  parameterSets <- reactiveValues()

  parameterSets$savedat <- reactiveVal(c("No","",0))
  parameterSets$sverestdat <- reactiveVal(c("None",0))
  parameterSets$importdat <- reactiveVal(c("No","",0))
  #chem_list <- getChemicalList()

  observeEvent(input$btn_import_chem,{
    importParameterSetUI(input$btn_import_chem,"chem")
    parameterSets$importdat <- callModule(importParameterSet,input$btn_import_chem,"chem",module_source = "HT-IVIVE")

  })

  return_val<- reactiveValues("data_added" = NULL)
  vals <- reactiveValues()
  vals$chemical <- character()
  vals$m_table <- data.table::data.table(
    #"row_number"=numeric(),
    "rn"=numeric(),
    "Name"=character(),
    "Chemical"=numeric(),
    "Organism"=numeric(),
    "Type" = numeric(),
    "Standard Exposure"=numeric(),
    "In vitro POD"=numeric(),
    "Hepatic Clearance"=numeric(),
    "Renal Clearance"=numeric(),
    "Plasma Clearance"=numeric(),keep.rownames = TRUE
  )
  vals$result_table <- data.table::data.table(
    "Name"=character(),

    "Chemical"=numeric(),
    "Organism"=numeric(),#c("Human Adult","Human Adult",
                 #"Rat Adult","Human Adult"),
    "Type" = numeric(),#c("Oral Non Volatile","Oral Non Volatile",
               #"Oral Non Volatile","Oral Non Volatile"),
    "Standard Exposure"=numeric(),#c("1 mg/kg/day","1 mg/kg/day",
                         #"1 mg/kg/day","1 mg/kg/day"),
    "In vitro POD"=character(),#c("50.3 \u03BCm",
                    #"10.2 \u03BCm",
                    #"2.5 mg/L",
                    #"8.9 \u03BCm"),
    "Actual Hepatic Clearance (L/h)"=numeric(),
    "Actual Renal Clearance (L/h)"=numeric(),#10,#=numeric(),
    "Scaled Blood Clearance (L/h)"=numeric(),#10,#numeric(),
    "Css (mg/L)"=numeric(),#10,#numeric(),
    #"Equivalent Dose Type"=numeric(),#10,#numeric(),
    "Equivalent dose (Exposure Units)"=character(),
    "Exposure" = character(),#15#numeric()
    "Margin of exposure"=numeric()
  )
  output$master_table <- DT::renderDataTable(
    DT::datatable(data = vals$m_table,rownames = FALSE,escape = FALSE,selection = "single",
                  options = list(
                    dom="tpl",
                    preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                    drawCallback = DT::JS('function() {
                                          Shiny.bindAll(this.api().table().node()); } '),
                    columnDefs =list(list(
                      targets = 0,
                      visible = FALSE
                    ))
                  )),server = T)
  # scientific_notation_js <- c("function(row,data){",
  #                             "for (i=6,i< data.length,i++){",
  #                             "$('td:eq('+i+')',row).html(data[i].toExponential(2);",
  #                             "}",
  #                             "}")

  output$result_table <- DT::renderDataTable(
    DT::datatable(data = vals$result_table,rownames = FALSE,escape = FALSE,selection = "single",extensions = "Buttons",
                  options = list(
                    dom="Btpl",
                    charset = "utf-8",
                    buttons = c("copy","csv","colvis"))#,
                    #rowCallback = DT::JS(scientific_notation_js))
                    ),server = T)
    # ,extensions = "Buttons",
    #               options = list(buttons=c('copy'),
    #                              dom = 'Bfrtip'))

  #mster_tble_proxy <- DT::dataTableProxy("master_table",session,deferUntilFlush = FALSE)
  observeEvent(input$run,{
    non_reactive_vals <- reactiveValuesToList(vals)

    valid_row_names <- names(which(!(sapply(non_reactive_vals[grep("row_*",names(non_reactive_vals))],is.null))))
    data_list <- non_reactive_vals[valid_row_names]
    result <- runPlthemHTIVIVE(data_list)
    vals$result_table <- makeResultTable(isolate(vals$m_table),result)
    updateTabsetPanel(session,"navMenu","Results")

  })
  observeEvent(input$add_row,{
    chem_list <- getAllSetChoices("chem")
    namespace <- paste0("add",as.character(input$add_row))
    HT_IVIVEUI(namespace)

    vals<- callModule(id =namespace, module = HT_IVIVE,vals,
                      type = "add",
                      chem_list = chem_list,
                      idx = as.numeric(input$add_row))


    if (length(chem_list)>0){

    }else{
      shinyWidgets::sendSweetAlert(session,"No chemicals found",
                                   "Please import chemicals to the project",
                                   type = "error")
    }

    })
  observeEvent(input$edit_row,{
    row_selected <- input$master_table_rows_selected
    if (is.null(row_selected)){
      shinyWidgets::sendSweetAlert(session,
                                   "No Row Selected","",
                                   type = "error")
    }else{
      namespace <- paste0("edit",as.character(input$edit_row))
      HT_IVIVEUI(namespace)
      vals<- callModule(id =namespace, module = HT_IVIVE,vals,
                        type = "edit",chem_list = chem_list,
                        row_selected = row_selected)
    }


    }
    #chem_names <- getChemicalList()

  )
  observeEvent(input$navMenu,{
    if (input$navMenu == "stop"){
      clearProjectDb()
      query <- "Update Utils Set Value=NULL;"
      mainDbUpdate(query)
      stopApp()
    }
  })
  observeEvent(input$remove_row,{
    selected_row <- input$master_table_rows_selected
    if (is.null(selected_row)){
      shinyWidgets::sendSweetAlert(session,
                                   "No Row Selected","",
                                   type = "error")
    }else{
      row_data <- vals$m_table[selected_row,]
      row_number <- row_data$rn
      row_key <- paste0("row_",row_number)
      vals$m_table <- vals$m_table[!(selected_row),]
      vals[[row_key]]<- NULL
    }

  })

})

makeResultTable <- function(input_table,result){
  output_table <- data.table::data.table(
    "Name"=input_table$Name,

    "Chemical"=input_table$Chemical,
    "Organism"=input_table$Organism,
    "Type" = input_table$Type,
    "Standard Exposure"=input_table[["Standard Exposure"]],
    "In vitro POD"=input_table[["In vitro POD"]],
    "Actual Hepatic Clearance (L/h)"=paste0(lapply(result,"[[","hep")),
    "Actual Renal Clearance (L/h)"=paste0(lapply(result,"[[","ren")),
    "Actual Plasma Clearance (L/h)"=paste0(lapply(result,"[[","pls")),
    "Css (mg/L)"=paste0(lapply(result,"[[","css")),
    #"Equivalent Dose Type"=numeric(),
    "Equivalent Dose"=paste0(lapply(result,"[[","eqdose")),
    "Exposure" = paste0(lapply(result,"[[","expo")),
    "Margin of Exposure"=paste0(lapply(result,"[[","moe"))
  )

}

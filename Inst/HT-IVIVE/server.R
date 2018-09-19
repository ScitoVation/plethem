shinyServer(function(input,output,session){
  parameterSets <- reactiveValues()

  parameterSets$savedat <- reactiveVal(c("No","",0))
  parameterSets$sverestdat <- reactiveVal(c("None",0))
  parameterSets$importdat <- reactiveVal(c("No","",0))
  #chem_list <- getChemicalList()

  observeEvent(input$btn_import_chem,{
    importParameterSetUI(input$btn_import_chem,"chem")
    parameterSets$importdat <- callModule(importParameterSet,input$btn_import_chem,"chem")

  })

  return_val<- reactiveValues("data_added" = NULL)
  vals <- reactiveValues()
  vals$chemical <- character()
  vals$m_table <- data.table::data.table(
    #"row_number"=numeric(),
    "rn"=numeric(),
    "Chemical"=numeric(),
    "Organism"=numeric(),
    "Type" = numeric(),
    "Standard Exposure"=numeric(),
    "Invitro POD"=numeric(),
    "Hepatic Clearance"=numeric(),
    "Renal Clearance"=numeric(),
    "Plasma Clearance"=numeric(),keep.rownames = TRUE
  )
  vals$result_table <- data.table::data.table(

    "Chemical"=numeric(),#c("Chemical A",
                 # "Chemical B",
                 # "Chemical C",
                 # "Chemical D"),
  #"100-53-23",#numeric(),
    "Organism"=numeric(),#c("Human Adult","Human Adult",
                 #"Rat Adult","Human Adult"),
    "Type" = numeric(),#c("Oral Non Volatile","Oral Non Volatile",
               #"Oral Non Volatile","Oral Non Volatile"),
    "Standard Exposure"=numeric(),#c("1 mg/kg/day","1 mg/kg/day",
                         #"1 mg/kg/day","1 mg/kg/day"),
    "Invitro POD"=character(),#c("50.3 \u03BCm",
                    #"10.2 \u03BCm",
                    #"2.5 mg/L",
                    #"8.9 \u03BCm"),
    "Actual Hepatic Clearance (L/h)"=numeric(),
    "Actual Renal Clearance (L/h)"=numeric(),#10,#=numeric(),
    "Scaled Blood Clearance (L/h)"=numeric(),#10,#numeric(),
    "Css (mg/L)"=numeric(),#10,#numeric(),
    #"Equivalent Dose Type"=numeric(),#10,#numeric(),
    "Equivalent dose (Exposure Units)"=character()#15#numeric()
  )
  output$master_table <- DT::renderDataTable(
    DT::datatable(data = vals$m_table,rownames = F,escape = F,selection = "single",
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

  output$result_table <- DT::renderDataTable(
    DT::datatable(data = vals$result_table,rownames = F,escape = F,selection = "single",extensions = "Buttons",
                  options = list(
                    dom="Btpl",
                    buttons = c("copy","csv","colvis"))
                    ),server = T)
    # ,extensions = "Buttons",
    #               options = list(buttons=c('copy'),
    #                              dom = 'Bfrtip'))

  #mster_tble_proxy <- DT::dataTableProxy("master_table",session,deferUntilFlush = F)
  observeEvent(input$run,{
    #print(names(vals))
    non_reactive_vals <- reactiveValuesToList(vals)

    valid_row_names <- names(which(!(sapply(non_reactive_vals[grep("row_*",names(non_reactive_vals))],is.null))))
    data_list <- non_reactive_vals[valid_row_names]
    #print(data_list)
    result <- runPlthemHTIVIVE(data_list)
    #print(result)
    vals$result_table <- makeResultTable(isolate(vals$m_table),result)
    updateTabsetPanel(session,"navMenu","Results")

  })
  observeEvent(input$add_row,{
    chem_list <- getAllSetChoices("chem")
    if (length(chem_list)>0){
      namespace <- paste0("add",as.character(input$add_row))
      HT_IVIVEUI(namespace)

      vals<- callModule(id =namespace, module = HT_IVIVE,vals,
                        type = "add",
                        chem_list = chem_list,
                        idx = as.numeric(input$add_row))
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
      stopApp()
    }
  })
  # observeEvent(input$ok,{
  #   num_add <- isolate(as.numeric(input$add_row))
  #   chem <- input$sel_chem2add
  #   removeModal()
  #   data2add <- data.table::data.table("Chemical"=chem,
  #                                      "CAS"=num_add,
  #                                      "Organism"=num_add,#('<input type="button" class="btn btn-default action-button" id="physio%s" value="Human_%s"/>',as.character(num_add),as.character(num_add)),
  #                                      "Hepatic_Clearance"=num_add,
  #                                      "Renal_Clearance"=num_add,#sprintf('<input type="checkbox" id="renal%s" />',as.character(num_add)),
  #                                      "Plasma_Clearance"=num_add,
  #                                      "Invitro_Concentration"=num_add)
  #
  #
  # })
  # observe({
  #   data2add <- return_val$data_added
  #   if (!(is.null(data2add))){
  #     vals$m_table <- rbind(vals$m_table,data2add)
  #   }
  #
  # })
  # observeEvent(input$edit_row,{
  #   selected_row <- input$master_table_rows_selected
  #   print(vals$m_table[selected_row,])
  # })
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
  # test<-list()
  # test$renal <- reactive({
  #   input_names <-names(reactiveValuesToList(isolate(input)))
  #   renal_names <- grep("renal_*",input_names,value = TRUE)
  #   return(renal_names)
  #   })
  #
  # test$physio <- reactive({
  #   input_names <-names(reactiveValuesToList(isolate(input)))
  #   physio_names <- grep("physio_*",input_names,value = TRUE)
  #   return(physio_names)
  # })
  # observeEvent(input,{
  #   input_names<- names(reactiveValuesToList(input))
  #   return(input_names)
  #   #tbl_names <- apply(input_names,function(x){x.startsWith("physio")})
  #   #print(tbl_names)
  #   # if (length(tbl_names==0)){
  #   #   return(list())
  #   # }else{
  #   #   return(lapply(tbl_names,function(x){input[[x]]}))
  #   # }
  #
  #   })
  # observeEvent(tbl_physio(),{
  #   print(tbl_physio)
  # })
 # observeEvent(unlist(lapply(test$renal(),function(x){input[[x]]})),{
 #   print(grep("renal_*",names(reactiveValuesToList(input)),value = TRUE))
 #   print(sapply(test$renal(),function(x){input[[x]]}))
 # },ignoreInit = TRUE,ignoreNULL = TRUE)
 #
 # observeEvent(unlist(lapply(test$physio(),function(x){input[[x]]})),{
 #   print(test$physio())
 # },ignoreInit = TRUE)

  #Set standard exposure for the type of reverse dosimetry
  # observeEvent(input$rd_type,{
  #   rd_type <- input$rd_type
  #   if (rd_type == "oralnonvol"){
  #     output$stdExp <- renderUI({
  #       withMathJax("$$1 \\frac{mg}{kg*day}$$")
  #     })
  #   }else if(rd_type == "oralvol"){
  #     output$stdExp <- renderUI({
  #       withMathJax("$$1 \\frac{mg}{kg*day}$$")
  #     })
  #   }else{
  #     output$stdExp <- renderUI({
  #       tags$h5(withMathJax("$$1 \\frac{mg}{L}$$"))
  #     })
  #   }
  # })

})

makeResultTable <- function(input_table,result){
  output_table <- data.table::data.table(

    "Chemical"=input_table$Chemical,
    "Organism"=input_table$Organism,
    "Type" = input_table$Type,
    "Standard Exposure"=input_table[["Standard Exposure"]],
    "Invitro POD"=input_table[["Invitro POD"]],
    "Actual Hepatic Clearance (L/h)"=paste0(lapply(result,"[[","hep")),
    "Actual Renal Clearance (L/h)"=paste0(lapply(result,"[[","ren")),
    "Actual Plasma Clearance (L/h)"=paste0(lapply(result,"[[","pls")),
    "Css (mg/L)"=paste0(lapply(result,"[[","css")),
    #"Equivalent Dose Type"=numeric(),
    "Equivalent dose"=paste0(lapply(result,"[[","eqdose"))
  )

}

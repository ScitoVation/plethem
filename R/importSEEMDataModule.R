#' @export
importSEEMDataUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(
    title = "Import Seem Data",
   
    radioGroupButtons(ns("seem_filter"),"Application",selected = "Consumer and Industrial", 
                      choices = c("Consumer and Industrial",
                                  "Industrial Only",
                                  "Active Component in Pesticides",
                                  "Inert Component in Pesticides")),
    actionButton(ns("get_list"),"Get Selected Chemical List"),
    pickerInput(ns("chems"),"Select Chemicals to Import",choices = c(""),multiple = T),
    checkboxGroupButtons(ns("data2add"),"Select Estimates to Import",
                         choices = c("Total Median"="Total.Median",
                                     "Total Upper 95th Percentile"="Total.Upper95")),
    
    footer = tagList(
      actionButton(ns("import"),"Import"),
      modalButton("Dismiss")
    )
    
  ))
}
#' @export
importSEEMData <- function(input,output,session){#,expo_name_df){
  ns <- session$ns
  reactiveVals <- reactiveValues()
  reactiveVals$choices <- c("")
  id_name <- "expoid"
  set_table_name <- "ExposureSet"
  vals_table_name <- "Exposure"
  # get the current ID for the parameter set from project database
  query <- sprintf("SELECT %s FROM %s ;",id_name,set_table_name)
  id_list <- projectDbSelect(query)
  
  if (length(id_list[[id_name]])==0){
    id_num = 1
  }else{
    id_num = max(id_list[[id_name]])+1
  }
  
  fpath <- reactive({
    fpath <- file.choose()
    return(fpath)
  })
  observeEvent(input$get_list,{
    query <- "Select Substance_CASRN  from Predictions;"
    path <- fpath()
    result <- externDbSelect(query,path)
    result2display <- result[["Substance_CASRN"]][1:10]
    updatePickerInput(session,"chems",choices = result2display)
    # if(!(is.null(input$seem_db))){
    #   print(input$seem_db$datapath)
    # }
  })
  observeEvent(input$import,{
    chem_list <- input$chems
    print(chem_list)
    path <- fpath()
    print(path)
    
    for (each_cas in chem_list){
      query<- sprintf("SELECT Total_upper95 From Predictions Where Substance_CASRN = '%s';",
                      each_cas)
      result <- externDbSelect(query,path)
      name <- each_cas
      descrp <-"Imported From SEEM"
      query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                       set_table_name,
                       id_name,
                       id_num,
                       name,
                       descrp)
      projectDbUpdate(query)
      #vars_names <- expo_name_df$
      data2write <- list("bdose"=result$Total_Upper95,
                         "breps"=1,
                         "blen"=1,
                         "brep_flag"="FALSE"
                         )
      
      var_names <- names(data2write)
      
      vals <- paste0("'",as.character(data2write),"'")
      
      all_values_string <- paste(paste0(sprintf('(%d,',id_num),
                                        sprintf("'%s'",var_names),
                                        ',',vals,')'),
                                 collapse = ", ")
      write_col_names <- sprintf("%s, param, value",id_name)
      query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                       vals_table_name,
                       write_col_names,
                       all_values_string)
      
      projectDbUpdate(query)
      
      id_num <- id_num+1
      
      
    }
    
    
  })
  
}
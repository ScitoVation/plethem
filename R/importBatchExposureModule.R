#' UI function for importing generic exposure data
#' @description UI function for importing generic exposure data into PLETHEM. The function should not be called by the user
#' @param namespace namespace for the module
#' @export
importBatchExposureUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(
    shinyWidgets::useSweetAlert(),
    title= "Import exposure data",
    fileInput(ns("expo_upload"),
              "Select Exposure file",multiple = FALSE
              accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
              ),
    shinyBS::bsCollapse(
      shinyBS::bsCollapsePanel("Oral Exposure",
                               DT::DTOutput(ns("oralDT")) ),
      shinyBS::bsCollapsePanel("Drinking Water Exposure",
                               DT::DTOutput(ns("dwDT")) ),
      shinyBS::bsCollapsePanel("Inhalation Exposure",
                               DT::DTOutput(ns("inhDT")) ),
      shinyBS::bsCollapsePanel("Intravenous Exposure",
                               DT::DTOutput(ns("ivDT")) )
    ),
    # ,
    # DT::DTOutput(ns("inh_expo")),
    # DT::DTOutput(ns("iv_expo")),
    footer = tagList(modalButton("Dismiss"),
                     shinyBS::bsButton(ns("import"),"Import Selected Exposures")),
    # pickerInput(ns("sel_expo"),multiple = TRUElabel = "Select Exposure",choices = NULL),
    # DT::DTOutput(ns("expo_tble"))
    # 
    # tabsetPanel(id = ns("expo_box"),type = "tabs",
    #             tabPanel(title = "Oral",
    #                      value = "oral",
    #                      DT::DTOutput(ns("oral_expo"))),
    #             tabPanel(tile = "Inhalation",
    #                      value = "inh",
    #                      DT::DTOutput(ns("inh_expo"))),
    #             tabPanel(title="Intravenous",
    #                      value ="iv",
    #                      DT::DTOutput(ns("iv_expo")))),
    size = "l"


    
    
  ))
}
#' Server function for batch data module
#' @description Server function for import batch data module. This function should not be called by the user
#' @param input input object for the UI
#' @param output input object to the UI
#' @param session session object for the module
#' @param expo_name_df dataframe containing variable names for exposure values
#' @export
#'@export
importBatchExposure<- function(input,output,session,expo_name_df){
  returnValues <- reactiveValues()
  returnValues$retdata <- c("No")
  expo_file <- reactive({   
    input$expo_upload
  })
  data_file_path <- reactive({
    validate(need(input$expo_upload,"No File Uploaded"))
    return(expo_file()$datapath)
  })
  
  oral_tble <- reactive({
    data <- readxl::read_excel(data_file_path(),sheet = "Oral")
    return(data)
  })
  inh_tble <- reactive({
    data <- readxl::read_excel(data_file_path(),sheet = "Inhalation")
    return(data)
  })
  dw_tble <- reactive({
    data <- readxl::read_excel(data_file_path(),sheet = "Drinking Water")
    return(data)
  })
  iv_tble <- reactive({
    data <- readxl::read_excel(data_file_path(),sheet = "Intravenous")
    return(data)
  })
  
  # save_val <- reactive({
  #   parseBatchExposureFile(data_file_path(),expo_name_df)
  #   temp_df <- data.frame()
  #   data_oral<- readxl::read_xlsx(data_file_path(),sheet = "Oral")
  #   data_inh<- readxl::read_xlsx(data_file_path(),sheet = "Inhalation")
  #   data_inh
  #   return(data$Name)
  # })
  # 
  # updatePickerInput(session,"sel_expo",choices = expo_choices())
  output$oralDT <- DT::renderDT(DT::datatable(oral_tble(),
                                                 autoHideNavigation = TRUE
                                                 fillContainer = TRUErownames = FALSE,server = TRUE
  output$inhDT <- DT::renderDT(DT::datatable(inh_tble(),
                                                autoHideNavigation = TRUE
                                                fillContainer = TRUErownames = FALSE,server = TRUE
  output$dwDT <- DT::renderDT(DT::datatable(dw_tble(),
                                               autoHideNavigation = TRUE
                                               fillContainer = TRUErownames = FALSE,server = TRUE
  output$ivDT <- DT::renderDT(DT::datatable(iv_tble(),
                                               autoHideNavigation = TRUE
                                               fillContainer = TRUErownames = FALSE,server = TRUE
  
  observeEvent(input$import,{
    oral_rows <- input$oralDT_rows_selected
    #print(oral_rows)
    inh_rows <- input$inhDT_rows_selected
    dw_rows <- input$dwDT_rows_selected
    iv_rows <- input$ivDT_rows_selected
    
    if (all(is.null(c(oral_rows,inh_rows,dw_rows,iv_rows)))){
      #print(oral_rows)
      shinyWidgets::sendSweetAlert(session,"No Exposure Selected",type = "error")
    }else{
      # parse Oral exposures and write to database
      for (i in oral_rows){
        print(i)
        data <- as.data.frame(oral_tble()[i,],stringsAsFactors = FALSE
        print(data)
        colnames(data)<- c("Name","bdose","blen","breps","brep_flag")
        name <- data$Name
        print(name)
        id_num <- getNextID("ExposureSet")
        descrp <- "Imported from batch file"
        query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                         "ExposureSet",
                         "expoid",
                         id_num,
                         name,
                         descrp)
        #print(query)
        projectDbUpdate(query)
        
        var_names <- expo_name_df$Var
        data2write <- setNames(rep(0,length(var_names)),var_names)
        data2write["expo_sidebar"]<-"oral"
        data2write["bdose"]<- data$bdose
        data2write["blen"]<- data$blen
        data2write["breps"]<- data$breps
        data2write["brep_flag"]<- ifelse(data$brep_flag == "Yes","TRUE","FALSE")
        vals <- paste0("'",as.character(data2write),"'")
        
        all_values_string <- paste(paste0(sprintf('(%d,',id_num),
                                          sprintf("'%s'",var_names),
                                          ',',vals,')'),
                                   collapse = ", ")
        write_col_names <- sprintf("%s, param, value","expoid")
        query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                         "Exposure",
                         write_col_names,
                         all_values_string)
        #print(query)
        projectDbUpdate(query)
        
        
      }
      # parse Oral exposures and write to database
      for (i in dw_rows){
      #  print(i)
        data <- as.data.frame(oral_tble()[i,],stringsAsFactors = FALSE
       # print(data)
        colnames(data)<- c("Name","drdose","dreps","vdw")
        name <- data$Name
       # print(name)
        id_num <- getNextID("ExposureSet")
        descrp <- "Imported from batch file"
        query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                         "ExposureSet",
                         "expoid",
                         id_num,
                         name,
                         descrp)
       # print(query)
        projectDbUpdate(query)
        
        var_names <- expo_name_df$Var
        data2write <- setNames(rep(0,length(var_names)),var_names)
        data2write["expo_sidebar"]<-"dw"
        data2write["drdose"]<- data$drdose
        data2write["dreps"]<- data$dreps
        data2write["vdw"]<- data$vdw
        
        vals <- paste0("'",as.character(data2write),"'")
        
        all_values_string <- paste(paste0(sprintf('(%d,',id_num),
                                          sprintf("'%s'",var_names),
                                          ',',vals,')'),
                                   collapse = ", ")
        write_col_names <- sprintf("%s, param, value","expoid")
        query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                         "Exposure",
                         write_col_names,
                         all_values_string)
       # print(query)
        projectDbUpdate(query)
      }
      
      # parse Inhalation exposures and write to database
      for (i in inh_rows){
        #print(i)
        data <- as.data.frame(oral_tble()[i,],stringsAsFactors = FALSE
        #print(data)
        colnames(data)<- c("Name","inhdose","inhtlen","inhdays")
        name <- data$Name
        #print(name)
        id_num <- getNextID("ExposureSet")
        descrp <- "Imported from batch file"
        query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                         "ExposureSet",
                         "expoid",
                         id_num,
                         name,
                         descrp)
        #print(query)
        projectDbUpdate(query)
        
        var_names <- expo_name_df$Var
        data2write <- setNames(rep(0,length(var_names)),var_names)
        data2write["expo_sidebar"]<-"inh"
        data2write["inhdose"]<- data$inhdose
        data2write["inhtlen"]<- data$inhtlen
        data2write["inhdays"]<- data$inhdays
        
        vals <- paste0("'",as.character(data2write),"'")
        
        all_values_string <- paste(paste0(sprintf('(%d,',id_num),
                                          sprintf("'%s'",var_names),
                                          ',',vals,')'),
                                   collapse = ", ")
        write_col_names <- sprintf("%s, param, value","expoid")
        query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                         "Exposure",
                         write_col_names,
                         all_values_string)
        #print(query)
        projectDbUpdate(query)
        
      }
      
      # parse Intravenous exposures and write to database
      for (i in oral_rows){
       # print(i)
        data <- as.data.frame(oral_tble()[i,],stringsAsFactors = FALSE
        #print(data)
        colnames(data)<- c("Name","ivdose","ivlen","ivrep_flag")
        name <- data$Name
       # print(name)
        id_num <- getNextID("ExposureSet")
        descrp <- "Imported from batch file"
        query <- sprintf("INSERT INTO %s (%s, name, descrp) VALUES (%d, '%s' , '%s' );",
                         "ExposureSet",
                         "expoid",
                         id_num,
                         name,
                         descrp)
       # print(query)
        projectDbUpdate(query)
        
        var_names <- expo_name_df$Var
        data2write <- setNames(rep(0,length(var_names)),var_names)
        data2write["expo_sidebar"]<-"iv"
        data2write["ivdose"]<- data$ivdose
        data2write["ivlen"]<- data$ivlen
        data2write["ivrep_flag"]<- ifelse(data$ivrep_flag == "Yes","TRUE","FALSE")
        vals <- paste0("'",as.character(data2write),"'")
        
        all_values_string <- paste(paste0(sprintf('(%d,',id_num),
                                          sprintf("'%s'",var_names),
                                          ',',vals,')'),
                                   collapse = ", ")
        write_col_names <- sprintf("%s, param, value","expoid")
        query <- sprintf("INSERT INTO %s (%s) VALUES %s ;",
                         "Exposure",
                         write_col_names,
                         all_values_string)
       # print(query)
        projectDbUpdate(query)
        
        
      }
      removeModal()
      
    }
    
    
  })
  returnValues$retdata<- eventReactive(input$import,{return(c("Yes"))})
}
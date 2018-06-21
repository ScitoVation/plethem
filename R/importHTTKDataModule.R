#' UI for importing data from HTTK
#' @export
importHTTKDataUI <- function(namespace){
  shinyjs::useShinyjs()
  ns <- NS(namespace)
  httk_chem <- chem.physical_and_invitro.data$CAS
  names(httk_chem)<- chem.physical_and_invitro.data$Compound
  showModal(
    modalDialog(title = "Select Chemicals",size = "l",
                selectInput(ns("org"),"Select Organism Data to Import",
                            choices=c("Human","Rat"),selected ="Human"),
                multiInput(ns("chems"),"Chemicals to Import",choices = httk_chem),
                footer = tagList(
                  actionButton(ns("import"),"Import"),
                  modalButton("Dismiss")
                  )
                )
    )
  return(NULL)
}
#'@export
importHTTKData <- function(input,output,session){
  returnValues <- reactiveValues()
  returnValues$retdata <- c("No","",0)
  ns <- session$ns
  id_name <- "chemid"
  set_table_name <- "ChemicalSet"
  vals_table_name <- "Chemical"
  dataset <- chem.physical_and_invitro.data
  # get the current ID for the parameter set from project database
  query <- sprintf("SELECT %s FROM %s ;",id_name,set_table_name)
  id_list <- projectDbSelect(query)
  
  if (length(id_list[[id_name]])==0){
    id_num = 1
  }else{
    id_num = max(id_list[[id_name]])+1
  }
  
  observeEvent(input$import,{
    selected_list <- input$chems
    org <- input$org
    for (each_cas in selected_list){
      
      dataidx <- which(dataset$CAS == each_cas)
      name <- dataset$Compound[dataidx]
      descrp <-"Imported From HTTK"
      query <- sprintf("INSERT INTO %s (%s, name, descrp,cas) VALUES (%d, '%s' , '%s','%s' );",
                       set_table_name,
                       id_name,
                       id_num,
                       name,
                       descrp,
                       each_cas)
      projectDbUpdate(query)
      fupls <- dataset[[paste0(org,".Funbound.plasma")]][dataidx]
      fupls <- ifelse(is.null(fupls),1,fupls)
      Clint <- dataset[[paste0(org,".Clint")]][dataidx]
      Clint <- ifelse(is.null(Clint),0,Clint)
      Clmetabolismc <- calc_hepatic_clearance(chem.cas = each_cas)
      data2write <- list("Clmetabolismc"=ifelse(is.null(Clmetabolismc),0,Clmetabolismc),
                         "Funbound.plasma"=fupls,
                         "MW"=dataset$MW[dataidx],
                         "Clint"=Clint,
                         "LogP"=dataset$logP[dataidx],
                         "LogPwa"= ifelse(is.na(dataset$logPwa[dataidx]),0,dataset$logPwa[dataidx]),
                         "LogMA"=ifelse(is.na(dataset$logMA[dataidx]),0,dataset$logMA[dataidx]),
                         "pKa_Accept"= ifelse(is.na(dataset$pKa_Accept[dataidx]),0,dataset$pKa_Accept[dataidx]),
                         "pKa_Donor"=ifelse(is.na(dataset$pKa_Donor[dataidx]),0,dataset$pKa_Donor[dataidx]),
                         "Corg"=org )
      
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
    #print(selected_list)
    removeModal()
  })
  returnValues$retdata<- eventReactive(input$import,{return(c("Yes","chem",1))})
}
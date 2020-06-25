# Module for uploading biomonitering data
# This module is only called by the rapidPBPK_pop UI and hence is not a part 
# of the main R/ folder of the package
newEditBiomoniteringDataUI <- function(namespace,biomid=NULL){
  if (is.null(biomid)){
    selected_set <- list("chem"=NULL,"tissue"=NULL,"units"=NULL)
    unit_choices = NULL
  }else{
    query <- sprintf("Select chem,tissue,units from Biomonitering Where biomid = %i",
                     biomid)
    selected_set <- projectDbSelect(query)
    if(selected_set$tissue=="pls"){
      unit_choices = list("\u00B5moles/L"="uml","mg/L"="mgl")
    }else{
      unit_choices = list("mg/L"="mgl","\u00B5g/day"="ugd")
    }
  }
  ns <- NS(namespace)
  modalType  <- "Import New Biomonitoring Data"
  showModal(modalDialog(title = modalType,
                        useSweetAlert(),
                        fluidPage(
                          fluidRow(
                            column(6,
                                   fileInput(ns("btn_import_file"),
                                             "Upload Biomonitoring Data",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv"))
                                   )
                          ),
                          fluidRow(
                            textInput(ns("txt_biom_name"),NULL,placeholder = "Name",
                                      width = validateCssUnit("100%"))
                          ),
                          fluidRow(
                            textAreaInput(ns("txt_biom_descrp"),NULL,
                                          placeholder = "Description",
                                          width = validateCssUnit("100%")
                            )
                          ),
                          fluidRow(
                            column(
                              5,
                              shinyWidgets::radioGroupButtons(
                                ns("rdobtn_tissue"),
                                label = "Select Tissue Type",
                                choices = list("Plasma"="pls","Urine"="urine"),
                                selected = selected_set$tissue,
                                status = "info",
                                checkIcon = list("yes"=icon("ok",lib = "glyphicon")),
                                justified = T,
                                width = "100%"
                              )
                            ),
                            column(
                              5,
                              shinyWidgets::radioGroupButtons(
                                ns("rdobtn_chemType"),
                                "Select Chemical Type",
                                choices = list("Parent"="prnt", "Metabolite"="met"),
                                selected = selected_set$chem,
                                status = "info",
                                checkIcon = list("yes"=icon("ok",lib = "glyphicon")),
                                justified = T,
                                width = "100%"
                              )
                            ),
                            column(
                              2,
                              selectizeInput(
                                ns("sel_biomdata_units"),"Data Units",
                                choices = unit_choices,
                                selected = selected_set$units
                              )
                            )
                          )
                        ),
                        footer = tagList(
                          modalButton("Close"),
                          actionButton(ns("ok"),"Save Set")),
                        fade =T,
                        size = "l"
                        )
            )
            
}

newEditBiomoniteringData <- function(input,output,session,type = "new",biomid = NULL){
  returnValues <- reactiveValues()
  returnValues$savedat <- c("No","",0)
  ns <- session$ns
  
  
  if(type == "edit"){
    query <- sprintf("Select name, descrp from BiomoniteringSet where biomid = %i",
                     biomid)
    set_meta <- projectDbSelect(query)
    updateTextInput(session,"txt_biom_name",value = set_meta$name)
    updateTextAreaInput(session,"txt_biom_descrp",value = set_meta$descrp)
    # query <- sprintf("Select tissue,units from Biomonitering Where biomid = %i",
    #                  biomid)
    # selected_set <- projectDbSelect(query)
    # if(selected_set$tissue=="pls"){
    #   unit_choices = list("\u00B5moles/L"="uml","ng/L"="ngl","mg/L"="mgl")
    # }else{
    #   unit_choices = list("ng/L"="ngl","mg/L"="mgl","\u00B5g/day"="ugd")
    # }
    # updateSelectizeInput(session,"sel_biomdata_units",choices = unit_choices,
    #                      selected =selected_set$units)
  }
  observeEvent(input$rdobtn_tissue,{
    tissue <- input$rdobtn_tissue
    if(tissue=="pls"){
      choices = list("\u00B5moles/L"="uml","ng/L"="ngl","mg/L"="mgl")
    }else{
      choices = list("ng/L"="ngl","mg/L"="mgl","\u00B5g/day"="ugd")
    }
    updateSelectizeInput(session,"sel_biomdata_units",choices = choices)
  },ignoreInit = T,ignoreNULL = T)
  returnValues$savedat<- eventReactive(input$ok,{
    name <- input$txt_biom_name
    descrp <- input$txt_biom_descrp
    chem <- input$rdobtn_chemType
    tissue <- input$rdobtn_tissue
    units <- input$sel_biomdata_units
    if(type == "new" && is.null(input$btn_import_file)){
      sendSweetAlert(session,"Error",
                     "No Biomonitoring Data Uploaded",
                     type = "error")
      return(c("No","",0))
      
    }
    else if(name == "" || descrp== ""){
      sendSweetAlert(session,"Error",
                     "Please enter name and description",
                     type = "error")
      return(c("No","",0))
    }
    else{
      
      #write the data
      if(type == "new"){
        biom_fpath <- input$btn_import_file$datapath
        biom_data <- as.data.frame(read.csv(biom_fpath))
        serialized_biom_data <- rawToChar(serialize(biom_data,NULL,T))
        biomid <- as.integer(getNextID("BiomoniteringSet"))
        query <- sprintf("Insert Into BiomoniteringSet (biomid,name,descrp) VALUES (%i,'%s','%s');",
                         biomid,name,descrp)
        projectDbUpdate(query)
        query <- sprintf("Insert Into Biomonitering (biomid,chem,tissue,units,data) VALUES (%i,'%s','%s','%s','%s');",
                         biomid,chem,tissue,units,serialized_biom_data)
        projectDbUpdate(query)
        sendSweetAlert(session,title = "Saved","New biomonitering data added",type ="info")
      }else{
        # if the file import is null then new data should not be written
        if(is.null(input$btn_import_file$datapath)){
          query <- sprintf("Update Biomonitering Set chem = '%s',tissue='%s', units = '%s' where biomid = %i;",
                           chem,
                           tissue,
                           units,
                           biomid)
          projectDbUpdate(query)
        }else{
          biom_fpath <- input$btn_import_file$datapath
          biom_data <- as.data.frame(read.csv(biom_fpath))
          serialized_biom_data <- rawToChar(serialize(biom_data,NULL,T))
          query <- sprintf("Update Biomonitering Set chem = '%s',tissue='%s', units = '%s', data = '%s' where biomid = %i;",
                           chem,
                           tissue,
                           units,
                           serialized_biom_data,
                           biomid)
          projectDbUpdate(query)
        }
       
      }
      removeModal()
      return(c("Yes","biom",biomid))
    }
  })
  return(returnValues$savedat)
}
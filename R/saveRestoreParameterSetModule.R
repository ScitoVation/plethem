# This module describes the UI and server for the restore/save dialog in PLETHEM UI

#' UI for the restore/save dialog
#' @description UI for the save restore dialog box. This function should not be called by the user.
#' @param namespace namespace for the UI
#' @export
saveRestoreParameterSetUI <- function(namespace){
  ns <- NS(namespace)
  showModal(modalDialog(
    title = "Save or Restore Selected Parameters",
    tagList(DTOutput(ns("param_table"))),
    footer = fluidRow(
      column(6,
             shinyWidgets::actionGroupButtons(c(ns("saveall"),ns("savesel")),
                                                c("Save All","Save Selected"),
                                                direction = "horizontal",fullwidth = T )
      ),

      column(6,
             shinyWidgets::actionGroupButtons(c(ns("restoreall"),ns("restoresel")),
                                              c("Restore All","Restore Selected"),
                                              direction = "horizontal",fullwidth = T )
             )
    )

    ,easyClose = TRUE
  ))
}


#' Server for the restore/save dialog
#' @description Server for the save restore dialog box. This function should not be called by the user.
#' @param input input object for the UI
#' @param output input object to the UI
#' @param session session object for the module
#' @param UI_values values for the parameters in the UI
#' @param set_values values for the parameters in the database
#' @param param_names names of parameters to save or restore
#' @param type type of parameter set to save
#' @export
saveRestoreParameterSet <- function(input,output,session,UI_values,set_values,param_names,type){
  returnValues <- reactiveValues()
  returnValues$retData <- reactiveVal(c("None"))
  ns <- session$ns

  col_names <- c("Name","Variable","Original Value","Current Value")


  name_list <- names(UI_values)
  

  name_list <- name_list[!(name_list == "cmplist")]
  print(setdiff(name_list,names(set_values)))
  temp_unique_list <- unname(lapply(name_list,
                             function(x,a=UI_values,b=set_values){
                               a[[x]]==b[x]}))
  print(temp_unique_list)
  param_data<-t(as.data.frame(UI_values[temp_unique_list == FALSE]))
  #give some time for the UI to Load so it doesnt hiccup
  Sys.sleep(0.5)
  if (dim(param_data)[1]==0){
    param_data <- data.frame(matrix(ncol = 4, nrow = 0))
    sendSweetAlert(session,"No changes detected",
                   "The user interface data mataches the set data. No changes were detected.",closeOnClickOutside = F,
                   showCloseButton = T)
    removeModal()

  }else{
    # get the variable names that are changed
    vars <- rownames(param_data)
    # get the original values for these variables
    org_values <- as.data.frame(set_values[vars],stringsAsFactors = FALSE)
    print(org_values)
    # convert to data frame
    param_data <- data.frame(param_data,stringsAsFactors = FALSE)
    # Merge the original values to the correct table by using row names
    param_data <- merge(param_data,org_values,by = "row.names")
    # set the column name of the first column to Var.
    # this is the column name in the param_names dataframe
    colnames(param_data)[1] <- "Var"
    # merge the names from the param_names dataframe to the change table
    # based on the the values in column Var
    param_data <- merge(param_names[,c("Name","Var")],
                        param_data,by = "Var")
    # After merge the columns are in the order
    # variable, Name, New value , Orginal Value
    # reorder them to be
    # Name, Variable, Original Value, New Value
    param_data <- param_data[c(2,1,4,3)]

  
  colnames(param_data) <- col_names
  output$param_table<- DT::renderDataTable(DT::datatable(param_data),server = TRUE)
  
  
  
  
  returnValues$retData <- eventReactive({
    input$saveall
    input$savesel
    input$restoresel
    input$restoreall
    1
    },{
      rows_selected <- input$param_table_rows_selected
      selected_data <- param_data[rows_selected,c(2,3,4)]
      all_data <- param_data[,c(2,3,4)]
      if(input$saveall == 1){
        return_data <- c("save",all_data,type)
        removeModal()
      }else if(input$restoreall == 1){
        return_data <- c("restore",all_data,type)
        removeModal()
      }else if (input$savesel == 1){
        return_data <- c("save",selected_data,type)
        removeModal()
      }else if(input$restoresel ==1){
        return_data <- c("restore",selected_data,type)
        removeModal()
      }else{
        return_data <- c("None")
      }


    return(return_data)
  },ignoreInit = TRUE,ignoreNULL = T)
  }

  return(returnValues$retData)

}


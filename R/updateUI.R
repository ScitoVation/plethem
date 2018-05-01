#' Update Inputs for PLETHEM UI
#' @description This a common function used to update the inputs for any PLETHEM User Interface. It is used by the apps to
#' provide a common pathways for all UI updates to happen
#' @param session The Shiny session in which all the inputs have to be updated
#' @param param_df The parameter dataframe. The parameter dataframe has the following columns
#' Name - The name of the parameter
#' Var - The variable in the UI representing this parameter
#' ParamType - The type of input, either Numeric, Radio, Checkbox, Select, Tabset. Used to identify which update function to call
#' Val - The value to be update with. If the type is numeric, the value is coerced to be a number.
#' @export
updateUIInputs <- function(session, param_df){
  by(param_df, 1:nrow(param_df),function(df_row){
    type <- df_row$ParamType
    varname <- df_row$Var
    val <- df_row$Val
    if (type == "Numeric"){
      var <- paste0("ms_",varname)
      val <- as.numeric(df_row$Val)
      updateNumericInput(session,var,value = val)
    } else if(type == "TabItem"){
      var <- paste0("ms_", varname)
      updateTabItems(session,var,selected = val)
    } else if(type == "Select"){
      var <- paste0("ms_",varname)
      updateSelectInput(session,var,selected = val)
    } else if(type == "Text"){
      var <- paste0("ms_",varname)
      updateTextInput(session,var,value = val)
    } else if (type == "Checkbox"){
      var <- paste0("ms_",varname)
      shinyWidgets::updateAwesomeCheckbox(session,var,value = as.logical(val))
    } else if (type == "CheckboxGroup"){
      var <- paste0("ms_",varname)
      updateCheckboxGroupInput(session,var,selected =eval(parse(text = val)))
    } else if (type == "RadioGroupButtons"){
      var <- paste0("ms_",varname)
      updateRadioGroupButtons(session,var,selected =val)
    }
    
  })

}

# Command line script to call IVIVE gadget.

#' run IVIVE gadget and save files as needed.
#' @description Run IVIVE gadget and save CSV files needed to create metabolism sets in the PBPK model
#' @return Values returned by the IVIVE gadget
#' @examples 
#' \dontrun{
#' doIVIVE()
#' }
#' @export
doIVIVE<- function(){
  path <- tcltk::tk_choose.dir()
  ret_vals <- iviveGadget(TRUE,path)
  return(ret_vals)
}

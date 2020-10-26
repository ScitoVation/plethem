
#' Call httk function for parameterizing PBPK models
#' @description Function that calls parameterize_pbpk function within the httk package
#' @param chem_name name of the chemical for which to parameterize the model. Has to be in the httk database.
#' @param species species for which to parameteize the model for. Defaults to human
#' @return list containing parameters for the PBPK model
#' @export
#' 
httkParameterPBTK<- function(chem_name,species = "Human"){
  params <- httk::parameterize_pbtk(chem.name = chem_name,species = species,default.to.human = TRUE)
  return(params)
}
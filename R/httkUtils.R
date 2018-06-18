#' @export
#' 
httkAddTable<- function(data2add,data_list,species= "Human"){
  chem.physical_and_invitro.data<- httk::add_chemtable(data2add,data_list,
                                                             chem.physical_and_invitro.data,
                                                             reference = "None",
                                                             species = species,
                                                             overwrite = T)
}

#' @export
#' 
httkParameterPBTK<- function(chem_name,species = "Human"){
  params <- parameterize_pbtk(chem.name = chem_name,species = species,default.to.human = T)
  return(params)
}
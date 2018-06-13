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
httkParameterPBTK<- function(data2add,data_list,chem_name,species = "Human"){
  temp_data <- add_chemtable(data2add,data_list,
                             chem.physical_and_invitro.data,
                             reference = "None",
                             species = species,
                             overwrite = T)
  unlockBinding("chem.physical_and_invitro.data",environment(add_chemtable))
  chem.physical_and_invitro.data<<-temp_data
  #lockBinding("chem.physical_and_invitro.data",environment(add_chemtable))
  #assignInNamespace("chem.physical_and_invitro.data",temp_data,envir = environment(add_chemtable))
  print(chem_name)

  print(chem_cas)
  params <- parameterize_pbtk(chem.name = chem_name,species = species,default.to.human = T)
}
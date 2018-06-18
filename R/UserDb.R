#' creates a new user databases or sets the path to the existing Db
#' @export


setUserDb<- function(){
  userDbPath <- file.choose()
  query <- sprintf("Update Utils Set value = '%s' Where variable = 'UserDbPath';",userDbPath)
  mainDbUpdate(query)
  if(!(file.exists(userDbPath))){
    
    file.copy(system.file("database/plethemUserDb.sqlite",package = "plethem"),userDbPath)
  }
  query <- "Select chemid from ChemicalSet;"
  result <- userDbSelect(query)
  print(sprintf("Selected User Database has %i chemicals",length(result$chemid)))
}
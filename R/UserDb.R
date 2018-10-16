#' Sets the path to the existing Db
#' @export
setUserDb<- function(){
  if ("rstudioapi" %in% installed.packages()){
    userDbPath <- rstudioapi::selectFile(caption = "Select User database",filter = "*.sqlite")
  }else{
    userDbPath <- tcltk::tk_choose.files(caption = "Select User database",multi = F,
                                         filters = "*.sqlite")
  }
  
  query <- sprintf("Update Utils Set value = '%s' Where variable = 'UserDbPath';",userDbPath)
  mainDbUpdate(query)
  
  query <- "Select chemid from ChemicalSet;"
  result <- userDbSelect(query)
  print(sprintf("Selected User Database has %i chemicals",length(result$chemid)))
}

#' Creates a new userDb based on the empty database in the package
#' @export
createUserDb <- function(){
  if("rstudioapi" %in% utils::installed.packages()){
    userDbPath <- rstudioapi::selectDirectory(caption = "Create a new user database")
  }else{
    userDbPath <- tcltk::tk_choose.dir(caption = "Create a new database")
  }
  
  file.copy(system.file("database/plethemUserDb.sqlite",package = "plethem"),userDbPath)
}

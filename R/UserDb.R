#' Sets the path to the existing Db
#' @description The function allows the users to set an existing database as the user database to use for plethem.
#' @param path path to the user database file. If no path is provided, it launches a file select dialog.
#' @examples
#' \dontrun{
#' setUserDb(),
#' setUserDb("C:/Users/Documents/PLETHEMUserDb.sqlite")
#' }
#' @export
setUserDb<- function(path = NULL){
  filters <- matrix(c("SQLite DB",".sqlite"),1,2,byrow = T)
  if (is.null(path)){
    userDbPath <- getFileFolderPath(type="file",
                      caption = "Select user database",
                      c("SQLite DB","*.sqlite"))
  }else{
    userDbPath <- path
  }
  query <- sprintf("Update Utils Set value = '%s' Where variable = 'UserDbPath';",userDbPath)
  mainDbUpdate(query)
  
  query <- "Select chemid from ChemicalSet;"
  result <- userDbSelect(query)
  print(sprintf("Selected User Database has %i chemicals",length(result$chemid)))
}

#' Creates a new userDb based on the empty database in the package
#' @description The function allows the users to create a new empty user database file that is needed to run plethem.
#' @param path path to where the user database needs to be stored. Make sure you have write permission to this folder. If no path is provided, it launches a folder select dialog.
#' @examples
#' if(interactive()){
#' currentDirectory <- getwd()
#' createEmptyUserDb(currentDirectory)
#' }
#' @export
createEmptyUserDb <- function(path = NULL){
  if(is.null(path)){
    userDbPath <- getFileFolderPath(type = "dir",caption = "Create a new database")
  }else{
    userDbPath <- path
  }
  
  file.copy(system.file("database/plethemUserDb.sqlite",package = "plethem"),userDbPath)
}

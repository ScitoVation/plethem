#' creates a new user databases or sets the path to the existing Db
#' @export


setUserDb<- function(){
  userDbPath <- choose.files(default = "plethemUserDb.sqlite",multi = F,
                              caption = "Select/Create user database")
  query <- sprintf("Update Utils Set value = '%s' Where variable = 'UserDbPath';",userDbPath)
  mainDbUpdate(query)
  if(!(file.exists(userDbPath))){
    
    file.copy(system.file("database/plethemUserDb.sqlite",package = "plethem"),userDbPath)
  }
}
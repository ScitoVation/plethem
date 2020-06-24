library(DBI)
library(RSQLite)
library(dplyr)

#' get the list of chemicals currently in the project database
#' @description Gets the chemicals that are currently a part of the project. This list comes from the chemical table in the
#'   project database. This function returns the chemical properties needed by all the models
#'   within PLETHEM. Chemical information specific to the model currently used is requested through a different function. This
#'   function can be used directly by the user to get a list of chemicals in the current project.
#' @return Returns a chemical list containing the names(chem_name), CAS numbers(CAS), Molecular Weight(MW),KM and Fraction unbound
#'  Unbound in Plasma (FuPls) for all the chemicals in the project
#' @export
getProjectChemicalList <- function(){
  chemset <- getAllSetChoices("chem")
  chem_names<- names(chemset)
  names(chemset)<- NULL
  chem_list <- lapply(chemset, function(x){
    query <- sprintf("Select param,value FROM Chemical Where chemid = %d",
                     as.integer(x))
    chem_vals <- projectDbSelect(query)
    chem_vals <- setNames(chem_vals$value,chem_vals$param)
    chem_vals["names"]<-chem_names[x]
    return(chem_vals)
  })

  return(chem_list)
}

#' Get the next Id for the table entry
#' @description Gets the next valid id for a given table.
#' This function is used internally to decide what id number should be used to save a parameter set
#' @param tble_name Name of the table for which the ID is required
#' @param db_path The location of the project database.
#' This defaults to database/project.sqlite and is not expected to change
#' @return int Integer ID of the next row in the table.
#' @export
getNextID <- function(tble_name, db_path ="database/project.sqlite"){
  id_name <- switch(tble_name,
                    "ExposureSet" = "expoid",
                    "ChemicalSet" = "chemid",
                    "PhysiologicalSet" = "physioid",
                    "AdmeSet"="admeid",
                    "BiomoniteringSet"="biomid",
                    "SimulationsSet" = "simid",
                    "ObservationSet" = "obsid",
                    "Variability"="varid")
  query <- sprintf("SELECT %s FROM %s ;",id_name,tble_name)
  id_list <- projectDbSelect(query)
  if (length(id_list[[id_name]])==0){
    id_num = 1
  }else{
    id_num = max(id_list[[id_name]])+1
  }
  return(id_num)
}


#' Read all the contents of a table
#' @description The functions reads all the contents of the given table.
#' It is used internally to save project data
#' @param tble_name Name of the table to save. NOTE SQLITE table names are not case sensitive
#' @param db_path The location of the project database. This defaults to database/project.sqlite and is not expected to change
#' @return table as a dataframe
projectReadTable <- function(tble_name, db_path ="database/project.sqlite"){
  conn <- getDbConn(db_path)
  ret_data <- RSQLite::dbReadTable(conn,tble_name)
  return(ret_data)
}

#' Write the dataframe to the table
#' @description The functions writes the datafarme to the sqlite table. It will overwrite any data
#' It is used internally to load project data
#' @param data Data frame containing the data to save to the table. This will overwrite any existing data
#' @param tble_name Name of the table to write the data to. NOTE SQLITE table names are not case sensitive
#' @param db_path The location of the project database. This defaults to database/project.sqlite and is not expected to change
#' @return None
projectWriteTable <- function(data, tble_name, db_path ="database/project.sqlite"){
  conn <- getDbConn(db_path)
  ret_data <- RSQLite::dbWriteTable(conn,tble_name,data, overwrite = T)
  RSQLite::dbDisconnect(conn)
}

#' Runs all select queries to the project database.
#' @description The function runs the select queries issued to the project db and returns the dataframe
#' @param query A valid SQL Query
#' @param db_path The location of the project database. This defaults to database/project.sqlite and is not expected to change
#' This function will not be called by the user directly
#' @export
projectDbSelect <- function(query, db_path ="database/project.sqlite"){
  conn <- getDbConn(db_path)
  res <- RSQLite::dbSendQuery(conn,query)
  res_df <- RSQLite::dbFetch(res)
  RSQLite::dbClearResult(res)
  return(res_df)

}

#' Runs all update queries to the project database.
#' @description The function runs the update queries issued to the project db
#' @param query A valid SQL Query
#' @param db_path The location of the project database. This defaults to database/project.sqlite and is not expected to change
#' This function will not be called by the user directly
#' @export
projectDbUpdate <- function(query, db_path ="database/project.sqlite"){
  conn <- getDbConn(db_path)
  RSQLite::dbExecute(conn,query)
  RSQLite::dbDisconnect(conn)
}

#' Runs all update queries to the main database.
#' @description The function runs the update queries issued to the main db
#' @param query A valid SQL Query
#' @param db_path The location of the main database. This defaults to database/plethemdb.sqlite and is not expected to change
#' This function will not be called by the user directly
#' @export
mainDbUpdate <- function(query, db_path ="database/plethemdb.sqlite"){
  conn <- getDbConn(db_path)
  RSQLite::dbExecute(conn,query)
  RSQLite::dbDisconnect(conn)
}

#' Runs all select queries to the main database.
#' @description The function runs the select queries issued to the main db and returns the dataframe
#' @param query A valid SQL Query
#' @param db_path The location of the project database. This defaults to database/plethemdb.sqlite and is not expected to change
#' This function will not be called by the user directly
#' @export
mainDbSelect <- function(query, db_path ="database/plethemdb.sqlite"){
  conn <- getDbConn(db_path)
  res <- RSQLite::dbSendQuery(conn,query)
  res_df <- RSQLite::dbFetch(res)
  RSQLite::dbClearResult(res)
  return(res_df)

}

#' Runs all select queries to the user database.
#' @description The function runs the select queries issued to the user db and returns the dataframe
#' the path to user database is stored in main plethem database and is selected from there
#' @param query A valid SQL Query
#' @export
userDbSelect <- function(query){
  # get user dbPath
  
  db_path <- mainDbSelect("Select value FROM Utils where variable = 'UserDbPath'")$value
  conn <- getDbConn(db_path,internal = F)
  res <- RSQLite::dbSendQuery(conn,query)
  res_df <- RSQLite::dbFetch(res)
  RSQLite::dbClearResult(res)
  return(res_df)

}
#' Runs all update queries to the user database.
#' @description The function runs the update queries issued to the user db
#' @param query A valid SQL Query
#' @export
userDbUpdate <- function(query){
  db_path <- mainDbSelect("Select value FROM Utils where variable = 'UserDbPath'")$value
  conn <- getDbConn(db_path,internal = F)
  RSQLite::dbExecute(conn,query)
  RSQLite::dbDisconnect(conn)
}

#' Runs all select queries an arbitrary database
#' @description The function runs the select queries issued to the user db and returns the dataframe
#' the path to user database is stored in main plethem database and is selected from there
#' @param query A valid SQL Query
#' @param db_path A valid path
#' @export
externDbSelect <- function(query,db_path){
  # get user dbPath
  
  
  conn <- getDbConn(db_path,internal = F)
  res <- RSQLite::dbSendQuery(conn,query)
  res_df <- RSQLite::dbFetch(res)
  RSQLite::dbClearResult(res)
  return(res_df)
  
}

#' Gets the connection to the Db to run all the queries against
#' @description The function returns the connection object to the database passed in DbPath
#' @param db_path The location of the project database.
#' @param internal Boolean. Is the database internal
#' This function will not be called by the user directly
#'
getDbConn<- function(db_path,internal = T){
  if (internal){
    db_path <- system.file(db_path,package = "plethem")
  }
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),db_path)
  return(conn)
}






# all functions to parse exposure datasets

#' Parse uploaed file for Consumer TRA
#' @description Parses the uploaded Consumer TRA exposure SpreadSheet to extract all the exposure names and values
#' This function will not be called directly by the user
#' @importFrom readxl excel_sheets read_excel
#' 
#' @param path Path to Excel File
#' 
parseTRAFile <- function(path){
  sheets<- readxl::excel_sheets(path)
  #parse inhalation data
  inh_data <- readxl::read_excel(path, sheet = sheets[grep("Inhalation",sheets)],skip = 2)
  inh_data[,c(1,2,4,16,17,18)]<- NULL
  idx <- c(1,which(is.na(inh_data[[2]]),arr.ind = T))
  inh_data <- inh_data[-idx,]
  colnames(inh_data)<-c("Exposure Name","Product Ingredient","Amount Per Application",
                        "Events per day","Fraction Released to Air",
                        "Dilution Fraction","Exposure Time (h)","Inhalation Rate(m^3/h)","CF",
                        "Room Volume","Body Weight(kg)","Exposure (mg/m^3)")
  inh_list <- unlist(inh_data[,1],use.names = F)
  ids <- paste0("inh",as.character(seq_along(inh_list)))
  inh_data$ids <- ids
  inh_list <- setNames(ids,inh_list)
  #parse oral data
  oral_data <- readxl::read_excel(path, sheet = sheets[grep("Oral",sheets)],skip = 2)
  oral_data[,c(1,2,4,13,14,15,16,17)]<- NULL
  idx <- c(1,which(is.na(oral_data[[2]]),arr.ind = T))
  oral_data <- oral_data[-idx,]
  colnames(oral_data)<-c("Exposure Name","Product Ingredient","Dosing Volume per Event (m^3)","TF",
                         "Events per day","Density","CF","Body Weight (kg)","Exposure (mg/kg/day)")
  oral_list <- unlist(oral_data[,1],use.names = F)
  ids <- paste0("oral",as.character(seq_along(oral_list)))
  oral_data$ids <- ids
  oral_list <- setNames(ids,oral_list)
  #parse dermal data
  dermal_data <- readxl::read_excel(path, sheet = sheets[grep("Dermal",sheets)],skip = 2)
  dermal_data[,c(1,2,4)]<- NULL
  idx <- c(1,which(is.na(dermal_data[[2]]),arr.ind = T))
  dermal_data <- dermal_data[-idx,]
  colnames(dermal_data)<-c("Exposure Name","Product Ingredient","Contact Area (cm^2)","TF",
                           "Events per day","Skin Thickness (cm)","Density","CF","Body Weight (kg)",
                           "Exposure (mg/kg/day)")
  dermal_list <- unlist(dermal_data[,1],use.names = F)
  ids <- paste0("dermal",as.character(seq_along(dermal_list)))
  dermal_data$ids <- ids
  dermal_list <- setNames(ids,dermal_list)
  return(list("inh" = inh_data,
              "oral"= oral_data,
              "dermal" = dermal_data,
              "exponames"= list("Inhalation" = inh_list,
                                "Oral" = oral_list,
                                "Dermal"=dermal_list)))
}
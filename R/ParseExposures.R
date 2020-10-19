# all functions to parse exposure datasets

#' Parse uploaded file for Consumer TRA
#' @description Parses the uploaded Consumer TRA exposure SpreadSheet to extract all the exposure names and values
#' This function will not be called directly by the user
#' @importFrom readxl excel_sheets read_excel
#' @import stringr
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
  inh_list <- unlist(inh_data[,1],use.names = FALSE)
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
  oral_list <- unlist(oral_data[,1],use.names = FALSE)
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
  dermal_list <- unlist(dermal_data[,1],use.names = FALSE)
  ids <- paste0("dermal",as.character(seq_along(dermal_list)))
  dermal_data$ids <- ids
  dermal_list <- setNames(ids,dermal_list)
  return(list("inh" = inh_data,
              "oral"= oral_data,
              #"dermal" = dermal_data,
              "exponames"= list("Inhalation" = inh_list,
                                "Oral" = oral_list)))#,
                                #"Dermal"=dermal_list)))
}

#' Parse uploaded file for consexpo
#' @description Parses the uploaded ConsExpo exposure SpreadSheet to extract all the exposure names and values
#' This function will not be called directly by the user
#' @importFrom readxl excel_sheets read_excel
#' 
#' @param path Path to Excel File
#' 
parseConsExpoFile <- function(path){
  print(path)
  f <- readLines(file(description = path),warn = FALSE,skipNul = T)
  f <- f[-which(f=="")]
  #f_frame <- setNames(f[c("value","Units")],f["Cat"])
  # Category Keywords to scan the file fow
  cat_keywords <- c("Substance","Product","Population","Scenario")
  subcat_keywords <- list("Substance"=c("Name","CASNumber","Molecular Weight","KOW"),
                          "Product"=c("Name","Weight Fraction Substance"),
                          "Population"=c("Name","Body weight"))
  
  # 
  names <- setNames(which(startsWith(f[,1],"Results for")),gsub("Results for scenario ","",f[which(startsWith(f[,1],"Results for")),1]))
  inhalation_data <- setNames(f[which(f[,1]=="Mean event concentration"),2],which(f[,1]=="Mean event concentration"))
  inhalation_data <- inhalation_data[inhalation_data != ""]
  dermal_data <- setNames(f[which(f[,1]=="Dermal load"),2],which(f[,1]=="Dermal load"))
  dermal_data <- dermal_data[dermal_data != ""]
  oral_data <- setNames(f[which(f[,1]=="Oral")+1,2],which(f[,1]=="Oral")+1)
  oral_data<- oral_data[oral_data != ""]
  return(list("exponames"=names,inhalation_data,dermal_data,oral_data))
  #return(f)
}
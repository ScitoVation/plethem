#' Function to create reverse dosimetry headers
#' @description ReverseDosimetryDataHeaders.csv
#' @param minValue
#' @param maxValue
#' @param stepSize
#' @return upperAndLowerBounds
#' @examples 
#' \dontrun{
#' bloodConcRange(-0.6,4.6,0.2)
#' }
#' @export
bloodConcRange <- function(minValue = -0.6, maxValue = 4.6, stepSize= 0.2){
  a <- c(0, 10^(seq(minValue, maxValue,by=stepSize)))
  
  upperAndLowerBounds <- data.frame(Larger.Than = a[-length(a)],
                                    Smaller.than.or.equal.to = c(a[-1]))
  
  return(upperAndLowerBounds)
}

#' Function to create reverse dosimetry headers
#' @description ReverseDosimetryDataHeaders.csv
#' @param minValue
#' @param maxValue
#' @param stepSize
#' @return upperAndLowerBounds
#' @export
bloodConcRange2 <- function(mcDF = data.frame()){
  minVal <- min(mcDF, na.rm = T)#min(mcDF[1])
  maxVal <- max(mcDF, na.rm = T)#max(mcDF[-1])
  numCol <- ncol(mcDF)
  bcMCResult2 <- mcDF
  incRate <- (maxVal/minVal)^(1/(numCol-1))
  a <- c(0, minVal)
  for (i in 1:(numCol-1)){
    a <- c(a, minVal*incRate^i)
  }
  upperAndLowerBounds <- data.frame(Larger.Than = a[-length(a)],
                                    Smaller.than.or.equal.to = c(a[-1]))
  return(upperAndLowerBounds)
}



#' Function to 
#' @description needed for probabilityTable
#' @param 
#' @param 
#' @return 
#' @export
frequencyData <- function(bounds, mcResult){
 
  resultFrequency <- data.frame(bloodConc1 = rep(NA, ncol(mcResult)))
  for (airconc in c(2:nrow(bounds))){
    resultFrequency <- eval(parse(text = paste0("cbind(resultFrequency,",
                             " data.frame(", paste0("bloodconc", airconc),
                                  " = rep(NA, ncol(mcResult))))")))
  }
  for (rowBounds in c(1:nrow(bounds))){
  for (testAirConcentration in c(1:ncol(mcResult))){
    resultFrequency[testAirConcentration, rowBounds] <- sum(mapply(isBetween,
    bounds[rowBounds,c("Larger.Than")],
    bounds[rowBounds,c("Smaller.than.or.equal.to")],
    mcResult[,testAirConcentration]))
  }} 
  
  return(resultFrequency)
  
}


#' Function to 
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
isBetween <- function(largerThan, smallerThanEqualTo, testValue){
  testValue > largerThan && testValue <= smallerThanEqualTo
}


#' Function to 
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
probabilityConc <- function(frequencyTable, weighingFactor = c()){
  if (length(weighingFactor)  == 0){
    tableOut <- sweep(frequencyTable,2,colSums(frequencyTable),'/')
  }
  else {
    tableOut <- sweep(frequencyTable,2, weighingFactor,'*')
  }
  ## Fix for columns with a sum of zero
  tableOut[is.na(tableOut)] <- 0
  return(tableOut)
}


#' Function to 
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
transformObs <- function(obsData) {
  log10(obsData)
} 


#' Function to 
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
measuredBloodRange <- function(MCbins, obsData){
  
  # get minimum and maxmimum value from the observed data
  minObs <- min(obsData)
  maxObs <- max(obsData)
  
  # first bin for observed data begins at 
  
  a <- c(minValue, firstMaxValue)
  while(max(a) < maxValue){
    a <- c(a, max(a)*stepSize)
  }
  
 upperAndLowerBounds <- data.frame(Larger.Than = a[-length(a)],
                                    Smaller.than.or.equal.to = c(a[-1]))
  
  return(upperAndLowerBounds)
}


#' Function to 
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
distributionData <- function(obsData, measuredRange){
  
  resultDist <- cbind(measuredRange, data.frame(frequencyCol = rep(NA, nrow(measuredRange)),
            percentCol = rep(NA, nrow(measuredRange))))
  for (rowBounds in c(1:nrow(measuredRange))){
    
      resultDist[rowBounds, c("frequencyCol")] <- sum(mapply(isBetween,
            measuredRange[rowBounds,c("Larger.Than")],
            measuredRange[rowBounds,c("Smaller.than.or.equal.to")],
            obsData[,1]))
  } 
  
  resultDist$percentCol <- resultDist$frequencyCol/sum(resultDist$frequencyCol)
  return(resultDist)
  
}


#' Function to 
#' @description 
#' @param
#' @param 
#' @return 
#' @export
cdf <- function(probabilityTable){
  cdfPert <- rowSums(probabilityTable)
  cdfPert <- data.frame(percentage = cdfPert,
                        cdf = rep(NA, length(cdfPert)))
  for (n in c(1:nrow(cdfPert))){
    cdfPert[n,c(2)] <- sum(cdfPert$percentage[1:n])
  }
  return(cdfPert)
  
}


#' bolean to check if a number is within a given range
isBetween <- function(largerThan, smallerThanEqualTo, testValue){
  testValue > largerThan && testValue <= smallerThanEqualTo
}

#' Create bins for concentrations from the monte carlo data
getMCConcBins <- function(mcDF = data.frame()){
  minVal <- min(mcDF, na.rm = T)
  maxVal <- max(mcDF, na.rm = T)
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

#' This function bins the data based and calcualtes frequency of data in each bin
getDataFrequency <- function(bounds, mcResult){
  
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

#'Function to calculate probability from the frequency table
#'@description The function calculates probability from the frequency table. If weighting factor is empty, 
#'the frequency table is assumed to be counts that are to be normalized. If a weighting factor array is provided,
#'the frequency table is assumed to be a fraction that needs to be scaled by the weighting factor.
getDataProbability <- function(frequencyTable, weightingFactor = c()){
  if (length(weightingFactor)  == 0){
    # divide each value by the sum of its respective column
    tableOut <- sweep(frequencyTable,2,colSums(frequencyTable),'/')
  }
  else {
    # mulitply the value by the weighting factor provided to get the probability
    tableOut <- sweep(frequencyTable,2, weightingFactor,'*')
  }
  ## Fix for columns with a sum of zero
  tableOut[is.na(tableOut)] <- 0
  return(tableOut)
}

#' Function to generate histogram of data
#' @description  The function returns a distribution for the observed data by using 
#' the bins generated using the monte carlo simulation data
getObsDataDistribution <- function(obsData, concBins){
  
  resultDist <- cbind(concBins, data.frame(frequencyCol = rep(NA, nrow(concBins)),
                                           percentCol = rep(NA, nrow(concBins))))
  for (rowBounds in c(1:nrow(concBins))){
    
    resultDist[rowBounds, c("frequencyCol")] <- sum(mapply(isBetween,
                                                           concBins[rowBounds,c("Larger.Than")],
                                                           concBins[rowBounds,c("Smaller.than.or.equal.to")],
                                                           obsData[,1]))
  } 
  
  resultDist$percentCol <- resultDist$frequencyCol/sum(resultDist$frequencyCol)
  
  return(resultDist)
  
}
#' calcualte CDF from the probability table
#' @description Converts a PDF to a CDF so it can be used for plotting and calculating percentile values
cdf <- function(probabilityTable){
  cdfPert <- rowSums(probabilityTable)
  cdfPert <- data.frame(percentage = cdfPert,
                        cdf = rep(NA, length(cdfPert)))
  for (n in c(1:nrow(cdfPert))){
    cdfPert[n,c(2)] <- sum(cdfPert$percentage[1:n])
  }
  return(cdfPert)
  
}

#' Calculate percentile values for exposure
#' @description Calulates the percentile value for a specific percentile when provided with the CDF and list of exposures for the CDF.
calcPercentileValue <- function(whichPercentile = 25,cdf,exposure_list){
  #x=cdf[,2]
  x <- cdf
  your.number = as.double(whichPercentile/100)
  if(your.number != 1){
    xIndex <- which(abs(x-your.number)==min(abs(x-your.number)))
    closestNum <- x[xIndex]
    if(closestNum > your.number) { # xIndex is higher than the percentile we're looking for
      lowerIndex <- xIndex - 1
      upperIndex <- xIndex
    } else{ #xIndex is lower than the percentile we're looking for
      lowerIndex <- xIndex
      upperIndex <- xIndex + 1
    }
  } else{
    upperIndex <- min(which(x == max(x))) -1 # This is -1 and lower is -2 in sample file.
    lowerIndex <- upperIndex - 2
  }
  rate <- (your.number-x[lowerIndex])/(x[upperIndex]-x[lowerIndex])
  # exposure_list <- as.numeric(lapply(as.character(exposure_list),function(x){
  #   gsub("[A-z]","",x)}))
  # print(coNames)
  
  exposure_value <- exposure_list[lowerIndex]+(rate*(exposure_list[upperIndex]-exposure_list[lowerIndex]))
  return(exposure_value)
}
#' Estimate exposure from montecarlo results and biomonitoring data
#' @description The function estimates exposures for the observed biomonitoring data using montecarlo simulation results over a large range of exposures.
#' The montecarlo results are obtained from a PBPK model. The biomonitoring results are obtained from a population level study.
#' The montecarlo results and biomonitoring data should have the same units and should be for the same physiological data source (eg: metabolite concentration in the urine).
#' @param mcData M by N data frame where M is the the individual exposures at which the PBPK model is run and N is the number of monte carlo runs at each exposure. It is recmmonded that M is between 25 and 40 and N is greater than 1000
#' @param biomData List consisting of biomonitering data. It is recommeded that atleast 1000 biomonitering values be provided to ensure accuracy for results
#' @param exposureBounds upper and lower bounds of the exposure for which the mcData was run
#' @param percentiles vector of percentiles for which exposure needs to be estimated. By default returns the 5th, 50th, 95th and 99th exposure estimate
#' @return List of values related to reverse dosimetry
#' \describe{
#' \item{cdf}{Cumulative Distribution function of the exposure estimate}
#' \item{pdf}{Probility distribution function of the exposure estimate}
#' \item{percentiles}{Data frame of percentiles and exposure estimates for the percentile}
#' 
#' }
#' 
#' @examples
#' \dontrun{
#' runReverseDosimetry(mcData,biomdata,exposureBounds = c(0.01,1.4), percentiles = c(5,50,95))
#' }
#' @importFrom pracma akimaInterp
#' @export
runReverseDosimetry <- function(mcData,biomData,exposureBounds,percentiles = c(5,50,95,99)){
  concBins <- getMCConcBins(mcData)
  mcFrequency <- getDataFrequency(concBins,mcData)
  mcProbability <- getDataProbability(mcFrequency)
  obsDataDist <- getObsDataDistribution(biomData,concBins)
  exposureProbability <- getDataProbability(mcProbability,
                                            obsDataDist$percentCol)
  exposureCDF <- cdf(exposureProbability)
  probability_vector <- exposureCDF$percentage
  cdf_vector <- exposureCDF$cdf
  # CDF and PDF are only calculated at doses at which they were measured
  # interpolate using splines to so that there are a total of 1000 doses
  # use this to get a more accurate exposure estimate
  extended_dose_list <- logseq(exposureBounds[[1]],exposureBounds[[2]],1000)
  interpCDF <- akimaInterp(dose_list,cdf_vector,extended_dose_list)
  interpPDF <- akimaInterp(dose_list,probability_vector,extended_dose_list)
  #estimate exposures at given percentile values
  exposures <- sapply(percentiles2Calculate,calcPercentileValue,
                      interpCDF,extended_dose_list)
  exposureDF <- data.frame("Percentile"=percentiles,"Exposure"=exposures)
  cdf <- data.frame("dose_list"=extended_dose_list,"cdf"=interpCDF)
  pdf <- data.frame("dose_list"=extended_dose_list,"pdf"=interpPDF)
  return("cdf"=cdf,"pdf"=pdf,"exposures"=exposureDF)
}



#' 
#' #' Function to create reverse dosimetry headers
#' #' @description ReverseDosimetryDataHeaders.csv
#' #' @param minValue
#' #' @param maxValue
#' #' @param stepSize
#' #' @return upperAndLowerBounds
#' #' @examples 
#' #' \dontrun{
#' #' bloodConcRange(-0.6,4.6,0.2)
#' #' }
#' bloodConcRange <- function(minValue = -0.6, maxValue = 4.6, stepSize= 0.2){
#'   a <- c(0, 10^(seq(minValue, maxValue,by=stepSize)))
#'   
#'   upperAndLowerBounds <- data.frame(Larger.Than = a[-length(a)],
#'                                     Smaller.than.or.equal.to = c(a[-1]))
#'   
#'   return(upperAndLowerBounds)
#' }
#' 
#' #' Function to create reverse dosimetry headers
#' #' @description ReverseDosimetryDataHeaders.csv
#' #' @param minValue
#' #' @param maxValue
#' #' @param stepSize
#' #' @return upperAndLowerBounds
#' #' @export
#' bloodConcRange2 <- function(mcDF = data.frame()){
#'   minVal <- min(mcDF, na.rm = T)#min(mcDF[1])
#'   maxVal <- max(mcDF, na.rm = T)#max(mcDF[-1])
#'   numCol <- ncol(mcDF)
#'   bcMCResult2 <- mcDF
#'   incRate <- (maxVal/minVal)^(1/(numCol-1))
#'   a <- c(0, minVal)
#'   for (i in 1:(numCol-1)){
#'     a <- c(a, minVal*incRate^i)
#'   }
#'   upperAndLowerBounds <- data.frame(Larger.Than = a[-length(a)],
#'                                     Smaller.than.or.equal.to = c(a[-1]))
#'   return(upperAndLowerBounds)
#' }
#' 
#' 
#' 
#' #' Function to 
#' #' @description needed for probabilityTable
#' #' @param 
#' #' @param 
#' #' @return 
#' #' @export
#' frequencyData <- function(bounds, mcResult){
#'  
#'   resultFrequency <- data.frame(bloodConc1 = rep(NA, ncol(mcResult)))
#'   for (airconc in c(2:nrow(bounds))){
#'     resultFrequency <- eval(parse(text = paste0("cbind(resultFrequency,",
#'                              " data.frame(", paste0("bloodconc", airconc),
#'                                   " = rep(NA, ncol(mcResult))))")))
#'   }
#'   for (rowBounds in c(1:nrow(bounds))){
#'   for (testAirConcentration in c(1:ncol(mcResult))){
#'     resultFrequency[testAirConcentration, rowBounds] <- sum(mapply(isBetween,
#'     bounds[rowBounds,c("Larger.Than")],
#'     bounds[rowBounds,c("Smaller.than.or.equal.to")],
#'     mcResult[,testAirConcentration]))
#'   }} 
#'   
#'   return(resultFrequency)
#'   
#' }
#' 
#' 
#' #' Function to 
#' #' @description 
#' #' @param 
#' #' @param 
#' #' @return 
#' #' @export
#' isBetween <- function(largerThan, smallerThanEqualTo, testValue){
#'   testValue > largerThan && testValue <= smallerThanEqualTo
#' }
#' 
#' 
#' #' Function to 
#' #' @description 
#' #' @param 
#' #' @param 
#' #' @return 
#' #' @export
#' probabilityConc <- function(frequencyTable, weighingFactor = c()){
#'   if (length(weighingFactor)  == 0){
#'     tableOut <- sweep(frequencyTable,2,colSums(frequencyTable),'/')
#'   }
#'   else {
#'     tableOut <- sweep(frequencyTable,2, weighingFactor,'*')
#'   }
#'   ## Fix for columns with a sum of zero
#'   tableOut[is.na(tableOut)] <- 0
#'   return(tableOut)
#' }
#' 
#' 
#' #' Function to 
#' #' @description 
#' #' @param 
#' #' @param 
#' #' @return 
#' #' @export
#' transformObs <- function(obsData) {
#'   log10(obsData)
#' } 
#' 
#' 
#' #' Function to 
#' #' @description 
#' #' @param 
#' #' @param 
#' #' @return 
#' #' @export
#' measuredBloodRange <- function(MCbins, obsData){
#'   
#'   # get minimum and maxmimum value from the observed data
#'   minObs <- min(obsData)
#'   maxObs <- max(obsData)
#'   
#'   # first bin for observed data begins at 
#'   
#'   a <- c(minValue, firstMaxValue)
#'   while(max(a) < maxValue){
#'     a <- c(a, max(a)*stepSize)
#'   }
#'   
#'  upperAndLowerBounds <- data.frame(Larger.Than = a[-length(a)],
#'                                     Smaller.than.or.equal.to = c(a[-1]))
#'   
#'   return(upperAndLowerBounds)
#' }
#' 
#' 
#' #' Function to 
#' #' @description 
#' #' @param 
#' #' @param 
#' #' @return 
#' #' @export
#' distributionData <- function(obsData, measuredRange){
#'   
#'   resultDist <- cbind(measuredRange, data.frame(frequencyCol = rep(NA, nrow(measuredRange)),
#'             percentCol = rep(NA, nrow(measuredRange))))
#'   for (rowBounds in c(1:nrow(measuredRange))){
#'     
#'       resultDist[rowBounds, c("frequencyCol")] <- sum(mapply(isBetween,
#'             measuredRange[rowBounds,c("Larger.Than")],
#'             measuredRange[rowBounds,c("Smaller.than.or.equal.to")],
#'             obsData[,1]))
#'   } 
#'   
#'   resultDist$percentCol <- resultDist$frequencyCol/sum(resultDist$frequencyCol)
#'   return(resultDist)
#'   
#' }
#' 
#' 
#' #' Function to 
#' #' @description 
#' #' @param
#' #' @param 
#' #' @return 
#' #' @export
#' cdf <- function(probabilityTable){
#'   cdfPert <- rowSums(probabilityTable)
#'   cdfPert <- data.frame(percentage = cdfPert,
#'                         cdf = rep(NA, length(cdfPert)))
#'   for (n in c(1:nrow(cdfPert))){
#'     cdfPert[n,c(2)] <- sum(cdfPert$percentage[1:n])
#'   }
#'   return(cdfPert)
#'   
#' }


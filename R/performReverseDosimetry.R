# bolean to check if a number is within a given range
isBetween <- function(largerThan, smallerThanEqualTo, testValue){
  testValue > largerThan && testValue <= smallerThanEqualTo
}

# Create bins for concentrations from the monte carlo data
getMCConcBins <- function(mcDF = data.frame()){
  minVal <- min(mcDF, na.rm = TRUE)
  maxVal <- max(mcDF, na.rm = TRUE)
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

# Calculate data frequency 
# @description This function bins the data based and calcualtes frequency of data in each bin 
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

#Function to calculate probability from the frequency table
#@description The function calculates probability from the frequency table. If weighting factor is empty, 
#the frequency table is assumed to be counts that are to be normalized. If a weighting factor array is provided,
#the frequency table is assumed to be a fraction that needs to be scaled by the weighting factor.
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

# Function to generate histogram of data
# @description  The function returns a distribution for the observed data by using 
# the bins generated using the monte carlo simulation data
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
# calcualte CDF from the probability table
# @description Converts a PDF to a CDF so it can be used for plotting and calculating percentile values
cdf <- function(probabilityTable){
  cdfPert <- rowSums(probabilityTable)
  cdfPert <- data.frame(percentage = cdfPert,
                        cdf = rep(NA, length(cdfPert)))
  for (n in c(1:nrow(cdfPert))){
    cdfPert[n,c(2)] <- sum(cdfPert$percentage[1:n])
  }
  return(cdfPert)
  
}

# Calculate percentile values for exposure
# @description Calulates the percentile value for a specific percentile when provided with the CDF and list of exposures for the CDF.
calcPercentileValue <- function(whichPercentile = 25,cdf,exposure_list){
  #x=cdf[,2]
  your.number = as.double(whichPercentile/100)
  if(your.number != 1){
    xIndex <- which(abs(cdf-your.number)==min(abs(cdf-your.number)))
    closestNum <- cdf[xIndex]
    if(closestNum[1] > your.number) { # xIndex is higher than the percentile we're looking for
      lowerIndex <- xIndex - 1
      upperIndex <- xIndex
    } else{ #xIndex is lower than the percentile we're looking for
      lowerIndex <- xIndex
      upperIndex <- xIndex + 1
    }
  } else{
    upperIndex <- min(which(cdf == max(cdf))) -1 # This is -1 and lower is -2 in sample file.
    lowerIndex <- upperIndex - 2
  }
  rate <- (your.number-cdf[lowerIndex])/(cdf[upperIndex]-cdf[lowerIndex])
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
#' @param mcData M by N data frame where M is the the individual exposures at which the PBPK model is run and N is the number of monte carlo runs at each exposure. It is recommended that M is between 25 and 40 and N is greater than 1000.
#' @param biomData List consisting of biomonitoring data. It is recommended that atleast 1000 biomonitering values be provided to ensure accuracy for results.
#' @param percentiles Vector of percentiles for which exposure needs to be estimated. By default returns the 5th, 50th, 95th and 99th exposure estimate.
#' @param dose_list A list of M elements that contain exposures at which monte carlo simulations were run. If no list is provided, the first column names of the mcData input are assumed to contain exposure values.
#' @return List of values related to reverse dosimetry
#' \describe{
#' \item{cdf}{Cumulative Distribution function of the exposure estimate}
#' \item{pdf}{Probability distribution function of the exposure estimate}
#' \item{percentiles}{Data frame of percentiles and exposure estimates for the percentile}
#' 
#' }
#' 
#' @examples
#' pD <- c(seq(.05,.90,.05),.94,.95,.96,.97,.98,.99,1.00)
#' dosing <- c(0.010, 0.011, 0.012, 0.014, 0.016, 0.018, 0.021, 0.023, 0.027, 0.030, 0.034, 0.039)
#' dosing <- c(dosing, 0.044, 0.050, 0.057, 0.065, 0.073, 0.083, 0.094, 0.107, 0.121, 0.137)
#' dosing <- c(dosing, 0.155, 0.176, 0.200)
#' runReverseDosimetry(mcDataExample, biomDataExample, percentiles = pD, dose_list = dosing)
#' @importFrom pracma akimaInterp logseq
#' @export
runReverseDosimetry <- function(mcData,biomData,percentiles=c(5,10,25,50,75,95,99,100),dose_list=NULL){
  # if a list of doses is not provided get them from colnames of the mc data file
  # this is generally used when passing MC data generated outside of plethem 
  # to the reverse dosimetry workflow
  if (is.null(dose_list)){
    dose_list <- as.numeric(lapply(as.character(colnames(mcData)),
                                   function(x){
                                     gsub("[A-z]","",x)
                                   }
    )
    )
  }
  exposureBounds <- c(max(dose_list),min(dose_list))
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
  extended_dose_list <- pracma::logseq(exposureBounds[[1]],exposureBounds[[2]],1000)
  interpCDF <- pracma::akimaInterp(dose_list,cdf_vector,extended_dose_list)
  interpPDF <- pracma::akimaInterp(dose_list,probability_vector,extended_dose_list)
  #estimate exposures at given percentile values
  exposures <- tryCatch({
    sapply(percentiles,calcPercentileValue,
           cdf_vector,dose_list)
  },
  error = function(e){
    message("Issues with Calculating percentiles. Returning null exposure values instead")
    return(replicate(length(percentiles),0,simplify = TRUE))
  })
  exposureDF <- data.frame("Percentile"=percentiles,"Exposure"=exposures)
  cdf <- data.frame("dose_list"=extended_dose_list,"cdf"=interpCDF)
  pdf <- data.frame("dose_list"=extended_dose_list,"pdf"=interpPDF)
  return(list("cdf"=cdf,"pdf"=pdf,"expoEstimates"=exposureDF))
}
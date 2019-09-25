# source("R_for_RD_Final.R")
# 
# mcResult <- read.csv("MCResults.csv")
# 
# bounds <- bloodConcRange()
# frequencyTable <- frequencyData(bounds, mcResult)
# probabilityTable <- probabilityConc(frequencyTable)
# obsData <- read.csv("Biomonitoring.csv")
# logtrans <- transformObs(obsData)
# measuredRange <- measuredBloodRange(maxValue = 2500)
# dist1 <- distributionData(obsData,measuredRange)
# # plot(x = log10(dist1$Smaller.than.or.equal.to), y = dist1$percentCol)
# dist2 <- distributionData(obsData,bounds)
# probabilityTable2 <- probabilityConc(probabilityTable, dist2$percentCol)
# cumulative <- rowSums(probabilityTable2)
# cdfResult <- cdf(probabilityTable2)
# # mcResultPlot <- read.csv("MCResults.csv",header = FALSE)
# # plot(x = as.numeric(log10(mcResultPlot[1,])), y = cdfResult$cdf, xlab= "Air Conc (ppb)",
# #      ylab = "cumulative density function" )
# 
# findPercentile <- function(whichPercentile = 25){ #cdfResults = cdfResult, whichPercentile, pdfCDF = pdfAndCDF){
#   x=cdfResult[,2]#c(1:100)
#   # your.number=0.25
#   your.number = as.double(whichPercentile/100)
#   if(your.number != 1){
#     xIndex <- which(abs(x-your.number)==min(abs(x-your.number)))
#     closestNum <- x[xIndex]
#     if(closestNum > your.number) {# xIndex is higher than the percentile we're looking for
#       lowerIndex <- xIndex - 1
#       upperIndex <- xIndex
#     } else{ #xIndex is lower than the percentile we're looking for
#       lowerIndex <- xIndex
#       upperIndex <- xIndex + 1
#     }
#   } else{
#     upperIndex <- min(which(x == 1)) # This is -1 and lower is -2 in sample file.
#     lowerIndex <- upperIndex - 1
#   }
#   rate <- (your.number-x[lowerIndex])/(x[upperIndex]-x[lowerIndex])
#   concNames <- pdfAndCDF[[1]]
#   coNames <- as.numeric(as.character(concNames))
#   cdfValue <- coNames[lowerIndex]+(rate*(coNames[upperIndex]-coNames[lowerIndex]))
#   return(cdfValue)
# }
# 
# percentileList <- c(5, 25, 50, 75, 90, 95, 99, 100)
# percentileResults <- sapply(percentileList, findPercentile)
# percentileDF <- data.frame(list('Percentile' = percentileList, 'Concentration' = percentileResults))
# 
# #######################################################################################################
# # 
# # 
# # print(findPercentile(100))
# # 
# # percentileList <- list(5, 25, 50, 75, 90, 95, 99, 100)
# # percentileResults <- sapply(percentileList, findPercentile)
# # percentileDF <- data.frame(list('Percentile' = percentileList, 'Concentration' = percentileResults))
# # # percentileDF <- data.frame(matrix(list(percentileList, percentileResults)))
# # # perDF2 <- data.frame(matrix(unlist(percentileList), nrow=8, byrow=T),stringsAsFactors=FALSE)
# # 
# # x=cdfResult[,2]#c(1:100)
# # # your.number=0.25
# # your.number = whichPercentile/100
# # xIndex <- which(abs(x-your.number)==min(abs(x-your.number)))
# # closestNum <- x[xIndex]
# # if(closestNum > your.number) {# xIndex is higher than the percentile we're looking for
# #   lowerIndex <- xIndex - 1
# #   upperIndex <- xIndex
# # } else{ #xIndex is lower than the percentile we're looking for
# #   lowerIndex <- xIndex
# #   upperIndex <- xIndex + 1
# # }
# # rate <- (your.number-x[lowerIndex])/(x[upperIndex]-x[lowerIndex])
# # concNames <- pdfAndCDF[[1]]
# # coNames <- as.numeric(as.character(concNames))
# # cdfValue <- coNames[lowerIndex]+(rate*(coNames[upperIndex]-coNames[lowerIndex]))
# # 
# # print(findPercentile(100))
# # percentileDF <- as.data.frame(list('Percentile' = c(5, 25, 50, 75, 90, 95, 99, 100)))
# # percentileDF <- percentileDF %>%
# #   mutate('Concentration' = findPercentile(Percentile))

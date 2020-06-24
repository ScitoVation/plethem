createGraphs <- function(oralModel1file = "../Batch File In Vivo Model 1 With Transit Constant.csv",
                         oralModel3file = "../Batch File In Vivo Model 3 With Transit Constant.csv",
                         inputFolder = "../dataForACC061020plots/",
                         outputFolder = "../plotsFor061020/"){

oralModel3results <- runBatchMode(chemicals = oralModel3file, exposures =NULL, load_files = TRUE,  
                                  model = "rapidPBPK", oralmodel = 3,
                                  organism = "rat")
df3 <- as.data.frame(oralModel3results$cpls_df)

oralModel1results <- runBatchMode(chemicals = oralModel1file, exposures =NULL, load_files = TRUE,  
                                  model = "rapidPBPK", oralmodel = 1,
                                  organism = "rat")
df1 <- as.data.frame(oralModel1results$cpls_df)

toPlot <- read.csv(oralModel1file)


for (plotFile in c(1:length(toPlot$In.Vivo.File.Name))){
  if (toPlot$Route[plotFile] == "Oral"){
    outsideData <- read.csv(paste0(inputFolder, toPlot$In.Vivo.File.Name[plotFile]), sep = ",", header = TRUE)
    # 1. Open jpeg file
    jpeg(paste0(outputFolder, sub(".csv", "", toPlot$In.Vivo.File.Name[plotFile]),".jpg"), width = 800, height = 800)
    # 2. Create the plot
    minValues <- c(log10(c(df1[,(plotFile*2)], df3[,(plotFile*2)],outsideData[,2])),
                   log10(outsideData[,2] - outsideData$Lower.Error))
    maxValues <- c(log10(c(df1[,(plotFile*2)], df3[,(plotFile*2)],outsideData[,2])),
                   log10(outsideData[,2] + outsideData$Upper.Error))
    plot(y=log10(c(df1[,(plotFile*2)], df3[,(plotFile*2)],outsideData[,2])),
         x=c(df1$time, df3$time, outsideData[,1]),
         col = c(rep("blue", nrow(df1)), rep("red", nrow(df3)), rep("black", nrow(outsideData))), pch = 19,
         cex = c(rep(0, nrow(df1)), rep(0, nrow(df3)), rep(1, nrow(outsideData))),
         xlab = "Time hr", ylab = "Log 10 Plasma Concentration uM", main = sub(".csv", "", toPlot$In.Vivo.File.Name[plotFile]),
         ylim = c(min(minValues[abs(minValues) < Inf]),
                  max(maxValues[abs(maxValues) < Inf])))
    # Graph as Lines
    lines(df1$time, log10(df1[,(plotFile*2)]), xlim=range(df1$time), ylim=range(log10(df1[,(plotFile*2)])), pch=16, col = "blue")
    lines(df3$time, log10(df3[,(plotFile*2)]), xlim=range(df3$time), ylim=range(log10(df3[,(plotFile*2)])), pch=16, col = "red")
    # Error Bars
    arrows(y0 = log10(outsideData[,2] + outsideData$Upper.Error),
           x0 = c(outsideData[,1]),
           y1 = log10(outsideData[,2] - outsideData$Lower.Error),
           x1 = c(outsideData[,1]),
           length=0.05, angle=90, code=3)
    # Legend
    legend("right", inset=.02, title="Data",
           c("Oral Model 1","Oral Model 3","Literature Data"), fill=c("blue", "red", "black"), horiz=TRUE, cex=0.8)
    # 3. Close the file
    dev.off()}
  if (toPlot$Route[plotFile] == "IV"){
    outsideData <- read.csv(paste0("../dataForACC061020plots/", toPlot$In.Vivo.File.Name[plotFile]), sep = ",", header = TRUE)
    # 1. Open jpeg file
    jpeg(paste0("../plotsFor061020/", sub(".csv", "", toPlot$In.Vivo.File.Name[plotFile]),".jpg"), width = 800, height = 800)
    # 2. Create the plot
    minValues <- c(log10(df1[,(plotFile*2)]),
                   log10(outsideData[,2] - outsideData$Lower.Error))
    maxValues <- c(log10(df1[,(plotFile*2)]),
                   log10(outsideData[,2] + outsideData$Upper.Error))
    plot(y=log10(c(df1[,(plotFile*2)],outsideData[,2])),
         x=c(df1$time, outsideData[,1]),
         col = c(rep("blue", nrow(df1)), rep("black", nrow(outsideData))), pch = 19,
         cex = c(rep(0, nrow(df1)), rep(1, nrow(outsideData))),
         xlab = "Time hr", ylab = "Log 10 Plasma Concentration uM", main = sub(".csv", "", toPlot$In.Vivo.File.Name[plotFile]),
         ylim = c(min(minValues[abs(minValues) < Inf]),
                  max(maxValues[abs(maxValues) < Inf])))
    # Graph as Lines
    lines(df1$time, log10(df1[,(plotFile*2)]), xlim=range(df1$time), ylim=range(log10(df1[,(plotFile*2)])), pch=16, col = "blue")
    # Error Bars
    arrows(y0 = log10(outsideData[,2] + outsideData$Upper.Error),
           x0 = c(outsideData[,1]),
           y1 = log10(outsideData[,2] - outsideData$Lower.Error),
           x1 = c(outsideData[,1]),
           length=0.05, angle=90, code=3)
    # Legend
    legend("right", inset=.02, title="Data",
           c("IV Model","Literature Data"), fill=c("blue", "black"), horiz=TRUE, cex=0.8)
    # 3. Close the file
    dev.off()}
}
}
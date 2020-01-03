library(ggplot2)
getDistProbability <- function(milkfat,milkfatColor,binDistance=25,minBin=20) {
  
  # Importing distCorrelation from distCorrelation.R's exported .csv files
  distCorrelation <- read.csv(paste("visualizations/distCorrelation/savedCorrelation/", milkfat, ".csv", sep=""))
  
  # Populating distCorrelation with bins based on binDistance
  distProbability <- data.frame(NA,NA,NA)
  colnames(distProbability) <- c("distance","probability","n")
  binNum <- floor(max(distCorrelation$distance)/binDistance)
  for (i in 1:binNum) {
    lowBound <- (i-1)*binDistance
    highBound <- i*binDistance
    currBin <- distCorrelation[(distCorrelation$distance > lowBound) & (distCorrelation$distance < highBound),]
    currRow <- c(mean(currBin$distance),mean(currBin$same),nrow(currBin))
    distProbability <- rbind(distProbability,currRow)
  }
  distProbability <- subset(distProbability, !is.nan(distance))
  distProbability <- subset(distProbability, !is.na(distance))
  
  # Combining small bins into larger ones (the last one might still be under minBin number)
  smallBins <- subset(distProbability, n<minBin) # smallBins are bins under minBin number
  distProbability <- subset(distProbability, n>=minBin)
  bigBins <- data.frame(0,0,0)
  colnames(bigBins) <- c("distance","probability","n")
  while (nrow(smallBins)!=0) { # Until there's nothing left in smallBins
    currBin <- data.frame(0,0,0)
    colnames(currBin) <- c("distance","probability","n")
    while (sum(currBin$n)<minBin && nrow(smallBins)!=0) { # Until the current bin has sufficient size
      currBin <- rbind(currBin,smallBins[1,])
      smallBins <- smallBins[-1,]
    }
    currBin <- subset(currBin,n!=0)
    bigBins <- rbind(bigBins,c(mean(currBin$distance),mean(currBin$probability),sum(currBin$n)))
  }
  bigBins <- subset(bigBins,n!=0)
  distProbability <- rbind(distProbability,bigBins)
  
  # Exporting the dataframes
  write.csv(distProbability, paste("visualizations/distCorrelation/savedProbability/", milkfat, ".csv", sep=""), row.names=FALSE)
  distProbability <<- distProbability
  
  # Graphing
  sameProbability <- mean(distCorrelation$same)*100
  distProbability$probability <- distProbability$probability*100
  distProbabilityPlot <<- ggplot(distProbability, aes(x=distance, y=probability)) + # Call distProbabilityPlot globally for the ggPlot (can't do it from inside function?)
    geom_point(colour = milkfatColor, size=3) + 
    geom_hline(yintercept=sameProbability, size=1.5, colour="gray50", linetype="dashed") +
    geom_smooth(method="lm", colour="black") +
    ylab("") +
    xlab("")
  lm_fit <- lm(distance ~ probability, data=distProbability)
  summary(lm_fit)
}

getDistProbability("skim","hotpink3")
distProbabilityPlot


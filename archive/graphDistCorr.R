library(ggplot2)

distCorrWhole <- read.csv("distanceCorr/wholeDistCorrBin.csv")
distCorrReduced <- read.csv("distanceCorr/reducedDistCorrBin.csv")
distCorrLow <- read.csv("distanceCorr/lowDistCorrBin.csv")
distCorrSkim <- read.csv("distanceCorr/skimDistCorrBin.csv")

distCorrWhole$probability <- distCorrWhole$probability*100
distCorrReduced$probability <- distCorrReduced$probability*100
distCorrLow$probability <- distCorrLow$probability*100
distCorrSkim$probability <- distCorrSkim$probability*100

sameProbWhole <- mean(distCorrWhole$probability)
sameProbReduced <- mean(distCorrReduced$probability)
sameProbLow <- mean(distCorrLow$probability)
sameProbSkim <- mean(distCorrSkim$probability)

ggplot(data=NULL, aes(x=distance, y=probability)) + 
  geom_point(data=distCorrWhole,colour="red") +
  geom_hline(yintercept=sameProbWhole, size=1, colour="red", linetype="dashed") +
  geom_point(data=distCorrReduced,colour="blue") + 
  geom_hline(yintercept=sameProbReduced, size=1, colour="blue", linetype="dashed") +
  geom_point(data=distCorrLow,colour="orange") + 
  geom_hline(yintercept=sameProbLow, size=1, colour="orange", linetype="dashed") +
  geom_point(data=distCorrSkim,colour="black") +
  geom_hline(yintercept=sameProbSkim, size=1, colour="black", linetype="dashed") +
  ggtitle(paste("Distance dependency of standardization")) +
  #ylim(c(0,50)) +
  ylab("Probability of Sameness (%)") +
  xlab("Distance (km)")

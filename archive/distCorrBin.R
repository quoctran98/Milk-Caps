milkfat <- "skim"
milkfatColor <- "orange"
binSize <- 100 #even number pls (actual bin is 1 larger)

distRaw <- read.csv(paste("visualizations/distCorrelation/raw/", milkfat, ".csv", sep=""))

#generates distCorr DF with distance, same, and proportion
distCorr <- cbind(distRaw,NA)
colnames(distCorr) <- c("distance","same","proportion")
distCorr <- subset(distCorr, distance != 0)
distCorr <- distCorr[order(distCorr$distance), ]

#fills "proportion" in distCorr but with bins?
for (row in 1:nrow(distCorr)) {
  print(row)
  lowerBound <- row-(binSize/2)
  if (lowerBound <= 0) {
    lowerBound <- 1
  }
  higherBound <- row+(binSize/2)
  if (higherBound > nrow(distCorr)) {
    higherBound <- nrow(distCorr)
  }
  distCorr[row,"proportion"] <-  mean(distCorr[lowerBound:higherBound,"same"])
}

write.csv(distCorr, paste("distCorr/", milkfat, "DistCorr.csv", sep=""), row.names = FALSE)
distCorr <- read.csv(paste("distCorr/", milkfat, "DistCorr.csv", sep=""))

sameProbability <- mean(distRaw$same)*100
distCorr$proportion <- distCorr$proportion*100

ggplot(distCorr, aes(x=distance, y=proportion)) + 
  geom_point(colour = milkfatColor) + 
  geom_hline(yintercept=sameProbability, size=1, colour="gray50") +
  ggtitle(paste("Distance dependency of standardization for",milkfat,"fat milk")) +
  ylab("Probability of Sameness (%)") +
  geom_smooth(method="lm", colour="black") +
  xlab("Distance (km)") +
  theme_gray()

lm_fit <- lm(distance ~ probability, data=distCorr)
summary(lm_fit)

library(zipcode)
data("zipcode")
source("functions.R")
source("rawData/dataCleanup.R")

getDistCorrelation <- function (milkfat) {
  # Filtering the dataset
  colorReference <- read.csv("otherData/colorReference.csv", header=FALSE)
  responses <- merge(responses,zipcode, by="zip")
  responses <- subset(responses, (responses[,milkfat] %in% colorReference$V1))
  
  distCorrelation <- data.frame(c(0), c(1))
  colnames(distCorrelation) <- c("distance","same")
  
  # Finds Haversine distance between every two caps
  numMax <- (nrow(responses)^2)
  for (cap1 in 1:(nrow(responses)-1)) { # -1 because the last row doesn't need to be calculated
    cap1row <- responses[cap1,]
    for (cap2 in (cap1+1):nrow(responses)) { # Prevents repeat calculations
      print(((((cap1-1)*nrow(responses))+cap2)/numMax)*100) # Reports the percentage complete
      cap2row <- responses[cap2,]
      if (cap1row["brand"] != cap2row["brand"]) { # Doesn't count if the two milk caps are from the same brand
        distance <- haversineDist(as.numeric(cap1row["latitude"]),as.numeric(cap1row["longitude"]),as.numeric(cap2row["latitude"]),as.numeric(cap2row["longitude"])) # Haversine distance function in functions.R
        distCorrelation <- rbind(distCorrelation, c(distance, as.numeric(cap1row[milkfat] == cap2row[milkfat])))
      }
    }
  }
  distCorrelation <- subset(distCorrelation,!is.na(distance))
  distCorrelation <- distCorrelation[order(distCorrelation$distance),]
  rownames(distCorrelation) <- 1:nrow(distCorrelation)
  
  # Exporting the dataframes
  distCorrelation <<- distCorrelation
  write.csv(distCorrelation, paste("visualizations/distCorrelation/distCorrelation/", milkfat, ".csv", sep=""), row.names=FALSE)
}

getDistCorrelation("skim")

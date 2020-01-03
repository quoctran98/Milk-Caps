library(stringr)
library(dplyr)
library(ggplot2)
library(zipcode)
source("rawData/dataCleanup.R")
data("zipcode")
responses <- merge(responses,zipcode, by="zip")
rm(zipcode)

# Calculation of standardization
# Argument 'milkcaps' is vector of milk cap colors as a vector
calcStandard <- function (milkcaps) {
  milkcaps <- as.character(milkcaps)
  milkcaps <- milkcaps[!is.na(milkcaps)]
  uniqueMilkcaps <- unique(milkcaps)
  milkcapCount <- c()
  # Count of each milk cap color type
  for (each in c(1:length(uniqueMilkcaps))) {
    currCount <- sum(milkcaps == uniqueMilkcaps[each])
    milkcapCount <- c(milkcapCount, currCount)
  }
  # Statistical noise = standard deviation / mean
  return(sd(milkcapCount)/mean(milkcapCount))
}

# Milk production for each US state from USDA ERS data for 2018
milkProduction <- read.csv("otherData/milkProduction.csv", header=FALSE)
colnames(milkProduction) <- c("stateFull","total")
milkProduction <- subset(milkProduction, total != "")
milkProduction$total <- str_replace_all(milkProduction$total, fixed(","), "")
milkProduction$total <- as.numeric(milkProduction$total)

# The proportion of milk production for each milkfat from USDA ERS data for 2018
# This assumes that these proportions are constant for every region
milkProduction$whole <- milkProduction$total*(16040/47672)
milkProduction$reduced <- milkProduction$total*(15721/47672)
milkProduction$low <- milkProduction$total*(6534/47672)
milkProduction$skim <- milkProduction$total*(4007/47672)

# Adding abbreviations to milk production dataframe
stateAbbr <- read.csv("otherData/abbreviations.csv", header=TRUE)
colnames(stateAbbr) <- c("stateFull","abbrev","state")
milkProduction <- merge(milkProduction, stateAbbr)
rm(stateAbbr)

# Creating allStandard and allDemand matrices
allStates <- unique(responses$state)
allStandard <- (matrix(ncol=4, nrow=length(allStates)))
colnames(allStandard) <- c("whole","reduced","low","skim")
rownames(allStandard) <- allStates
allDemand <- allStandard

# Filling allStandard by using calcStandard for each subset (milkfat and region)
for (state in allStates) {
  for (milkfat in c("whole","reduced","low","skim")) {
    allStandard[state,milkfat] <-  calcStandard(responses[responses$state == state, milkfat])
  }
}
rm(milkfat)
rm(state)
rm(allStates)

# Filling allDemand by summing milk production dataframe rows for each region
for (row in 1:nrow(allDemand)) {
  for (col in 1:ncol(allDemand)) {
    state <- rownames(allDemand)[row]
    milkfat <- colnames(allDemand)[col]
    
    allDemand[row, col] <- sum(milkProduction[milkProduction$state == state, milkfat])
  }
}
rm(row)
rm(col)

# Combining standard and demand matrices into a dataframe
combinedDF <- data.frame(NA,NA,NA,NA)
colnames(combinedDF) <- c("demand","standard","state","milkfat")
# For each cell in both matrices
for (row in 1:nrow(allDemand)) {
  for (col in 1:ncol(allDemand)) {
    demand <- (allDemand[row, col])
    standard <- (allStandard[row, col])
    state <- (rownames(allDemand)[row])
    milkfat <- (colnames(allDemand)[col])
    combinedDF <- rbind(combinedDF, c(demand,standard,state,milkfat))
  }
}
rm(demand)
rm(standard)
rm(state)
rm(milkfat)
rm(row)
rm(col)
combinedDF <- combinedDF[!is.na(combinedDF$standard),]
combinedDF <- combinedDF[combinedDF$standard != 0,]
#combinedDF <- combinedDF[combinedDF$milkfat == "whole",]
combinedDF$demand <- as.double(combinedDF$demand)
combinedDF$standard <- as.double(combinedDF$standard)
rm(allDemand)
rm(allStandard)

# Color reference for each milkfat
colRef <- data.frame(c("whole","reduced","low","skim"),c("red","blue","darkgreen","hotpink3"))
colnames(colRef) <- c("milkfat","color")
combinedDF <- merge(combinedDF,colRef)
rm(colRef)

# Plotting
number_ticks <- function(n) {function(limits) pretty(limits, n)}
ggplot(combinedDF, aes(x=demand, y=standard)) +
  geom_point(colour=combinedDF$color, size=3) +
  geom_smooth(method="lm") +
  xlab("Milk Production (Millions of Pounds)") +
  ylab("Standardization of Cap Colors (Noise)") +
  scale_x_continuous(breaks=number_ticks(7)) +
  coord_trans(x="sqrt")

lm_fit <- lm(standard ~ sqrt(demand), data=combinedDF)
summary(lm_fit)

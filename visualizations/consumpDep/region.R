library(stringr)
library(dplyr)
library(ggplot2)
source("rawData/dataCleanup.R")
load("./rawData/zipcode.rda") # from https://cran.r-project.org/src/contrib/Archive/zipcode/
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

# Adding region data to each response by State
responses <- cbind(responses,rep(nrow(responses)))
colnames(responses) <- c(colnames(responses)[1:ncol(responses)-1],"region")
responses$region <- 0

# Regions from the USDA Agricultural Research Service
# Can be edited easily and new regions can be added
responses[responses$state %in% c("ME","NH","VT","MA","RI","CT","NJ","PA","NY","DE","MD","VA","WV"),"region"] <- "Northeast"
responses[responses$state %in% c("TN","NC","SC","GA","FL","AL","MS","AR","LA"),"region"] <- "Southeast"
responses[responses$state %in% c("OH","IN","IL","MI","WI","MO","IA","MN","KY"),"region"] <- "Midwest"
responses[responses$state %in% c("TX","OK","NE","KS","SD","ND","MT","WY","CO","NM"),"region"] <- "Plains"
# Includes AK and HI
responses[responses$state %in% c("WA","OR","CA","NV","ID","UT","AZ","AK","HI"),"region"] <- "Pacific West"

# Adds region data to milk production dataframe (removes states that don't appear)
milkProduction <- merge(milkProduction, unique(responses[,c("state","region")]))

# Creating allStandard and allDemand matrices
allRegions <- unique(responses$region)
allStandard <- (matrix(ncol=4, nrow=length(allRegions)))
colnames(allStandard) <- c("whole","reduced","low","skim")
rownames(allStandard) <- allRegions
allDemand <- allStandard

# Filling allStandard by using calcStandard for each subset (milkfat and region)
for (region in allRegions) {
  for (milkfat in c("whole","reduced","low","skim")) {
    allStandard[region,milkfat] <-  calcStandard(responses[responses$region == region, milkfat])
  }
}
rm(milkfat)
rm(region)
rm(allRegions)

# Filling allDemand by summing milk production dataframe rows for each region
for (row in 1:nrow(allDemand)) {
  for (col in 1:ncol(allDemand)) {
    region <- rownames(allDemand)[row]
    milkfat <- colnames(allDemand)[col]
    
    allDemand[row, col] <- sum(milkProduction[milkProduction$region == region, milkfat])
  }
}
rm(row)
rm(col)

# Combining standard and demand matrices into a dataframe
combinedDF <- data.frame(NA,NA,NA,NA)
colnames(combinedDF) <- c("demand","standard","region","milkfat")
# For each cell in both matrices
for (row in 1:nrow(allDemand)) {
  for (col in 1:ncol(allDemand)) {
    demand <- (allDemand[row, col])
    standard <- (allStandard[row, col])
    region <- (rownames(allDemand)[row])
    milkfat <- (colnames(allDemand)[col])
    combinedDF <- rbind(combinedDF, c(demand,standard,region,milkfat))
  }
}
rm(demand)
rm(standard)
rm(region)
rm(milkfat)
rm(row)
rm(col)
combinedDF <- combinedDF[!is.na(combinedDF$demand),]
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
ggplot(combinedDF, aes(x=demand, y=standard)) +
  geom_point(colour=combinedDF$color, size=5) +
  geom_smooth(method="lm") +
  xlab("Milk Production (Millions of Pounds)") +
  ylab("Standardization of Cap Colors (Noise)")

lm_fit <- lm(demand ~ standard, data=combinedDF)
summary(lm_fit)

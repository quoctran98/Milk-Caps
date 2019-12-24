library(dplyr)
library(ggplot2)
library(zipcode)
source("rawData/dataCleanup.R")
data("zipcode")
responses <- merge(responses,zipcode, by="zip")
rm(zipcode)

calcVariance <- function (milkcaps) {
  milkcaps <- as.character(milkcaps)
  milkcaps <- milkcaps[!is.na(milkcaps)]
  uniqueMilkcaps <- unique(milkcaps)
  milkcapCount <- c()
  for (each in c(1:length(uniqueMilkcaps))) {
    currCount <- sum(milkcaps == uniqueMilkcaps[each])
    milkcapCount <- c(milkcapCount, currCount)
  }
  #Shannon <- milkcapCount/sum(milkcapCount)
  #Shannon <- Shannon*log(Shannon)
  #Shannon <- sum(Shannon)
  #Shannon <- -1*Shannon
  #return(Shannon)
  #return(max(milkcapCount)/sum(milkcapCount))
  return(sd(milkcapCount)/mean(milkcapCount))
  #return(sd(milkcapCount))
}

responsesNE <- subset(responses, state %in% c("ME","NH","VT","MA","RI","CT","NJ","PA","NY","DE","MD","VA","WV"))
responsesSE <- subset(responses, state %in% c("TN","NC","SC","GA","FL","AL","MS","AR","LA"))
responsesMW <- subset(responses, state %in% c("OH","IN","IL","MI","WI","MO","IA","MN","KY"))
responsesPL <- subset(responses, state %in% c("TX","OK","NE","KS","SD","ND","MT","WY","CO","NM"))
responsesPW <- subset(responses, state %in% c("WA","OR","CA","NV","ID","UT","AZ"))

allStandard <- data.frame(vector(mode="numeric", length=5),vector(mode="numeric", length=5),vector(mode="numeric", length=5),vector(mode="numeric", length=5))
colnames(allStandard) <- c("whole","reduced","low","skim")
rownames(allStandard) <- c("NE","SE","MW","PL","PW")
  
allDemand <- data.frame(vector(mode="numeric", length=5),vector(mode="numeric", length=5),vector(mode="numeric", length=5),vector(mode="numeric", length=5))
colnames(allDemand) <- c("whole","reduced","low","skim")
rownames(allDemand) <- c("NE","SE","MW","PL","PW")

#allDemand filled by milkDemand.R
source("milkDemand.R")

for (milkfat in c("whole","reduced","low","skim")) {
  allStandard["NE",milkfat] <- calcVariance(responsesNE[,milkfat])
  allStandard["SE",milkfat] <- calcVariance(responsesSE[,milkfat])
  allStandard["MW",milkfat] <- calcVariance(responsesMW[,milkfat])
  allStandard["PL",milkfat] <- calcVariance(responsesPL[,milkfat])
  allStandard["PW",milkfat] <- calcVariance(responsesPW[,milkfat])
}

combinedList <- cbind(NA,NA,NA,NA)
colnames(combinedList) <- c("demand","standard","region","milkfat")

rownames(allStandard) <- c("Northeast","Southeast","Midwest","Plains","Pacific West")
for (col in c(1:4)) {
  currList <- data.frame(vector(mode="numeric",length=5),vector(mode="numeric",length=5),vector(mode="character",length=5),vector(mode="character",length=5))
  colnames(currList) <- c("demand","standard","region","milkfat")
  currList$demand <- as.numeric(allDemand[,col])
  currList$standard <- as.numeric(allStandard[,col])
  currList$region <- rownames(allStandard)
  currList$milkfat <- c("whole","reduced","low","skim")[col]
  combinedList <- rbind(combinedList,currList)
}
combinedList <- combinedList[2:21,]

colRef <- data.frame(c("whole","reduced","low","skim"),c("red","blue","darkgreen","hotpink3"))
colnames(colRef) <- c("milkfat","color")

combinedList <- merge(combinedList,colRef)
#combinedList <- subset(combinedList, milkfat != "low")

ggplot(combinedList, aes(x=demand, y=standard)) +
  geom_point(colour=combinedList$color, size=5) +
  geom_smooth(method="lm") +
  #ggtitle("Milk demand drives cap color standardization") +
  xlab("Milk Production (Millions of Pounds)") +
  ylab("Standardization of Cap Colors (Noise)")

lm_fit <- lm(demand ~ standard, data=combinedList)
summary(lm_fit)

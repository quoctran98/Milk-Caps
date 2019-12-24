library(stringr)
milkProduction <- read.csv("otherData/milkProduction.csv", header=FALSE)
colnames(milkProduction) <- c("stateFull","total")
milkProduction <- subset(milkProduction, total != "")
milkProduction$total <- str_replace_all(milkProduction$total, fixed(","), "")
milkProduction$total <- as.numeric(milkProduction$total)

#Milkfat proportions from USDA ERS data for 2018
milkProduction$whole <- milkProduction$total*(16040/47672)
milkProduction$reduced <- milkProduction$total*(15721/47672)
milkProduction$low <- milkProduction$total*(6534/47672)
milkProduction$skim <- milkProduction$total*(4007/47672)

stateAbbr <- read.csv("otherData/abbreviations.csv", header=TRUE)
colnames(stateAbbr) <- c("stateFull","abbrev","state")

milkProduction <- merge(milkProduction, stateAbbr)

#allDemand from consDep.R
for (milkfat in c("whole","reduced","low","skim")) {
  allDemand["NE",milkfat] <- sum(milkProduction[milkProduction$state %in% c("ME","NH","VT","MA","RI","CT","NJ","PA","NY","DE","MD","VA","WV") ,milkfat])
  allDemand["SE",milkfat] <- sum(milkProduction[milkProduction$state %in% c("TN","NC","SC","GA","FL","AL","MS","AR","LA") ,milkfat])
  allDemand["MW",milkfat] <- sum(milkProduction[milkProduction$state %in% c("OH","IN","IL","MI","WI","MO","IA","MN","KY") ,milkfat])
  allDemand["PL",milkfat] <- sum(milkProduction[milkProduction$state %in% c("TX","OK","NE","KS","SD","ND","MT","WY","CO","NM") ,milkfat])
  allDemand["PW",milkfat] <- sum(milkProduction[milkProduction$state %in% c("WA","OR","CA","NV","ID","UT","AZ") ,milkfat])
}
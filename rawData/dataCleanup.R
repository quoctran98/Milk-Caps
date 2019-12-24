library(stringr)
rawResponses <- read.csv("rawData/rawResponses.csv", header=TRUE)

responses <- rawResponses
colnames(responses) <- c("time","zip","brand","whole","reduced","low","skim")

responses$brand <- toupper(responses$brand)
responses$brand <- str_replace_all(responses$brand, fixed("`"), "")
responses$brand <- str_replace_all(responses$brand, fixed("’"), "")
responses$brand <- str_replace_all(responses$brand, fixed("'"), "")
responses$brand <- str_replace_all(responses$brand, fixed("-"), "")
for (row in c(1:nrow(responses))) {
  responses[row,"brand"] <- strsplit(responses[row,"brand"]," ")[[1]][1]
  responses[row,"brand"] <- strsplit(responses[row,"brand"],"/")[[1]][1]
  responses[row,"brand"] <- strsplit(responses[row,"brand"],",")[[1]][1]
}
rm(row)

badResponses <- read.csv("rawData/badResponses.csv", header=TRUE)
responses <- responses[-badResponses$row,]

badBrands <- read.csv("rawData/badBrands.csv", header=TRUE)
for (wrongBrand in badBrands$wrong) {
  responses$brand[responses$brand == wrongBrand] <- as.character(unlist(badBrands[badBrands$wrong == wrongBrand,][2]))
  rm(wrongBrand)
}

colorReference <- read.csv("otherData/colorReference.csv", header=FALSE)
colnames(colorReference) <- c("whole","wholeHex")
responses <- merge(responses,colorReference, by="whole", all.x=TRUE)
colnames(colorReference) <- c("reduced","reducedHex")
responses <- merge(responses,colorReference, by="reduced", all.x=TRUE)
colnames(colorReference) <- c("low","lowHex")
responses <- merge(responses,colorReference, by="low", all.x=TRUE)
colnames(colorReference) <- c("skim","skimHex")
responses <- merge(responses,colorReference, by="skim", all.x=TRUE)
colnames(colorReference) <- c("color","hex")

row.names(responses) <- 1:nrow(responses)
responses <- responses[,c(5,6,7,4,8,3,9,2,10,1,11)]

allColors <- as.character(read.csv("otherData/colorReference.csv", header=FALSE)$V1)
for (row in 1:nrow(responses)) {
  if (!responses[row,"whole"] %in% allColors) {
    responses[row,"whole"] <- NA
  }
  if (!responses[row,"reduced"] %in% allColors) {
    responses[row,"reduced"] <- NA
  }
  if (!responses[row,"low"] %in% allColors) {
    responses[row,"low"] <- NA
  }
  if (!responses[row,"skim"] %in% allColors) {
    responses[row,"skim"] <- NA
  }
}
rm(row)
rm(allColors)

for(col in 1:ncol(responses)) {
  responses[,col] <- factor(as.character(responses[,col]))
}
rm(col)

rm(badBrands)
rm(badResponses)
rm(rawResponses)
rm(colorReference)

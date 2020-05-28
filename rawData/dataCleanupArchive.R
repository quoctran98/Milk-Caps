library(stringr)

# Importing responses
rawResponses <- read.csv("rawData/rawResponses.csv", header=TRUE)
responses <- rawResponses
colnames(responses) <- c("time","zip","brand","whole","reduced","low","skim")
rm(rawResponses)

# Cleaning up the brand names
responses$brandRaw <- responses$brand
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

# Removing the bad responses
badResponses <- read.csv("rawData/badResponses.csv", header=TRUE)
responses <- responses[-badResponses$row,]
rm(badResponses)

# Replacing the bad brands
badBrands <- read.csv("rawData/badBrands.csv", header=TRUE)
for (wrongBrand in badBrands$wrong) {
  responses$brand[responses$brand == wrongBrand] <- as.character(unlist(badBrands[badBrands$wrong == wrongBrand,][2]))
  rm(wrongBrand)
}
rm(badBrands)

# Adding hex of each color
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

# Rename rows and reorder 
row.names(responses) <- 1:nrow(responses)
responses <- responses[,c("time","zip","brand","brandRaw","whole","wholeHex","reduced","reducedHex","low","lowHex","skim","skimHex")]

# Removing unknown colors from the data
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
rm(colorReference)

# Refactoring each column
for(col in 1:ncol(responses)) {
  responses[,col] <- factor(as.character(responses[,col]))
}
rm(col)
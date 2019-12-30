source("rawData/dataCleanup.R")

getConsensus <- function (category, threshold = 0.7) {
  # Initializing dataframe
  brands <- data.frame(NA,NA,NA,NA)
  colnames(brands) <- c("brand","category","proportion","sample")
  allBrands <- as.character(unique(responses$brand))
  
  # Going through each unique brand
  for (brand in allBrands) {
    # Extract all the unique strings in each category that match the brand
    brands[match(brand,allBrands),"brand"] <- brand
    allStrings <- as.character(responses[responses$brand==brand,category])
    allStrings <- allStrings[!is.na(allStrings)] 
    if (length(allStrings)==0) {
      allStrings <- NA
    }
    
    # Get the proportion of the most popular string in category
    maxProportion <- 0
    maxString <- NA
    for (string in allStrings) { 
      proportion <- (sum(allStrings==string)/length(allStrings))
      if (!is.na(string) && proportion > maxProportion) {
        maxProportion <- proportion
        maxString <- string
      }
    }
    
    # Return dataframe
    brands[match(brand,allBrands),"category"] <- maxString
    brands[match(brand,allBrands),"proportion"] <- maxProportion
    brands[match(brand,allBrands),"sample"] <- length(allStrings)
  }
  
  # Filter out colors below consensus threshold
  brands[brands$proportion <= threshold,"category"] <- NA
  return(brands)
}

consensusBrands <- data.frame(
  unique(responses$brand),
  getConsensus("whole")$category,
  getConsensus("reduced")$category,
  getConsensus("low")$category,
  getConsensus("skim")$category,
  getConsensus("brandRaw")$category,
  getConsensus("brandRaw")$sample
)

colnames(consensusBrands) <- c("brand","whole","reduced","low","skim","raw","sample")
consensusBrands <- consensusBrands[order(consensusBrands$brand),]
rownames(consensusBrands) <- 1:nrow(consensusBrands)

write.csv(consensusBrands,"brands/consensus.csv")

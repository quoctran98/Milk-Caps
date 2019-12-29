source("rawData/dataCleanup.R")

sortBrands <- function (milkfat, threshold = 0.7) {
  # Initializing dataframe
  brands <- data.frame(NA,NA,NA,NA)
  colnames(brands) <- c("brand","color","proportion","sample")
  allBrands <- as.character(unique(responses$brand))
  
  # Going through each unique brand
  for (brand in allBrands) {
    # Extract all the colors that match the brand
    brands[match(brand,allBrands),"brand"] <- brand
    allColors <- as.character(responses[responses$brand==brand,milkfat])
    allColors <- allColors[!is.na(allColors)] 
    if (length(allColors)==0) {
      allColors <- NA
    }
    
    # Get the proportion of the most popular color
    maxProportion <- 0
    maxColor <- NA
    for (col in allColors) { 
      proportion <- (sum(allColors==col)/length(allColors))
      if (!is.na(col) && proportion > maxProportion) {
        maxProportion <- proportion
        maxColor <- col
      }
    }
    
    # Return dataframe
    brands[match(brand,allBrands),"color"] <- maxColor
    brands[match(brand,allBrands),"proportion"] <- maxProportion
    brands[match(brand,allBrands),"sample"] <- length(allColors)
  }
  
  # Filter out colors below consensus threshold
  brands[brands$proportion <= threshold,"color"] <- NA
  return(brands)
}

consensusBrands <- data.frame(
  unique(responses$brand),sortBrands("whole")$color,
  sortBrands("reduced")$color,
  sortBrands("low")$color,
  sortBrands("skim")$color,
  sortBrands("whole")$sample
)

colnames(consensusBrands) <- c("brand","whole","reduced","low","skim","sample")
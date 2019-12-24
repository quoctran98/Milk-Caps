library(ggmosaic)
source("rawData/dataCleanup.R")
colHex <- read.csv("otherData/colorReference.csv", header=FALSE)
colnames(colHex) <- c("color","hex")

contingencyTable <- function (milkfats) {
  # Making the matrix framework
  # Exclude entire row when either milkfat field is NA
  trimmedResponses <- responses[,milkfats]
  trimmedResponses <- trimmedResponses[!is.na(trimmedResponses[1]),]
  trimmedResponses <- trimmedResponses[!is.na(trimmedResponses[2]),]
  mf1Col <- unique(trimmedResponses[,milkfats[1]])
  mf2Col <- unique(trimmedResponses[,milkfats[2]])
  masterMatrix <- matrix(0, length(mf1Col), length(mf2Col))
  rownames(masterMatrix) <- mf1Col
  colnames(masterMatrix) <- mf2Col
  # Populating the observed matrix
  obsMatrix <- masterMatrix
  for (row in 1:nrow(trimmedResponses)) {
    mf1 <- as.character(trimmedResponses[row,milkfats[1]])
    mf2 <- as.character(trimmedResponses[row,milkfats[2]])
    obsMatrix[mf1,mf2] <- obsMatrix[mf1,mf2] + 1
  }
  
  # Populating the expected matrix = (rowTotal x colTotal) / allTotal
  expMatrix <- masterMatrix
  allTotal <- sum(obsMatrix)
  for (row in 1:nrow(obsMatrix)) {
    for (col in 1:ncol(obsMatrix)) {
      rowTotal <- sum(obsMatrix[row,])
      colTotal <- sum(obsMatrix[,col])
      expMatrix[row,col] <- ((rowTotal*colTotal)/allTotal)
    }
  }
  
  # Populating the chi-square matrix = ((observed-expected)^2)/expected
  chiMatrix <- masterMatrix
  for (row in 1:nrow(obsMatrix)) {
    for (col in 1:ncol(obsMatrix)) {
      obs <- (obsMatrix[row,col])
      exp <- (expMatrix[row,col])
      chiMatrix[row,col] <- (((obs-exp)^2)/exp)
    }
  }
  
  # Exporting the matrices
  obsMatrix <<- obsMatrix
  expMatrix <<- expMatrix
  chiMatrix <<- chiMatrix
  
  # Plotting the contingency tables
  par(las=2 # Perpendicular labels
      )
  mosaicplot(chiMatrix,
             col=as.character(colHex$hex[match(mf2Col,colHex$color)]), # Color is hex of mf2
             xlab=paste(milkfats[1],"milk"), # Rows and columns are flipped on mosaic plot
             ylab=paste(milkfats[2],"milk"),
             main="")
}
 
contingencyTable(c("low","reduced"))

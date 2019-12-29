library(ggmosaic)
source("rawData/dataCleanup.R")
colHex <- read.csv("otherData/colorReference.csv", header=FALSE)
colnames(colHex) <- c("color","hex")

contingencyTable <- function (milkfats, obsThreshold = 0) {
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
  
  # Setting cells with low counts below the threshold to 0
  for (row in 1:nrow(obsMatrix)) {
    for (col in 1:ncol(obsMatrix)) {
      if(obsMatrix[row,col] < obsThreshold) {
        obsMatrix[row,col] <- 0
      }
    }
  }
  
  # Removing newly zeroed rows and columns
  badRows <- c()
  badCols <- c()
  for (row in 1:nrow(obsMatrix)) {
    rowTotal <- sum(obsMatrix[row,])
    if (rowTotal == 0) {
      badRows <- c(badRows,row)
    }
  }
  for (col in 1:ncol(obsMatrix)) {
    colTotal <- sum(obsMatrix[,col])
    if (colTotal == 0) {
      badCols <- c(badCols,row)
    }
  }
  if (length(badRows) != 0) {
    obsMatrix <- obsMatrix[-badRows,]
  }
  if (length(badCols) != 0) {
    obsMatrix <- obsMatrix[,-badCols]
  }
  masterMatrix <- obsMatrix
  
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
      if (obs <= exp) { # Only positive changes from exp->obs are considered
        chiMatrix[row,col] <- 0.01
      } else {
        chiMatrix[row,col] <- (((obs-exp)^2)/exp)
      }
      
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
 
contingencyTable(c("reduced","skim"),2)

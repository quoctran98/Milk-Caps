source("rawData/dataCleanup.R")
library(reshape2)
library(ggplot2)
colHex <- read.csv("otherData/colorReference.csv", header=FALSE)
colnames(colHex) <- c("color","hex")

# milkfats is a vector of 2 milkfats to be compared (indep, dep)
# colors with less observations than obsThreshold are not included
contingencyTable <- function (milkfats, obsThreshold = 0) {
  # Making the matrix framework
  # Exclude entire row when either milkfat field is NA
  trimmedResponses <- responses[,milkfats]
  trimmedResponses <- trimmedResponses[!is.na(trimmedResponses[1]),]
  trimmedResponses <- trimmedResponses[!is.na(trimmedResponses[2]),]
  
  # Colors
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
        # We can't plot underrepresented colors
        chiMatrix[row,col] <- 0
      } else {
        chiMatrix[row,col] <- (((obs-exp)^2)/exp)
      }
      
    }
  }
  
  # Changing absolute values to proportions
  chiMatrixProp <- chiMatrix
  for (row in 1:nrow(chiMatrix)) {
    rowTotal <- sum(chiMatrix[row,])
    for (col in 1:ncol(chiMatrix)) {
      chiMatrixProp[row, col] <- chiMatrix[row, col] / rowTotal
    }
  }
  
  # Plotting mosaic with ggplot
  chiMatrixLong <- melt(chiMatrixProp)
  colnames(chiMatrixLong) <- c("indepMilkfat", "depMilkfat", "value")
  ggplot(chiMatrixLong) +
    geom_bar(aes(x = indepMilkfat, y = value, fill = depMilkfat), color = "black", stat = "identity") + 
    scale_fill_manual(values=as.character(colHex$hex[match(mf2Col,colHex$color)])) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
    theme(axis.text.y=element_blank(),
          axis.text.x=element_text(size = 10),
          axis.title.y=element_text(size = 15),
          axis.title.x=element_text(size = 15),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(legend.position="none") +
    xlab(tools::toTitleCase(paste(milkfats[1], "milk"))) + # Independent milkfat first (hopefully I'm right)
    ylab(tools::toTitleCase(paste(milkfats[2], "milk")))
  
}

for (m1 in c("whole","reduced","low","skim")) {
  for (m2 in c("whole","reduced","low","skim")) {
    #assign(paste(m1, m2, sep = "_"),contingencyTable(c(m1, m2), 2), envir = .GlobalEnv)
    contingencyTable(c(m1, m2), 2)
    ggsave(paste("./visualizations/capCorrImages/",m1, "_", m2, ".png", sep = ""), plot = contingencyTable(c(m1, m2), 2))
  }
}
  
  
  
  
  
  

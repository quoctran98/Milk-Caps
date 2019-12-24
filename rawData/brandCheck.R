#Using this to check if the first 4 letters of each brand is unique enough to ignore the rest

#rawResponses <- read.csv("rawResponses.csv", header=TRUE)
#colnames(rawResponses) <- c("time","zip","brand","whole","reduced","low","skim")

#rawResponses$brand <- toupper(rawResponses$brand)

fullBrand <- rawResponses$brand
fullBrand <- sort(fullBrand)

cutBrand <- substr(rawResponses$brand, 1, 4)
cutBrand <- sort(cutBrand)

brands <- data.frame(cutBrand, fullBrand)
View(brands)
#From Sacha Epskamp on SO: https://stackoverflow.com/a/12996160/5299338
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

#R implementation of Haversine formula from (km): https://rosettacode.org/wiki/Haversine_formula#R
haversineDist <- function(lat1, long1, lat2, long2) {
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  long1 <- long1 * pi / 180
  long2 <- long2 * pi / 180
  a <- sin(0.5 * (lat2 - lat1))
  b <- sin(0.5 * (long2 - long1))
  12742 * asin(sqrt(a * a + cos(lat1) * cos(lat2) * b * b))
}
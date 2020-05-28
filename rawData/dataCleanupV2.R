library(tidyverse)

responses <- read.csv("rawData/rawResponses.csv", header=TRUE)
colnames(responses) <- c("time","zip","brand","whole","reduced","low","skim")

responses <- responses %>%
  .[-read.csv("rawData/badResponses.csv")$row,]

responses <- responses %>%
  mutate(brandRaw = brand) %>%
  mutate(brand = toupper(brand)) %>%
  mutate(brand = str_replace_all(brand, fixed("`"), "")) %>%
  mutate(brand = str_replace_all(brand, fixed("’"), "")) %>%
  mutate(brand = str_replace_all(brand, fixed("'"), "")) %>%
  mutate(brand = str_replace_all(brand, fixed("-"), "")) %>%
  mutate(brand = str_replace_all(brand, fixed("/"), " ")) %>%
  mutate(brand = str_replace_all(brand, fixed(","), " ")) %>%
  mutate(brand = strsplit(brand," "))

mutate(brand = brand[[1]][1])

responses %>%
  mutate(brand = as.character(brand)) %>%
  filter(brand %in% read.csv("rawData/badBrands.csv")$wrong) %>%
  arrange(brand) %>%
  mutate(brand = sort(read.csv("rawData/badBrands.csv")$right))

responses %>%
  mutate(zip = as.character(zip)) %>%
  filter(nchar(zip) <= 5) %>%
  mutate(zip = paste(rep.int("0", nchar(zip)), zip, sep = "")) 


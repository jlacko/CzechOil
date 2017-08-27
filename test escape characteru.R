library(rvest)
library(stringr)


impuls <- read_html(paste(url, i, sep = ''), encoding = "windows-1250")
asdf <- impuls %>%
  html_table()

Benzin <- asdf[[1]]$X7
chrBenzin <- gsub("\\sKč","",Benzin)
numBenzin <- as.double(chrBenzin)

numBenzin

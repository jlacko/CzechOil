library(rvest)
library(stringr)

url <- "http://benzin.impuls.cz/benzin.aspx?strana=3"
impuls <- read_html(url, encoding = "windows-1250")

asdf <- impuls %>%
  html_table()

Benzin <- asdf[[1]]$X7

chrBenzin <- gsub("(*UCP)\\s*Kč","",Benzin, perl=TRUE)
numBenzin <- as.double(chrBenzin)

numBenzin

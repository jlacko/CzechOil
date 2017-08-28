library(rvest)

url <- "http://benzin.impuls.cz/benzin.aspx?strana=3"
impuls <- read_html(url, encoding = "windows-1250")

asdf <- impuls %>%
  html_table()

Benzin <- asdf[[1]]$X7

chrBenzin <- gsub("(*UCP)\\s*Kč","",Benzin, perl=T)  # force PCRE regex engine - more consistent than TRE (R default)
numBenzin <- as.double(chrBenzin)

numBenzin

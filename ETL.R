# Inicializace ----
library(rvest)
library(stringr)
library(dplyr)
library(dbplyr)
library(RPostgreSQL)

url <- "http://benzin.impuls.cz/benzin.aspx?strana=" # bez čísla stránky

frmBenzin <- data.frame() # prázdný data frame

myDb <- dbConnect(dbDriver('PostgreSQL'),
                  host = "jla-postgres.c7ymi3y4c6gx.eu-central-1.rds.amazonaws.com",
                  port = 5432,
                  user = "jindra",
                  dbname = "dbase",
                  password = rstudioapi::askForPassword("Database password"))
 
maxDatum <- dbSendQuery(myDb, "select max(datum) from benzin") %>% #poslat na databázi
  dbFetch() #stahnout výsedky lokálně

# Škrábej ty prkna ať jsou bílý... ----

for (i in 1:56) { # for cyklus k načtení všech stran - počet stran ověřit...

    impuls <- read_html(paste(url, i, sep = ''), encoding = "windows-1250")
    asdf <- impuls %>%
      html_table()
    
    frmBenzin <- rbind(frmBenzin, asdf[[1]])
}

# Vyčištění dat ----

frmBenzin$X1 <- NULL
colnames(frmBenzin) <- c("nazev", "obec", "okres","smes", "datum", "cena")
frmBenzin$cena <- gsub("(*UCP)\\s*Kč", "", frmBenzin$cena, perl = T)
frmBenzin$cena <- as.double(frmBenzin$cena)
frmBenzin$datum <- as.Date(frmBenzin$datum, "%d. %m. %Y")
frmBenzin$okres <- gsub("Hlavní město\\s","",frmBenzin$okres)
frmBenzin$obec <- str_split(frmBenzin$obec, ",", simplify = T)[,1]
frmBenzin$key <- paste(frmBenzin$obec, frmBenzin$okres, sep = "/")

# spočítat a uložit průměr ----

frmBenzinKey <- frmBenzin %>%
  filter(smes == "natural95") %>%
  group_by(key, datum) %>%
  summarise(cena = mean(cena)) %>%
  filter(datum > maxDatum$max) # odstranit duplicitní řádky


db_insert_into(con = myDb, # nainsertovat nové řádky do databáze
               table = "benzin",
               values = frmBenzinKey) 


dbDisconnect(myDb)

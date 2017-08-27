# Inicializace ----
library(rvest)
library(tmap)
library(tmaptools)
library(raster)
library(RCzechia) # set shapefilů pro Českou republiku - devtools::install_github("jlacko/RCzechia")
library(stringr)
library(dplyr)
library(RColorBrewer)

url <- "http://benzin.impuls.cz/benzin.aspx?strana=" # bez čísla stránky
frmBenzin <- data.frame() # prázdný data frame

bbox <- extent(republika) # trochu víc místa nahoře a dole, aby se vešel nadpis & legenda
bbox@ymax <- bbox@ymax + 0.35
bbox@ymin <- bbox@ymin - 0.15

# Škrábej ty prkna ať jsou bílý... ----

for (i in 1:56) { # for cyklus k načtení všech stran

    impuls <- read_html(paste(url, i, sep = ''), encoding = "windows-1250")
    asdf <- impuls %>%
      html_table()
    
    frmBenzin <- rbind(frmBenzin, asdf[[1]])
}

# Vyčištění dat ----

frmBenzin$X1 <- NULL
colnames(frmBenzin) <- c("nazev", "obec", "okres","smes", "datum", "cena")
frmBenzin$cena <- gsub("\\sKč","",frmBenzin$cena)
frmBenzin$cena <- as.double(frmBenzin$cena)
frmBenzin$datum <- as.Date(frmBenzin$datum, "%d. %m. %Y")
frmBenzin$okres <- gsub("Hlavní město\\s","",frmBenzin$okres)
frmBenzin$obec <- str_split(frmBenzin$obec, ",", simplify = T)[,1]
frmBenzin$key <- paste(frmBenzin$obec, frmBenzin$okres, sep = "/")

# průměr za obci místo detailu ----

frmBenzinKey <- frmBenzin %>%
  dplyr::select(key, cena, smes) %>% # raster mi maskuje select (hajzl!)
  filter(smes == "natural95") %>%
  group_by(key) %>%
  summarise(cena = mean(cena))

# příprava shapefilu

obce$key <- paste(obce$Obec, obce$Okres, sep = "/")

obce <- append_data(obce, frmBenzinKey, key.shp = "key", key.data = "key") # 6 z 664 pump nespárovaných jde přijmout

# vlastí kreslení... ----

nadpis <- "Cena benzínu po obcích v České republice" # nadpis grafu
leyenda <- "Cena Naturalu 95"  # nadpis legendy
endCredits <- paste("zdroj dat: Ráádio Impuls (http://benzin.impuls.cz/), staženo k", format(max(frmBenzin$datum), "%d.%m.%Y") ,sep = " ")

wrkObce <- obce[obce$Obyvatel > 300000, ] # Praha, Brno

plot <-   tm_shape(obce, bbox = bbox)+tm_fill(col = "cena", pal = "YlOrRd", title = leyenda, showNA = F, colorNA = NULL)+
  tm_shape(republika, bbox = bbox)+tm_borders("grey30", lwd = 1) +
  tm_shape(wrkObce)+tm_borders("grey20", lwd = 0.5)+
  tm_style_white(nadpis, frame = F, fontfamily = "Calibri", legend.text.size = 0.5, legend.title.size = 0.7, legend.format = list(text.separator = "-", fun=function(x) paste0(formatC(x, digits=2, format="f"), " Kč")))+
  tm_credits(endCredits, position = c("RIGHT", "BOTTOM"), size = 0.4, col = "grey35")

save_tmap(plot , filename = "benzin.png", width = 1600, type = "cairo")



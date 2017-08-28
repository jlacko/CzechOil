# Inicializace ----
library(rvest)
library(tmap)
library(tmaptools)
library(raster)
library(RCzechia) # set shapefilů pro Českou republiku - devtools::install_github("jlacko/RCzechia")
library(stringr)
library(dplyr)
library(RColorBrewer)
#library(extrafont)

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
frmBenzin$cena <- gsub("(*UCP)\\s*Kč", "", frmBenzin$cena, perl = T)
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

obce_body$key <- paste(obce_body$Obec, obce_body$Okres, sep = "/")

obce_body <- append_data(obce_body, frmBenzinKey, key.shp = "key", key.data = "key", ignore.na = T) # ignoruju NA = zahazuju nespárované obce (neznámá cena beňa)
obce_body <- subset(obce_body, !is.na(obce_body$cena)) # zahazuju obce s neznámou cenou benzínu

# vlastí kreslení... ----

nadpis <- "Cena benzínu po obcích v České republice" # nadpis grafu
leyenda <- "Cena Naturalu 95"  # nadpis legendy
endCredits <- paste("zdroj dat: Ráádio Impuls (http://benzin.impuls.cz/), staženo k", format(max(frmBenzin$datum), "%d.%m.%Y") ,sep = " ")

wrkObce <- obce_polygony[obce_polygony$Obyvatel > 300000, ] # Praha, Brno

plot <-   tm_shape(obce_body, bbox = bbox) + tm_bubbles(size = 1/20, col = "cena", border.alpha = 0, showNA = F, pal = "YlOrRd", title.col = leyenda) +
  tm_shape(republika, bbox = bbox) + tm_borders("grey30", lwd = 1) +
  tm_shape(wrkObce) + tm_borders("grey20", lwd = 0.5)+
  tm_style_white(nadpis, frame = F, fontfamily = "Roboto", legend.text.size = 0.5, legend.title.size = 0.7, legend.format = list(text.separator = "-", fun=function(x) paste0(formatC(x, digits=2, format="f"), " Kč")))+
  tm_credits(endCredits, position = c("RIGHT", "BOTTOM"), size = 0.4, col = "grey35")

save_tmap(plot , filename = "benzin.png", width = 1600, type = "cairo")



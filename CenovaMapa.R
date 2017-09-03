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

vObce <- c("Praha", "Brno", "Plzeň", "Ostrava") # velké obce - nejsou puntíkem, ale obrysem

obce_body <- obce_body %>%
  append_data(frmBenzinKey, key.shp = "key", key.data = "key") %>% # klíč = obec / okres
  subset(!is.na(obce_body$cena)) %>%  # zahazuju obce s neznámou cenou benzínu
  subset(!obce_body$Obec %in% vObce) # ať se netluče puntík s polygonem

wrkObce <- obce_polygony[obce_polygony$Obec %in% vObce, ] 
  
# vlastí kreslení... ----

nadpis <- "Cena benzínu po obcích v České republice" # nadpis grafu
leyenda <- "Cena Naturalu 95"  # nadpis legendy
endCredits <- paste("zdroj dat: Ráádio Impuls (http://benzin.impuls.cz), staženo k", format(max(frmBenzin$datum), "%d.%m.%Y") ,sep = " ")

tmBenzin <-   tm_shape(obce_body, bbox = bbox) + tm_bubbles(size = 1/40, col = "cena", alpha = 0.85, border.alpha = 0, showNA = F, pal = "YlOrRd", title.col = leyenda) +
  tm_shape(republika, bbox = bbox) + tm_borders("grey30", lwd = 1) +
  tm_shape(wrkObce) + tm_borders("grey30", lwd = 0.5)+
  tm_style_white(nadpis, frame = F, fontfamily = "Roboto", legend.text.size = 0.4, legend.title.size = 0.7, legend.format = list(text.separator = "-", fun=function(x) paste0(formatC(x, digits=0, format="f"), " Kč")))+
  tm_credits(endCredits, position = c("RIGHT", "BOTTOM"), size = 0.4, col = "grey30")

# print(tmBenzin)

save_tmap(tmBenzin , filename = "benzin.png", width = 1600, type = "cairo")



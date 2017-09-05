# Inicializace ----
library(tmap)
library(tmaptools)
library(raster)
library(RCzechia) # set shapefilů pro Českou republiku - devtools::install_github("jlacko/RCzechia")
library(dplyr)
library(RColorBrewer)
library(dbplyr)
library(RPostgreSQL)

# Připojení databáze ----

myDb <- dbConnect(dbDriver('PostgreSQL'),
                  host = "jla-postgres.c7ymi3y4c6gx.eu-central-1.rds.amazonaws.com",
                  port = 5432,
                  user = "jindra",
                  dbname = "dbase",
                  password = rstudioapi::askForPassword("Database password"))

maxDatum <- dbSendQuery(myDb, "select max(datum) from benzin") %>% # datum posledního uložení
  dbFetch()

frmBenzinKey <- tbl(myDb, "benzin") %>% # data frame posledního záznamu
  as_data_frame() %>%
  filter(datum == maxDatum$max)


# příprava shapefilu ----

bbox <- extent(republika) # trochu víc místa nahoře a dole, aby se vešel nadpis & legenda
bbox@ymax <- bbox@ymax + 0.35
bbox@ymin <- bbox@ymin - 0.15


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
endCredits <- paste("zdroj dat: Ráádio Impuls (http://benzin.impuls.cz), staženo k", format(maxDatum$max, "%d.%m.%Y") ,sep = " ")

tmBenzin <-   tm_shape(obce_body, bbox = bbox) + tm_bubbles(size = 1/40, col = "cena", alpha = 0.85, border.alpha = 0, showNA = F, pal = "YlOrRd", title.col = leyenda) +
  tm_shape(republika, bbox = bbox) + tm_borders("grey30", lwd = 1) +
  tm_shape(wrkObce) + tm_borders("grey30", lwd = 0.5)+
  tm_style_white(nadpis, frame = F, fontfamily = "Roboto", legend.text.size = 0.4, legend.title.size = 0.7, legend.format = list(text.separator = "-", fun=function(x) paste0(formatC(x, digits=0, format="f"), " Kč")))+
  tm_credits(endCredits, position = c("RIGHT", "BOTTOM"), size = 0.4, col = "grey30")

# print(tmBenzin)

save_tmap(tmBenzin , filename = "benzin.png", width = 1600, type = "cairo")



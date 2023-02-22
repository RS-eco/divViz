rm(list=ls()); invisible(gc())

# Load packages ----
#library(shiny) # comment when uploading app.R to server
#library(shinyWidgets)
#library(DT) # datatable()
library(ggplot2)
library(data.table)
library(dplyr)
library(dbplyr)
library(sf)
library(tidyr)
library(DBI)
library(RSQLite)
library(patchwork)
library(raster)
library(terra)
library(ggspatial)
library(scico)
#library(plotly)

# Needed for radar plot!!!
# Almost identical to coord_polar()
coord_straightpolar <- function(theta = 'x', start = 0, direction = 1, clip = "off") {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto(NULL, CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction), clip = clip,
          # This is the different bit
          is_linear = function(){TRUE})
}

# See dbplot
# https://edgararuiz.github.io/dbplot/#raster
# https://db.rstudio.com/best-practices/visualization/

# Load data ----

load("data/bavaria.rda")
bavaria <- sf::st_transform(bavaria, 31468)
load("data/districts.rda")
districts <- sf::st_transform(districts, 31468)
load("data/landkreise.rda")
landkreise <- sf::st_transform(landkreise, 31468)
load("data/tk_district.rda")

# Load taxonomy for adding class/order values
temp <- tempfile(fileext = ".rda")
download.file("https://github.com/RS-eco/bavDC/raw/main/data/taxonomyStd.rda", destfile=temp)
load(temp); rm(temp)

# Load ASK database
my_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "inst/extdata/database.db")
DBI::dbListTables(my_db)

# Pull part of database including data on species 
art_data <- dplyr::tbl(my_db, paste("art")) %>% dplyr::collect()

# Pull part of database including fundorte
fuo_data <- dplyr::tbl(my_db, paste("fuo")) %>% dplyr::collect()

# Load background grid
tk25 <- dplyr::tbl(my_db, "tk25") %>% dplyr::collect() %>%
  tidyr::unite(col="quadrant", c("KARTE", "QUADRANT"), sep="", remove = FALSE) %>% 
  dplyr::select(-c("KARTE_QUAD")) %>% mutate(quadrant = as.numeric(quadrant)) %>%
  as.data.frame() %>% dplyr::select(quadrant, KARTE, XQMITTE, YQMITTE, XLU, YLU, 
                                    XRU, YRU, XRO, YRO, XLO, YLO) %>%
  left_join(tk_district)

# Disconnect from database
DBI::dbDisconnect(my_db); rm(my_db); invisible(gc())

tk25_rough <- tk25 %>% group_by(KARTE, district) %>% 
  summarise(XLU_rough=min(XLU, na.rm=T), XRU_rough=max(XRU, na.rm=T), 
            YLU_rough=min(YLU, na.rm=T), YLO_rough=max(YLO, na.rm=T))

# combine gridded map from dat_ask with full locations of recorded species and taxonomy
art_data <- art_data %>% left_join(fuo_data) %>% 
  mutate(quadrant = as.numeric(sub("/", "", quadrant))) %>%
  left_join(taxonomyStd, by=c("art"="scientificName")) %>% tidyr::drop_na(class, order) %>%
  left_join(tk25) %>% left_join(tk25_rough)
rm(taxonomyStd, fuo_data, tk25, tk_district, tk25_rough); invisible(gc())

# Create custom taxon vector (Aves, Lepidoptera, Odonata, Orthoptera)
art_data$class_order <- "Aves"
art_data$class_order[art_data$class == "Insecta"] <- art_data$order[art_data$class == "Insecta"]
#unique(art_data$class_order)

art_data <- art_data %>% dplyr::select(XLU, YLU, XRU, YLO, XLU_rough, XRU_rough, YLU_rough, YLO_rough, 
                                       jahr, mon, karte, quadrant, district, class_order, family, art2) %>%
  mutate(class_order = factor(class_order, levels=c("Aves", "Lepidoptera", "Odonata", "Orthoptera"), 
                              labels=c("Vögel", "Schmetterlinge", "Libellen", "Heuschrecken"))); invisible(gc())
saveRDS(art_data, "inst/extdata/art_data.rds", compress="xz")
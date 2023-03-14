rm(list=ls()); invisible(gc())

# Load packages ----
library(data.table)
library(dplyr)
library(dbplyr)
library(tidyr)
library(DBI)
library(RSQLite)

# Load data ----
load("data/tk_district.rda")

# Load taxonomy for adding class/order values
temp <- tempfile(fileext = ".rda")
download.file("https://github.com/RS-eco/bavDC/raw/main/data/taxonomyStd.rda", destfile=temp)
load(temp); rm(temp)

# Load ASK database
my_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "data/ASK.db")
DBI::dbListTables(my_db)

# Pull part of database including data on species 
art_data <- dplyr::tbl(my_db, paste("ask_art")) %>% dplyr::collect()

# Pull part of database including fundorte
fuo_data <- dplyr::tbl(my_db, paste("ask_fuo")) %>% dplyr::collect()

# Load background grid
tk25 <- dplyr::tbl(my_db, "geo_tk25_quadranten") %>% dplyr::collect() %>%
  tidyr::unite(col="quadrant", c("KARTE", "QUADRANT"), sep="", remove = FALSE) %>% 
  dplyr::select(-c("KARTE_QUAD")) %>% mutate(quadrant = as.numeric(quadrant)) %>%
  as.data.frame() %>% dplyr::select(quadrant, KARTE, XQMITTE, YQMITTE, XLU, YLU, 
                                    XRU, YRU, XRO, YRO, XLO, YLO) %>%
  left_join(tk_district)

#ask_art$taxon <- substring(ask_art$art_id, 1,3)

fuo_data <- fuo_data %>% filter(ora_fuo_id %in% art_data$ora_fuo_id) %>%
  select(ora_fuo_id, id, karte, quadrant)
colnames(fuo_data)

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

art_data <- art_data %>% dplyr::select(XLU, YLU, XRU, YLO, XLU_rough, XRU_rough, YLU_rough, YLO_rough, 
                                       jahr, mon, karte, quadrant, district, class_order, family, art, sta) %>%
  mutate(class_order = factor(class_order, levels=c("Aves", "Lepidoptera", "Odonata", "Orthoptera"), 
                              labels=c("Vögel", "Schmetterlinge", "Libellen", "Heuschrecken"))); invisible(gc())

# Save file
if(!dir.exists("inst/extdata/")){dir.create("inst/extdata", recursive=T)}
saveRDS(art_data, file="inst/extdata/art_data.rds", compress="xz")

#' ## Create SpatialPolygonsDataFrame of Bavaria

# Verwaltungsbezirke, etc. finden sich hier:
# https://www.ldbv.bayern.de/produkte/opendata/opendata_weitere.html

# Load Verwaltungsbezirke data
districts <- sf::st_read("extdata/regbez_ex.shp")

#' Save to file
save(districts, file="data/districts.rda", compress="xz")

# Landkreise with District information

load("data/districts.rda")

# Load Landkreise data
landkreise <- sf::st_read("extdata/lkr_ex.shp")
#head(landkreise)

plot(sf::st_geometry(districts), col="blue")
plot(sf::st_geometry(landkreise), add=T)

landkreise_centr <- sf::st_centroid(landkreise)
landkreise$BEZ_RBZ <- unlist(sf::st_within(landkreise_centr, districts))
landkreise$BEZ_RBZ <- as.character(factor(landkreise$BEZ_RBZ, levels=c(1:7), labels=districts$BEZ_RBZ))

#' Save to file
save(landkreise, file="data/landkreise.rda", compress="xz")

# Mtbschnit downloaded from https://geo.dianacht.de/mtb/?proj=gk4

mtbschnitt <- sf::st_read("rawdata/mtbshape/mtbschnitt.shp")
st_crs(mtbschnitt) <- 4326
mtbschnitt <- sf::st_transform(mtbschnitt, 31468)
mtb_bav <- mask(vect(mtbschnitt), vect(districts))
mtb_bav <- st_as_sf(mtb_bav)
plot(st_geometry(mtb_bav))
plot(bavaria, add=T)

mtb_centroid <- sf::st_centroid(mtb_bav)
mtb_bav$district <- st_within(mtb_centroid, districts)
mtb_bav$district <- as.character(factor(mtb_bav$district, levels=c(1:7), labels=districts$BEZ_RBZ))
table(mtb_bav$district)

tk_district <- mtb_bav %>% as.data.frame() %>% select(NAME, district) %>%
  rename(KARTE=NAME) %>% mutate(KARTE=as.numeric(KARTE))# %>% drop_na()
tk_district$district[tk_district$KARTE %in% c(7028, 7128, 7227, 7228, 7327, 7427, 7625,
                                              7826, 7926, 8026, 8126, 8226, 8323, 8324, 8423, 8424,
                                              8429, 8525, 8526, 8626, 8628, 8727)] <- "Schwaben"
tk_district$district[tk_district$KARTE %in% c(7843, 7943, 8144, 8244, 8339, 8340, 8342, 8436, 8437,
                                              8442, 8444, 8531, 8543, 8544, 8631, 8632, 8633)] <- "Oberbayern"
tk_district$district[tk_district$KARTE %in% c(7744, 7745, 7646, 7447, 7349, 7249, 6946, 6845)] <- "Niederbayern"
tk_district$district[tk_district$KARTE %in% c(6744, 6644, 6643, 6542, 6341, 6240, 6141, 6041, 5940)] <- "Oberpfalz"
tk_district$district[tk_district$KARTE %in% c(5633, 5433,5434, 5535, 5536, 5537, 5638, 5739, 5839)] <- "Oberfranken"
tk_district$district[tk_district$KARTE %in% c(6927,6526, 6626, 6726, 6826)] <- "Mittelfranken"
tk_district$district[tk_district$KARTE %in% c(5426, 5427, 5524, 5525, 5528, 5629, 5722, 5723, 5730, 5820, 5821,
                                              5822, 5919, 6220,6222,6223,6320,6322,6323,6324,
                                              6420,6421,6424,6425,6525)] <- "Unterfranken"

save(tk_district, file="data/tk_district.rda", compress="xz")

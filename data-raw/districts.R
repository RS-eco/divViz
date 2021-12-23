#' ## Create SpatialPolygonsDataFrame of Bavaria

# Verwaltungsbezirke, etc. finden sich hier:
# https://www.ldbv.bayern.de/produkte/weitere/opendata.html

#' Get GADM data of Germany
#deu <- raster::getData("GADM", country="DEU", level=2, 
#                       path=getwd())

#' Subset data by NAME_1
#districts <- deu[deu$NAME_1 == "Bayern",]

#' Turn into sf-objects
#districts <- sf::st_as_sf(districts)
#plot(districts)
#head(districts)

# Load Verwaltungsbezirke data
districts <- sf::st_read("extdata/regbez_ex.shp")

#' Save to file
save(districts, file="data/districts.rda", compress="xz")

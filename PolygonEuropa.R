library(dplyr)
library(sf)
library(rgdal)
library(raster)


download.file("https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/europe-10-km-100-km/at_download/file", destfile = file.path(tempdir(), "Europe.zip"), mode = "wb")
unzip(zipfile = file.path(tempdir(), "Europe.zip"), exdir = tempdir())
Europe10grid <- shapefile(file.path(tempdir(), "europe_10km.shp"))
Europe10grid  <- st_as_sf(Europe10grid)
#europe_grid_10 <- subset(Europe10grid, select = -c(NofOrigin, EofOrigin))

europe_grid_10km <- cbind(Europe10grid, "Grid_id"=1:nrow(Europe10grid)) 
europe_grid_10km <- subset(europe_grid_10km, select = -c(NofOrigin, EofOrigin, CellCode))


download.file("https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/germany-spatialite/at_download/file", destfile = file.path(tempdir(), "Germany_spatialite.zip"), mode = "wb")
unzip(zipfile = file.path(tempdir(), "Germany_spatialite.zip"), exdir = tempdir())  
ge10grid <- read_sf(file.path(tempdir(), "Germany.sqlite"),layer = "de_10km")
ge_100grid <- read_sf(file.path(tempdir(), "Germany.sqlite"),layer = "de_100km")

download.file("https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/ireland-shapefile/at_download/file",
              destfile = file.path(tempdir(), "Ireland_shapefile"), mode = "wb")
unzip(zipfile = file.path(tempdir(), "Ireland_shapefile"), exdir = tempdir())  

Ireland_10grid <- shapefile(file.path(tempdir(), "ie_10km.shp"))

download.file("https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/great-britain-shapefile/at_download/file",
              destfile = file.path(tempdir(), "Great_Britain_shapefile"), mode = "wb")
unzip(zipfile = file.path(tempdir(), "Great_Britain_shapefile"), exdir = tempdir())  

GB_10grid <- shapefile(file.path(tempdir(), "gb_10km.shp"))

GB_and_IE_grid_10km <- bind(Ireland_10grid, GB_10grid )%>%
  st_as_sf()

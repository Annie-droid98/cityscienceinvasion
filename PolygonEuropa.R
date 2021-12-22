library(dplyr)
library(sf)
library(rgdal)
library(raster)

Polygon_linke_Ecke_oben_lat <- 75
Polygon_linke_Ecke_oben_long <- -25
Polygon_linke_Ecke_oben <- st_point(x = c(Polygon_linke_Ecke_oben_long,Polygon_linke_Ecke_oben_lat), dim = "XY")
Polygon_linke_Ecke_oben <- Polygon_linke_Ecke_oben %>% 
  st_sfc(crs = 4326)%>% 
  st_transform(crs = 5643)
Polygon_linke_Ecke_oben


Polygon_linke_Ecke_unten_lat <- 38
Polygon_linke_Ecke_unten_long <- -25

Polygon_linke_Ecke_unten <- st_point(x = c(Polygon_linke_Ecke_unten_long, Polygon_linke_Ecke_unten_lat), dim = "XY")
Polygon_linke_Ecke_unten <- Polygon_linke_Ecke_unten %>% 
  st_sfc(crs = 4326)%>% 
  st_transform(crs = 5643)
Polygon_linke_Ecke_unten

Polygon_rechte_Ecke_unten_lat <- 38
Polygon_rechte_Ecke_unten_long <- 37

Polygon_rechte_Ecke_unten <- st_point(x = c(Polygon_rechte_Ecke_unten_long, Polygon_rechte_Ecke_unten_lat), dim = "XY")
Polygon_rechte_Ecke_unten <- Polygon_rechte_Ecke_unten %>% 
  st_sfc(crs = 4326)%>% 
  st_transform(crs = 5643)
Polygon_rechte_Ecke_unten

Polygon_rechte_Ecke_oben_lat <- 75
Polygon_rechte_Ecke_oben_long <- 37

Polygon_rechte_Ecke_oben <- st_point(x = c(Polygon_rechte_Ecke_oben_long, Polygon_rechte_Ecke_oben_lat), dim = "XY")
Polygon_rechte_Ecke_oben <- Polygon_rechte_Ecke_oben %>% 
  st_sfc(crs = 4326)%>% 
  st_transform(crs = 5643)
Polygon_rechte_Ecke_oben 


#richtiges
Polygon_europa_transformiertes_crs_tess <- st_polygon(list(rbind(c(-251881.2, 3339335.3), c(-2236639, -364772.2), c(3208389,-670664.7), c(1651861, 3239747), c(-251881.2, 3339335.3))))

Polygon_rasterextent_europe <- st_polygon(list(rbind(c(900000,900000), c(7400000, 900000), c(7400000,5500000), c(900000,5500000),c(900000,900000))))

#Funktion um die verschiedenen Größen zu machen
#richtiges

Cellsizes <- list(5000,10000,20000,50000,100000)
Versuch_grids_transformiert <- lapply((Cellsizes), function(i){
  Polygon_europa_transformiertes_crs_tess %>%
    st_make_grid(cellsize = c(i), flat_topped = TRUE) %>%
    st_intersection(Polygon_europa_transformiertes_crs_tess)%>%
    st_cast("MULTIPOLYGON")%>%
    st_as_sf()%>%
    mutate(id = row_number())})




saveRDS(Versuch_grids_transformiert, file = "Differentgridsizes")


Cellsizes_raster <- list(10000,100000)
Versuch_grids_clc_extent <- lapply((Cellsizes_raster), function(i){
  Polygon_rasterextent_europe %>%
    st_make_grid(cellsize = c(i), flat_topped = TRUE) %>%
    st_intersection(Polygon_rasterextent_europe)%>%
    st_cast("MULTIPOLYGON")%>%
    st_as_sf()%>%
    mutate(id = row_number())})

st_crs(Versuch_grids_clc_extent[[1]]) <- st_crs(CLC_Landcover_2018)

saveRDS(Versuch_grids_clc_extent[[1]],"Grids_EPSG_3035")

download.file("https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/europe-10-km-100-km/at_download/file", destfile = file.path(tempdir(), "Europe.zip"), mode = "wb")
unzip(zipfile = file.path(tempdir(), "Europe.zip"), exdir = tempdir())
Europe10grid <- read_sf(file.path(tempdir(), "europe_10km.shp"))

europe_grid_10 <- subset(Europe10grid, select = -c(NofOrigin, EofOrigin))
Europe100grid <-  read_sf(file.path(tempdir(), "europe_100km.shp"))

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

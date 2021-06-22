install.packages("rgbif")
install.packages("sf")
install.packages("dplyr")
install.packages("sp")
install.packages("wellknown")
install.packages("ggplot2")
library(ggplot2)
library(wellknown)
library(rgbif)
library(dplyr)
library(sf)
library(leaflet)
library(maps)
library(rgeos)
#buffer europa
#buffer Mittelpunkt Hamburg

Mittelpunkt_Europa <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Europa[1,] <- c(49.843056,9.901944)
coordinates(Mittelpunkt_Europa) <- ~long + lat
Mittelpunkt_Europa
Mittelpunkt_Europasf <- as(Mittelpunkt_Europa, "sf")
Europa_Buffer <- st_buffer(Mittelpunkt_Europasf, dist = 0.2, endCapStyle="ROUND")
Europa_Buffer

#visualize the buffer on a map
mapHamburg_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Hamburg_Buffer)
mapHamburg_Buffer


#buffer von mir um den Mittelpunkt von Berlin
example_points <- data.frame(lat=numeric(), long= numeric())
example_points[1,] <- c(52.5200,13.4050)
coordinates(example_points) <- ~long + lat
example_points
example_pointssf <- as(example_points, "sf")
buffer_sf <- st_buffer(example_pointssf,  dist = 0.2, endCapStyle="ROUND")
buffer_sf

#visualize the buffer on a map
mapBerlin_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=buffer_sf)
mapBerlin_Buffer

#buffer Mittelpunkt Hamburg
Mittelpunkt_Hamburg <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Hamburg[1,] <- c(53.5511,9.9937)
coordinates(Mittelpunkt_Hamburg) <- ~long + lat
Mittelpunkt_Hamburg
Mittelpunkt_Hamburgsf <- as(Mittelpunkt_Hamburg, "sf")
Hamburg_Buffer <- st_buffer(Mittelpunkt_Hamburgsf, dist = 0.2, endCapStyle="ROUND")
Hamburg_Buffer

#visualize the buffer on a map
mapHamburg_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Hamburg_Buffer)
mapHamburg_Buffer


#Buffer München
Mittelpunkt_München <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_München[1,] <- c(48.1351,11.5820)
coordinates(Mittelpunkt_München) <- ~long + lat
Mittelpunkt_München
Mittelpunkt_Münchensf <- as(Mittelpunkt_München, "sf")
München_Buffer <- st_buffer(Mittelpunkt_Münchensf, dist = 0.2, endCapStyle="ROUND")
München_Buffer
#visualize the buffer on a map
mapMünchen_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=München_Buffer)
mapMünchen_Buffer


#Buffer Köln
Mittelpunkt_Köln <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Köln[1,] <- c(50.9375,6.9603)
coordinates(Mittelpunkt_Köln) <- ~long + lat
Mittelpunkt_Köln
Mittelpunkt_Kölnsf <- as(Mittelpunkt_Köln, "sf")
Köln_Buffer <- st_buffer(Mittelpunkt_Kölnsf, dist = 0.2, endCapStyle="ROUND")
Köln_Buffer
#visualize the buffer on a map
mapKöln_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Köln_Buffer)
mapKöln_Buffer

#buffer Mittelpunkt Frankfurt am Main
Mittelpunkt_FrankfurtamMain <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_FrankfurtamMain[1,] <- c(50.1109,8.6821)
coordinates(Mittelpunkt_FrankfurtamMain) <- ~long + lat
Mittelpunkt_FrankfurtamMain
Mittelpunkt_FrankfurtamMainsf <- as(Mittelpunkt_FrankfurtamMain, "sf")
FrankfurtamMain_Buffer <- st_buffer(Mittelpunkt_FrankfurtamMainsf, dist = 0.2, endCapStyle="ROUND")
FrankfurtamMain_Buffer

#visualize the buffer on a map
mapFrankfurtamMain_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=FrankfurtamMain_Buffer)
mapFrankfurtamMain_Buffer

#buffer Mittelpunkt Stuttgart
Mittelpunkt_Stuttgart <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Stuttgart[1,] <- c(48.7758,9.1829)
coordinates(Mittelpunkt_Stuttgart) <- ~long + lat
Mittelpunkt_Stuttgart
Mittelpunkt_Stuttgartsf <- as(Mittelpunkt_Stuttgart, "sf")
Stuttgart_Buffer <- st_buffer(Mittelpunkt_Stuttgartsf, dist = 0.2, endCapStyle="ROUND")
Stuttgart_Buffer

#visualize the buffer on a map
mapStuttgart_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Stuttgart_Buffer)
mapStuttgart_Buffer

#buffer Mittelpunkt Düsseldorf
Mittelpunkt_Düsseldorf <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Düsseldorf[1,] <- c(51.2277,6.7735)
coordinates(Mittelpunkt_Düsseldorf) <- ~long + lat
Mittelpunkt_Düsseldorf
Mittelpunkt_Düsseldorfsf <- as(Mittelpunkt_Düsseldorf, "sf")
Düsseldorf_Buffer <- st_buffer(Mittelpunkt_Düsseldorfsf, dist = 0.2, endCapStyle="ROUND")
Düsseldorf_Buffer

#visualize the buffer on a map
mapStuttgart_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Stuttgart_Buffer)
mapStuttgart_Buffer

#buffer Mittelpunkt Leipzig
Mittelpunkt_Leipzig <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Leipzig[1,] <- c(51.3397,12.3731)
coordinates(Mittelpunkt_Leipzig) <- ~long + lat
Mittelpunkt_Leipzig
Mittelpunkt_Leipzigsf <- as(Mittelpunkt_Leipzig, "sf")
Leipzig_Buffer <- st_buffer(Mittelpunkt_Leipzigsf, dist = 0.2, endCapStyle="ROUND")
Leipzig_Buffer

#visualize the buffer on a map
mapLeipzig_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Leipzig_Buffer)
mapLeipzig_Buffer

#buffer Mittelpunkt Dortmund
Mittelpunkt_Dortmund <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Dortmund[1,] <- c(51.5136,7.4653)
coordinates(Mittelpunkt_Dortmund) <- ~long + lat
Mittelpunkt_Dortmund
Mittelpunkt_Dortmundsf <- as(Mittelpunkt_Dortmund, "sf")
Dortmund_Buffer <- st_buffer(Mittelpunkt_Dortmundsf, dist = 0.2, endCapStyle="ROUND")
Dortmund_Buffer

#visualize the buffer on a map
mapDortmund_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Dortmund_Buffer)
mapDortmund_Buffer

#buffer Mittelpunkt Essen
Mittelpunkt_Essen <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Essen[1,] <- c(51.4556,7.0116)
coordinates(Mittelpunkt_Essen) <- ~long + lat
Mittelpunkt_Essen
Mittelpunkt_Essensf <- as(Mittelpunkt_Essen, "sf")
Essen_Buffer <- st_buffer(Mittelpunkt_Essensf, dist = 0.2, endCapStyle="ROUND")
Essen_Buffer

#visualize the buffer on a map
mapEssen_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Essen_Buffer)
mapEssen_Buffer

#buffer Mittelpunkt Bremen
Mittelpunkt_Bremen <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Bremen[1,] <- c(53.0793,8.8017)
coordinates(Mittelpunkt_Bremen) <- ~long + lat
Mittelpunkt_Bremen
Mittelpunkt_Bremensf <- as(Mittelpunkt_Bremen, "sf")
Bremen_Buffer <- st_buffer(Mittelpunkt_Bremensf, dist = 0.2, endCapStyle="ROUND")
Bremen_Buffer

#visualize the buffer on a map
mapBremen_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Bremen_Buffer)
mapBremen_Buffer


#buffer Mittelpunkt Dresden
Mittelpunkt_Dresden <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Dresden[1,] <- c(51.0504,13.7373)
coordinates(Mittelpunkt_Dresden) <- ~long + lat
Mittelpunkt_Dresden
Mittelpunkt_Dresdensf <- as(Mittelpunkt_Dresden, "sf")
Dresden_Buffer <- st_buffer(Mittelpunkt_Dresdensf, dist = 0.2, endCapStyle="ROUND")
Dresden_Buffer

#visualize the buffer on a map
mapDresden_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Dresden_Buffer)
mapDresden_Buffer


#buffer Mittelpunkt Hannover
Mittelpunkt_Hannover <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Hannover[1,] <- c(52.3759,9.7320)
coordinates(Mittelpunkt_Hannover) <- ~long + lat
Mittelpunkt_Hannover
Mittelpunkt_Hannoversf <- as(Mittelpunkt_Hannover, "sf")
Hannover_Buffer <- st_buffer(Mittelpunkt_Hannoversf, dist = 0.2, endCapStyle="ROUND")
Hannover_Buffer

#visualize the buffer on a map
mapHannover_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Hannover_Buffer)
mapHannover_Buffer

#buffer Mittelpunkt Nürnberg
Mittelpunkt_Nürnberg <- data.frame(lat=numeric(), long= numeric())
Mittelpunkt_Nürnberg[1,] <- c(49.4521,11.0767)
coordinates(Mittelpunkt_Nürnberg) <- ~long + lat
Mittelpunkt_Nürnberg
Mittelpunkt_Nürnbergsf <- as(Mittelpunkt_Nürnberg, "sf")
Nürnberg_Buffer <- st_buffer(Mittelpunkt_Nürnbergsf, dist = 0.2, endCapStyle="ROUND")
Nürnberg_Buffer

#visualize the buffer on a map
mapNürnberg_Buffer <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Nürnberg_Buffer)
mapNürnberg_Buffer


#such Anfrage mit dem Buffer um Berlin
wktbuffer <- sf_convert(buffer_sf)
searchbuffer <- occ_search(taxonKey = 4989904, geometry = wktbuffer, geom_big = "bbox", limit = 2000)
searchbuffer
class(searchbuffer)
searchbuffer <- searchbuffer$data

searchalle2019<- occ_search(taxonKey = 4989904,year = 2019)
searchalle2019
searchbuffer2 <- searchbuffer %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, datasetName)

#Suchanfrage Harmonia für alle Jahre
searchalleHarmonialleJahre <-occ_search(scientificName = "Harmonia axyridis", limit = 25000, country = "DE",  hasCoordinate = T)
searchalleHarmonialleJahre
searchalleHarmonialleJahre <- searchalleHarmonialleJahre$data

searchalleHarmonialleJahre2 <- searchalleHarmonialleJahre %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, datasetName)

#Filter nach den Jahren ab 2000
searchalleHarmonialleJahre2.1 <- data.frame(searchalleHarmonialleJahre2)

searchalleHarmonialleJahre2.1 <- searchalleHarmonialleJahre2.1 %>%
  filter(year>1999)
searchalleHarmonialleJahre2.2 <- searchalleHarmonialleJahre2.1 %>% 
  as.data.frame %>% 
  sf::st_as_sf(coords = c(2,3))
#Vorbereitung für den Plot für Berlin
points_within_Berlin_ab_2000 <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, buffer_sf, sparse = F), ]
points_within_Berlin_ab_2000

separated_coord_Berlin_ab_2000 <- points_within_Berlin_ab_2000 %>%
  mutate(lat = unlist(map(points_within_Berlin_ab_2000$geometry,1)),
         long = unlist(map(points_within_Berlin_ab_2000$geometry,2)))
separated_coord_Berlin_ab_2000
#geplottete density von den Coordinaten aus meinem Buffer
#der mit den farben und smooth
ggplot(separated_coord_Berlin_ab_2000, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord) + 
  scale_fill_viridis_c() + 
  ggtitle("Density Harmonia axyridis since 2000") +
  theme(legend.position = 'none')
#punkte wrap year
ggplot(separated_coord_Berlin_ab_2000, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axridis in Berlin since 2000") +
  facet_wrap(~year)
#Ende vom plotten der density für Berlin ab 2000



separated_coord_Berlin_ab_2000






searchalleHarmonialleJahre3 <- searchalleHarmonialleJahre2 %>% 
  as.data.frame %>% 
  sf::st_as_sf(coords = c(2,3))



occ_inBuffer <- st_contains(buffer_sf, searchalleHarmonialleJahre3)

#ich bekomme eine Tabelle und nicht nur die auf die es zutrifft! jetzt brauche ich noch die geometrie nach declong und lat
points_within <- searchalleHarmonialleJahre3[st_within( searchalleHarmonialleJahre3, buffer_sf, sparse = F), ]
points_within  

#filter geometry  ist nicht notwendig funktioniert auch wenn man die points_within direkt separiert
points_within_geometry <- points_within %>% 
  dplyr::select(geometry)
points_within_geometry

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord <- points_within_geometry %>%
  mutate(lat = unlist(map(points_within_geometry$geometry,1)),
         long = unlist(map(points_within_geometry$geometry,2)))

separated_coord

#plot the density in the buffer problem ist, dass die vom Buffer ausgegebenen nur die row + col id beinhalten und nicht die lat und long für BERLIN

#geplottete density von den Coordinaten aus meinem Buffer
#der mit den farben und smooth
ggplot(separated_coord, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')
#punkte
ggplot(separated_coord, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude')

#Density für Hamburg
points_within_Hamburg_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, Hamburg_Buffer, sparse = F), ]
points_within_Hamburg_Buffer
#filter geometry
points_within_geometry_Hamburg <- points_within_Hamburg_Buffer %>% 
  dplyr::select(geometry)
points_within_geometry_Hamburg

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Hamburg <- points_within_Hamburg_Buffer %>%
  mutate(lat = unlist(map(points_within_Hamburg_Buffer$geometry,1)),
         long = unlist(map(points_within_Hamburg_Buffer$geometry,2)))

separated_coord_Hamburg

ggplot(separated_coord_Hamburg, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Hamburg) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Hamburg, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Hamburg since 2000") +
  facet_wrap(~year)

#Density fürMünche
points_within_München_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, München_Buffer, sparse = F), ]
points_within_München_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_München <- points_within_München_Buffer %>%
  mutate(lat = unlist(map(points_within_München_Buffer$geometry,1)),
         long = unlist(map(points_within_München_Buffer$geometry,2)))

separated_coord_München

ggplot(separated_coord_München, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_München) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_München, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Munich since 2000") +
  facet_wrap(~year)

#Density für Köln
points_within_Köln_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, Köln_Buffer, sparse = F), ]
points_within_Köln_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Köln <- points_within_Köln_Buffer %>%
  mutate(lat = unlist(map(points_within_Köln_Buffer$geometry,1)),
         long = unlist(map(points_within_Köln_Buffer$geometry,2)))

separated_coord_Köln

ggplot(separated_coord_Köln, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Köln) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Köln, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis Cologne in  since 2000") +
  facet_wrap(~year)

#Density für Frankurt am Main
points_within_FrankfurtamMain_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, FrankfurtamMain_Buffer, sparse = F), ]
points_within_FrankfurtamMain_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_FrankfurtmMain <- points_within_FrankfurtamMain_Buffer %>%
  mutate(lat = unlist(map(points_within_FrankfurtamMain_Buffer$geometry,1)),
         long = unlist(map(points_within_FrankfurtamMain_Buffer$geometry,2)))

separated_coord_FrankfurtmMain

ggplot(separated_coord_FrankfurtmMain, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_FrankfurtmMain) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_FrankfurtmMain, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Frankfurt on the main since 2000") +
  facet_wrap(~year)

#Density für Stuttgart
points_within_Stuttgart_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, Stuttgart_Buffer, sparse = F), ]
points_within_Stuttgart_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Stuttgart <- points_within_Stuttgart_Buffer %>%
  mutate(lat = unlist(map(points_within_Stuttgart_Buffer$geometry,1)),
         long = unlist(map(points_within_Stuttgart_Buffer$geometry,2)))

separated_coord_Stuttgart

ggplot(separated_coord_Stuttgart, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Stuttgart) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Stuttgart, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Stuttgart since 2000") +
  facet_wrap(~year)


#Density für Düsseldorf
points_within_Düsseldorf_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, Düsseldorf_Buffer, sparse = F), ]
points_within_Düsseldorf_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Düsseldorf <- points_within_Düsseldorf_Buffer %>%
  mutate(lat = unlist(map(points_within_Düsseldorf_Buffer$geometry,1)),
         long = unlist(map(points_within_Düsseldorf_Buffer$geometry,2)))

separated_coord_Düsseldorf

ggplot(separated_coord_Düsseldorf, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Düsseldorf) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Düsseldorf, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Dusseldorf since 2000") +
  facet_wrap(~year)

#Density für Leipzig
points_within_Leipzig_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, Leipzig_Buffer, sparse = F), ]
points_within_Leipzig_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Leipzig <- points_within_Leipzig_Buffer %>%
  mutate(lat = unlist(map(points_within_Leipzig_Buffer$geometry,1)),
         long = unlist(map(points_within_Leipzig_Buffer$geometry,2)))

separated_coord_Leipzig

ggplot(separated_coord_Leipzig, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Leipzig) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Leipzig, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Leipzig since 2000") +
  facet_wrap(~year)


#Density für Dortmund
points_within_Dortmund_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, Dortmund_Buffer, sparse = F), ]
points_within_Dortmund_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Dortmund <- points_within_Dortmund_Buffer %>%
  mutate(lat = unlist(map(points_within_Dortmund_Buffer$geometry,1)),
         long = unlist(map(points_within_Dortmund_Buffer$geometry,2)))

separated_coord_Dortmund

ggplot(separated_coord_Dortmund, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Dortmund) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Dortmund, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Dortmund since 2000") +
  facet_wrap(~year)


#Density für Essen
points_within_Essen_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, Essen_Buffer, sparse = F), ]
points_within_Essen_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Essen <- points_within_Essen_Buffer %>%
  mutate(lat = unlist(map(points_within_Essen_Buffer$geometry,1)),
         long = unlist(map(points_within_Essen_Buffer$geometry,2)))

separated_coord_Essen

ggplot(separated_coord_Essen, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Essen) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Essen, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Essen since 2000") +
  facet_wrap(~year)


#Density für Bremen
points_within_Bremen_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, Bremen_Buffer, sparse = F), ]
points_within_Bremen_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Bremen <- points_within_Bremen_Buffer %>%
  mutate(lat = unlist(map(points_within_Bremen_Buffer$geometry,1)),
         long = unlist(map(points_within_Bremen_Buffer$geometry,2)))

separated_coord_Bremen

ggplot(separated_coord_Bremen, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Bremen) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Bremen, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Bremen since 2000") +
  facet_wrap(~year)

#Density für Dresden
points_within_Dresden_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, Dresden_Buffer, sparse = F), ]
points_within_Dresden_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Dresden <- points_within_Dresden_Buffer %>%
  mutate(lat = unlist(map(points_within_Dresden_Buffer$geometry,1)),
         long = unlist(map(points_within_Dresden_Buffer$geometry,2)))

separated_coord_Dresden

ggplot(separated_coord_Dresden, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Dresden) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Dresden, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Dresden since 2000") +
  facet_wrap(~year)

#Density für Hannover
points_within_Hannover_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, Hannover_Buffer, sparse = F), ]
points_within_Hannover_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Hannover <- points_within_Hannover_Buffer %>%
  mutate(lat = unlist(map(points_within_Hannover_Buffer$geometry,1)),
         long = unlist(map(points_within_Hannover_Buffer$geometry,2)))

separated_coord_Hannover

ggplot(separated_coord_Hannover, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Hannover) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Hannover, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Hannover since 2000") +
  facet_wrap(~year)

#Density für Nürnberg
points_within_Nürnberg_Buffer <- searchalleHarmonialleJahre2.2[st_within( searchalleHarmonialleJahre2.2, Nürnberg_Buffer, sparse = F), ]
points_within_Nürnberg_Buffer

#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Nürnberg <- points_within_Nürnberg_Buffer %>%
  mutate(lat = unlist(map(points_within_Nürnberg_Buffer$geometry,1)),
         long = unlist(map(points_within_Nürnberg_Buffer$geometry,2)))

separated_coord_Nürnberg

ggplot(separated_coord_Nürnberg, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Nürnberg) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Nürnberg, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Harmonia axyridis in Nürnberg since 2000") +
  facet_wrap(~year)


#---------------------------------------------------------------------------------------------------------------------------------------------------
#Suchanfrage Harmonia für alle Jahre
searchCoccinellaalleJahre <-occ_search(scientificName = "Coccinella septempunctata", limit = 25000, country = "DE",  hasCoordinate = T)
searchCoccinellaalleJahre
searchCoccinellaalleJahre2 <- searchCoccinellaalleJahre$data
searchCoccinellaalleJahre2 <- searchCoccinellaalleJahre2 %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, datasetName)

#Filter nach den Jahren ab 2000
searchCoccinellaalleJahre2.1 <- data.frame(searchCoccinellaalleJahre2)

searchCoccinellaalleJahre2.1 <- searchCoccinellaalleJahre2.1 %>%
  filter(year>1999)
searchCoccinellaalleJahre2.2 <- searchCoccinellaalleJahre2.1 %>% 
  as.data.frame %>% 
  sf::st_as_sf(coords = c(2,3))
#Vorbereitung für den Plot für Berlin Density
points_within_Berlin_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, buffer_sf, sparse = F), ]
points_within_Berlin_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Berlin_Coccinella <- points_within_Berlin_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Berlin_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Berlin_ab_2000_Coccinella$geometry,2)))

separated_coord_Berlin_Coccinella

ggplot(separated_coord_Berlin_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Berlin_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Berlin_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Berlin since 2000") +
  facet_wrap(~year)


occ_inBufferplot <- ggplot(separated_coord, aes(x = long, y = lat)) +
  geom_point()+
  xlim(0, 25) +
  ylim(0, 100)




#Density Hamburg
points_within_Hamburg_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, Hamburg_Buffer, sparse = F), ]
points_within_Hamburg_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Hamburg_Coccinella <- points_within_Hamburg_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Hamburg_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Hamburg_ab_2000_Coccinella$geometry,2)))

separated_coord_Hamburg_Coccinella

ggplot(separated_coord_Hamburg_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Hamburg_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Hamburg_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Hamburg since 2000") +
  facet_wrap(~year)


#Density München
points_within_München_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, München_Buffer, sparse = F), ]
points_within_München_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_München_Coccinella <- points_within_München_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_München_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_München_ab_2000_Coccinella$geometry,2)))

separated_coord_München_Coccinella

ggplot(separated_coord_München_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_München_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_München_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata München since 2000") +
  facet_wrap(~year)

#Density Köln
points_within_Köln_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, Köln_Buffer, sparse = F), ]
points_within_Köln_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Köln_Coccinella <- points_within_Köln_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Köln_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Köln_ab_2000_Coccinella$geometry,2)))

separated_coord_München_Coccinella

ggplot(separated_coord_Köln_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Köln_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Köln_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Köln since 2000") +
  facet_wrap(~year)

#Density Frankfurt am Main
points_within_FrankfurtamMain_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, FrankfurtamMain_Buffer, sparse = F), ]
points_within_FrankfurtamMain_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_FrankfurtamMain_Coccinella <- points_within_FrankfurtamMain_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_FrankfurtamMain_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_FrankfurtamMain_ab_2000_Coccinella$geometry,2)))

separated_coord_FrankfurtamMain_Coccinella

ggplot(separated_coord_FrankfurtamMain_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Stuttgart_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_FrankfurtamMain_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Frankfurt am Main since 2000") +
  facet_wrap(~year)

occ_inBufferplot
occ_inBufferyear<- ggplot(searchbuffer, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point()+
  xlim(0, 25) +
  ylim(0, 100) +
  facet_wrap(~year)

#Density Stuttgart
points_within_Stuttgart_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, Stuttgart_Buffer, sparse = F), ]
points_within_Stuttgart_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Stuttgart_Coccinella <- points_within_Stuttgart_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Stuttgart_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Stuttgart_ab_2000_Coccinella$geometry,2)))

separated_coord_Stuttgart_Coccinella

ggplot(separated_coord_Stuttgart_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Stuttgart_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Stuttgart_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Stuttgart since 2000") +
  facet_wrap(~year)


#Density Düsseldorf
points_within_Düsseldorf_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, Düsseldorf_Buffer, sparse = F), ]
points_within_Düsseldorf_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Düsseldorf_Coccinella <- points_within_Düsseldorf_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Düsseldorf_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Düsseldorf_ab_2000_Coccinella$geometry,2)))

separated_coord_Düsseldorf_Coccinella

ggplot(separated_coord_Düsseldorf_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Düsseldorf_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Düsseldorf_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Düsseldorf since 2000") +
  facet_wrap(~year)

#Density Leipzig
points_within_Leipzig_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, Leipzig_Buffer, sparse = F), ]
points_within_Leipzig_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Leipzig_Coccinella <- points_within_Leipzig_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Leipzig_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Leipzig_ab_2000_Coccinella$geometry,2)))

separated_coord_Leipzig_Coccinella

ggplot(separated_coord_Leipzig_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Leipzig_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Leipzig_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Leipzig since 2000") +
  facet_wrap(~year)


#Density Dortmund
points_within_Dortmund_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, Dortmund_Buffer, sparse = F), ]
points_within_Dortmund_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Dortmund_Coccinella <- points_within_Dortmund_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Dortmund_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Dortmund_ab_2000_Coccinella$geometry,2)))

separated_coord_Dortmund_Coccinella

ggplot(separated_coord_Dortmund_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Dortmund_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Dortmund_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Dortmund since 2000") +
  facet_wrap(~year)



#Density Bremen
points_within_Bremen_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, Bremen_Buffer, sparse = F), ]
points_within_Bremen_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Bremen_Coccinella <- points_within_Bremen_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Bremen_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Bremen_ab_2000_Coccinella$geometry,2)))

separated_coord_Bremen_Coccinella

ggplot(separated_coord_Bremen_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Bremen_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Bremen_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Bremen since 2000") +
  facet_wrap(~year)

#Density Essen
points_within_Essen_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, Essen_Buffer, sparse = F), ]
points_within_Essen_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Essen_Coccinella <- points_within_Essen_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Essen_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Essen_ab_2000_Coccinella$geometry,2)))

separated_coord_Essen_Coccinella

ggplot(separated_coord_Essen_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Essen_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Essen_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Essen since 2000") +
  facet_wrap(~year)


#Density Dresden
points_within_Dresden_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, Dresden_Buffer, sparse = F), ]
points_within_Dresden_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Dresden_Coccinella <- points_within_Dresden_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Dresden_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Dresden_ab_2000_Coccinella$geometry,2)))

separated_coord_Dresden_Coccinella

ggplot(separated_coord_Dresden_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Dresden_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Dresden_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Dresden since 2000") +
  facet_wrap(~year)


#Density Hannover
points_within_Hannover_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, Hannover_Buffer, sparse = F), ]
points_within_Hannover_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Hannover_Coccinella <- points_within_Hannover_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Hannover_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Hannover_ab_2000_Coccinella$geometry,2)))

separated_coord_Hannover_Coccinella

ggplot(separated_coord_Hannover_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Hannover_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Hannover_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Hannover since 2000") +
  facet_wrap(~year)


#Density Nürnberg
points_within_Nürnberg_ab_2000_Coccinella <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, Nürnberg_Buffer, sparse = F), ]
points_within_Nürnberg_ab_2000_Coccinella



#separierung der Coordinaten nach long und lat
library(tidyverse)

separated_coord_Nürnberg_Coccinella <- points_within_Nürnberg_ab_2000_Coccinella %>%
  mutate(lat = unlist(map(points_within_Nürnberg_ab_2000_Coccinella$geometry,1)),
         long = unlist(map(points_within_Nürnberg_ab_2000_Coccinella$geometry,2)))

separated_coord_Nürnberg_Coccinella

ggplot(separated_coord_Nürnberg_Coccinella, aes(x = long, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = separated_coord_Nürnberg_Coccinella) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#punkte
ggplot(separated_coord_Nürnberg_Coccinella, aes(x = long, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  ggtitle("Density Coccinella septempunctata Nürnberg since 2000") +
  facet_wrap(~year)


#andere Art des density plots aber das problem ist, dass ich in die x Achse entweder longitude oder latitude nur eingeben kann und es nicht 3D ist
# Change line color and fill color
ggplot(separated_coord_Berlin_Coccinella, aes(x=long, z=lat))+
  geom_density(color="darkblue", fill="lightblue")
#funktioniert nicht wirklich
ggplot(rbind(data.frame(separated_coord_Berlin_ab_2000, group="a"), data.frame(separated_coord_Berlin_Coccinella, group="b")), 
       aes(x=long,y=lat)) + 
  stat_density2d(geom="density2d", aes(color = group,alpha=..level..),
                 size=2,
                 contour=TRUE) + 
  scale_color_manual(values=c("a"="#FF0000", "b"="#00FF00")) +
  theme_minimal() +
  xlim(52.2, 52.8) + ylim(13.0, 13.6) +
  coord_cartesian(xlim = c(52.2, 52.8), ylim = c(13.0, 13.6))
#eigentlich ist hier noch ein geom_point drin dann sieht es aber sehr unübersichtlich aus
ggplot(rbind(data.frame(separated_coord_Berlin_ab_2000, group="a"), data.frame(separated_coord_Berlin_Coccinella, group="b")), 
       aes(x=long,y=lat)) + 
  stat_density2d(geom="density2d", aes(color = group,alpha=..level..),
                 size=2,
                 contour=TRUE) + 
  scale_color_manual(values=c("a"="#FF0000", "b"="#00FF00")) +
  theme_minimal() + 
  geom_point() +
  xlim(52.2, 52.8) + ylim(13.0, 13.6) +
  coord_cartesian(xlim = c(52.2, 52.8), ylim = c(13.0, 13.6))
#andere Art aber hier hängt das Bild davon ab, welche Daten ich zuerst plotte
rbind(data.frame(separated_coord_Berlin_ab_2000, group="a"), data.frame(separated_coord_Berlin_Coccinella, group="b")) %>%
  filter(year==2019) %>%
ggplot(aes(x=long,y=lat)) + 
  stat_density2d( aes(color = group)) + 
 # scale_fill_manual(values=c("a"="#FF0000", "b"="#00FF00")) +
  theme_minimal() +
  geom_point(aes(color = group, alpha=0.5)) 
#mit und ohne geom_point
ggplot(rbind(data.frame(separated_coord_Berlin_ab_2000, group="a"), data.frame(separated_coord_Berlin_Coccinella, group="b")), aes(x=long,y=lat)) + 
  stat_density2d(geom="tile", aes(fill = group, alpha=..density..), contour=FALSE) + 
  scale_fill_manual(values=c("a"="#FF0000", "b"="#00FF00")) +
  geom_point() +
  theme_minimal() 
#anderer Versuch einer density map verstehe nur noch nicht wofür mean und Standartabweichung ist und wie ich die Achsen zu log und lat machen kann

set.seed(4)
g = list(NA,NA)
for (i in 1:2) {
  sdev = runif(1)
  X = rnorm(1000, mean = 512, sd= 300*sdev)
  Y = rnorm(1000, mean = 384, sd= 200*sdev)
  this_df = as.data.frame( cbind(X = X,Y = Y, condition = 1:2) )
  this_df = as.data.frame( cbind(X = separated_coord_Berlin_ab_2000,Y = separated_coord_Berlin_Coccinella, condition = 1:2) )
  
  g[[i]] = ggplot(data= this_df, aes(x=X, y=Y) ) + 
    geom_point(aes(color= as.factor(condition)), alpha= .25) +
    coord_cartesian(ylim= c(0, 768), xlim= c(0,1024)) + scale_y_reverse() +
    stat_density2d(mapping= aes(alpha = ..level.., color= as.factor(condition)), geom="contour", bins=4, size= 2) 
  
}
print(g) # level has a different scale for each


#Test mit Quadrat density spannend, da ich so ein Gitter über meinen Buffer legen kann und schauen kann wie viele Beobachtungen es in jedem Quadrat gibt, vielleicht kann man dann die Quadrate der beiden Spezien miteinander vergleichen

library(maptools)
library(raster)
library(sf)
install.packages("spatstat")
library(spatstat)
library(rgdal)
library(broom)

Q_count_Harmonia <- by(separated_coord_Berlin_ab_2000, separated_coord_Berlin_ab_2000$year, 
   function(x){
  P   <- as.ppp(x) 
  marks(P) <- NULL
  quadratcount(P, nx=6, ny=3)
   })


#selbe Funktion anderes funktioniert
Q_count_C <- by(separated_coord_Berlin_Coccinella, separated_coord_Berlin_Coccinella$year, 
   function(x){
     Berlin_2000_C7_2 <- as.ppp(separated_coord_Berlin_Coccinella)
     marks(Berlin_2000_C7_2) <- NULL
     A <- quadratcount(Berlin_2000_C7_2, nx= 6, ny= 3)
   })


Q_count_C
scale()
  
Berlin_2000  <- as.ppp(separated_coord_Berlin_ab_2000)
marks(Berlin_2000) <- NULL


Q <- quadratcount(Berlin_2000, nx= 6, ny=3)
Q
Berlin_2000_C7 <- as.ppp(separated_coord_Berlin_Coccinella)
marks(Berlin_2000_C7) <- NULL
R <- quadratcount(Berlin_2000_C7, nx= 6, ny= 3)
R
R-Q

by(separated_coord_Berlin_Coccinella, separated_coord_Berlin_Coccinella$year, 
   function(x){
     Berlin_2000_C7_2 <- as.ppp(separated_coord_Berlin_Coccinella)
     marks(Berlin_2000_C7_2) <- NULL
     A <- quadratcount(Berlin_2000_C7_2, nx= 6, ny= 3)
   })

Berlin_2000_C7 <- as.ppp(separated_coord_Berlin_Coccinella)
marks(Berlin_2000_C7) <- NULL
R <- quadratcount(Berlin_2000_C7, nx= 6, ny= 3)
#noche einen Quadratcount für C7 und dann Differenz über die Jahre 
plot(Berlin_2000, main=NULL, cols=rgb(0,0,0,.2), pch=20)
plot(Berlin_2000, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE) # Add quadrat grid



class(Q)


Q+Q
#Versuch eine Backroundmap zu bekommen
install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(ggmap)
Berlin <- c(lon = 52.5200, lat = 13.4050)
Berlin_map <- get_map(location = Berlin)

# Library
library(ggmap)

# For google map, you have to give the center of the window you are looking at.
# Possibility for the map type argument: terrain / satellite / roadmap / hybrid

# get the map info
map_Deutschland <- get_googlemap("Berlin, Germany", zoom = 8, maptype = "terrain")

# Plot it
ggmap(map_Deutschland) + 
  theme_void() + 
  ggtitle("terrain") + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "grey", fill=NA, size=2)
  )


separated_coord_Berlin_ab_2000
separated_coord_Berlin_Coccinella

germany <- getData(country = "Germany", level = 1) 
?getData


#Plotted Map Deutschland Umrisse
getwd()
setwd("C:/Users/User/Documents/Bachelorarbeit/DEU_adm")
getwd()
Deutschland <- readOGR("DEU_adm0.shp")
# 'fortify' the data to get a dataframe format required by ggplot2
library(broom)
spdf_fortified_Deutschland <- tidy(Deutschland)
# Plot it
library(ggplot2)


ggplot() +
  geom_polygon(data = spdf_fortified_Deutschland, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  geom_point(data = points_within_Berlin_ab_2000_Coccinella)
theme_void()

#Versuch die Beobachtungen drauf zu plotten
ggplot() +
geom_polygon(data = spdf_fortified_Deutschland, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
theme_void() +
geom_point(data=separated_coord_Berlin_Coccinella, aes(long, lat), inherit.aes = FALSE, alpha = 0.5, size = 0.5) + coord_equal()

# Versuch die Beobachtungen drauf zu plotten der funktioniert
ggplot(spdf_fortified_Deutschland, aes(long, lat, group = group)) + 
  geom_polygon(alpha = 0.5) +
  geom_path(color = "white") +
  coord_equal() +
  scale_fill_identity() +
  theme(legend.position = "none") +
  geom_point(data=separated_coord_Berlin_ab_2000, aes(lat, long), inherit.aes = FALSE, alpha = 0.5, size = 0.5) + coord_equal() +
  facet_wrap(~year)


#Separierung der Buffer Koordinaten funktioniert nicht
separated_coord_buffer_sf <- buffer_sf_getrennt %>%
  mutate(lat = unlist(map(buffer_sf_getrennt$geometry,1)),
         long = unlist(map(buffer_sf_getrennt$geometry,2)))


occ_inBufferplot
occ_inBufferyear<- ggplot(searchbuffer, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point()+
  xlim(0, 25) +
  ylim(0, 100) +
  facet_wrap(~year)




occ_inBuffer
# contour lines
occ_inBufferyear + geom_density_2d()
# contour bands
occ_inBufferyear + geom_density_2d_filled(alpha = 0.5)
# contour bands and contour lines

ggplot(occ_inBuffer, aes(x = decimalLongitude, y = decimalLatitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = occ_inBuffer) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

st_intersects(buffer_sf, searchalleHarmonialleJahre3)
occ_inBuffer <- st_intersects(buffer_sf, searchalleHarmonialleJahre3)
print(occ_inBuffer)
point.in.polygon(buffer_sf, searchalleHarmonialleJahre3)
# transform into sf + Koordinaten umzu überprüfen ob wirklich alle Ergebnisse aus der Suchanfrage im Buffer sind
searchbuffer3 <- searchbuffer2 %>% 
  as.data.frame %>% 
  sf::st_as_sf(coords = c(2,3))
class(searchbuffer3)
st_contains(buffer_sf, searchbuffer3)
occimBuffer <- 
#plot the density
plotHarmoniaDeutschland <- ggplot(searchbuffer, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point()+
  xlim(0, 25) +
  ylim(0, 100)
plotHarmoniaDeutschland
# contour lines
plotHarmoniaDeutschland + geom_density_2d()
# contour bands
plotHarmoniaDeutschland + geom_density_2d_filled(alpha = 0.5)
# contour bands and contour lines
try <- plotHarmoniaDeutschland + geom_density_2d_filled(alpha = 0.5) +
  geom_density_2d(size = 0.25, colour = "black")
try
ggplot(searchbuffer, aes(x = decimalLongitude, y = decimalLatitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = searchbuffer) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

ggplot(searchbuffer, aes(x = decimalLongitude, y = decimalLatitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = searchbuffer) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#plot für alle Jahre
ggplot(searchbuffer, aes(x = decimalLongitude, y = decimalLatitude)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude')

#aufgegliedert nach Jahren
ggplot(searchbuffer, aes(x = decimalLongitude, y = decimalLatitude)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  facet_wrap(~year)
  


ggplot(searchbuffer, aes(x = decimalLongitude, y = decimalLatitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = searchbuffer) + 
  scale_fill_viridis_c()
  theme(legend.position = 'none')
  
  
  # Versuch die Differenz zu plotten 
  
# suchanfrage beide Spezien
  splist <- c('Harmonia axyridis', 'Coccinella septempunctata')
  keys <- sapply(splist, function(x) name_suggest(x)$data$key[1], USE.NAMES=FALSE)
 search_beide_Spezien <-occ_search( scientificName = splist, limit = 90000, country = "DE",  hasCoordinate = T)
search_beide_Spezien
searchalleHarmonialleJahre
 
beide_nach_Jahren <- table(search_beide_Spezien$year)
beide_nach_Jahren

table(separated_coord_Berlin_ab_2000$year)
table(separated_coord_Berlin_Coccinella$year)
#Versuch eine Funktion zu schreiben für die Buffer

ListederBuffer<- list(buffer_sf, Bremen_Buffer, München_Buffer, Essen_Buffer, Köln_Buffer, FrankfurtamMain_Buffer, Stuttgart_Buffer, Düsseldorf_Buffer, Leipzig_Buffer, Dortmund_Buffer, Hannover_Buffer, Nürnberg_Buffer, Leipzig_Buffer)

Versuch_Funktion_Buffer <- function(ListederBuffer) {
  points_within_the_Buffers <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, ListederBuffer, sparse = F), ]
  points_within_the_Buffers %>%
    mutate(lat = unlist(map(points_within_the_Buffers$geometry,1)),
           long = unlist(map(points_within_the_Buffers$geometry,2)))
}
  Versuch_Funktion_Buffer_plot <- function(ListederBuffer) {
    points_within_the_Buffers <- searchCoccinellaalleJahre2.2[st_within( searchCoccinellaalleJahre2.2, ListederBuffer, sparse = F), ]
  separated_coords_buffer  <- points_within_the_Buffers %>%
      mutate(lat = unlist(map(points_within_the_Buffers$geometry,1)),
             long = unlist(map(points_within_the_Buffers$geometry,2)))
  ggplot(separated_coords_buffer, aes(x = long, y = lat)) + 
    geom_point(size = 0.1, alpha = 0.05) + 
    coord_equal() + 
    xlab('Longitude') + 
    ylab('Latitude') +
    facet_wrap(~year)
}
Versuch_Funktion_Buffer(buffer_sf)
Versuch_Funktion_Buffer_plot(buffer_sf)
Versuch_Funktion_Buffer_plot(Hamburg_Buffer)


Berlin_2000  <- as.ppp(separated_coord_Berlin_ab_2000)
marks(Berlin_2000) <- NULL


Q <- quadratcount(Berlin_2000, nx= 6, ny=3)

Q

by(separated_coord_Berlin_ab_2000, separated_coord_Berlin_ab_2000$year, 
   function(x){
     P   <- as.ppp(x) 
     marks(P) <- NULL
     quadratcount(P, nx=6, ny=3)
   })

liste_separated_coords <- list(separated_coord_Berlin_ab_2000, separated_coord_Bremen, separated_coord_Dortmund, separated_coord_Dresden, separated_coord_Düsseldorf, separated_coord_Essen, separated_coord_FrankfurtmMain, separated_coord_Hamburg, separated_coord_Hannover, separated_coord_Köln, separated_coord_Leipzig, separated_coord_München, separated_coord_Nürnberg, separated_coord_Stuttgart)
by(liste_separated_coords, liste_separated_coords$year)
Quadratcount_für_Städte <- function(liste_separated_coords) {
  Städte_2000  <- as.ppp(liste_separated_coords)
  marks(Städte_2000) <- NULL
  plot(Städte_2000,pch=16,cex=0.5)
  plot(quadratcount(Städte_2000, nx = 6, ny = 6),add=T,col="red")
}
Quadratcount_für_Städte(separated_coord_Berlin_ab_2000)


Berlin_2000  <- as.ppp(separated_coord_Berlin_ab_2000)
marks(Berlin_2000) <- NULL


Q <- quadratcount(Berlin_2000, nx= 6, ny=3)
plot(Berlin_2000,pch=16,cex=0.5)
plot(quadratcount(Berlin_2000, nx = 6, ny = 6),add=T,col="red", facet_wrap(~year))

Quadratcount_für_Städte <- function(liste_separated_coords) {
  Städte_2000  <- as.ppp(liste_separated_coords)
  marks(Städte_2000) <- NULL
  quadratcount(Städte_Berlin, nx= 6, ny=3)
  facet_wrap(~year)
}
Quadratcount_für_Städte(separated_coord_Berlin_ab_2000)

class(searchalleHarmonialleJahre2.2)


#Einlesen der Daten beider Marienkäfer aller Jahre
getwd()
setwd("C:/Users/User/Documents/Bachelorarbeit/BachelorarbeitMarienkäfer/DataDeutschlandH+C")
getwd()
readfile <- read.delim("Marienkäfer Welt.csv", sep = "\t", quote = "#")
readfile


readfile_selected <- readfile %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode)

#Filter nach den Jahren ab 2000
readfile_selected2.1.1 <- data.frame(readfile_selected)

EU_Länder <-  list("DE","BE", "BG", "CZ", "DK", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU","HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE") 
readfile_selected2.1 <- readfile_selected2.1.1 %>%
  filter(year>1999) %>%
  filter(countryCode %in% EU_Länder)

readfile_selected2.2 <- readfile_selected2.1 %>% 
  as.data.frame %>% 
  sf::st_as_sf(coords = c(2,3))
readfile_selected2.2


separated_coords_Europa  <- readfile_selected2.2 %>%
  mutate(lat = unlist(map(readfile_selected2.2$geometry,1)),
         long = unlist(map(readfile_selected2.2$geometry,2)))
separated_coords_Europa

by(separated_coords_Europa, separated_coords_Europa$year)
Quadratcount_für_Europa <- function(separated_coords_Europa) {
  Europa_2000  <- as.ppp(separated_coords_Europa)
  marks(Europa_2000) <- NULL
  plot(Europa_2000,pch=16,cex=0.5)
  plot(quadratcount(Europa_2000, nx = 60, ny = 60),add=T,col="red")
}
Quadratcount_für_Europa(separated_coords_Europa)

#Die Funktionert!!

Q_count_Europa <- by(separated_coords_Europa, separated_coords_Europa$year, 
                function(x){
                  Europa_Beide_2000 <- as.ppp(x)
                  marks(Europa_Beide_2000) <- NULL
                  quadratcount(Europa_Beide_2000, nx= 10, ny= 5)
                })
Q_count_Europa
Q_count_Europa <- by(separated_coords_Europa, separated_coords_Europa$year, 
                     function(x){
                       Europa_Beide_2000 <- as.ppp(separated_coords_Europa)
                       marks(Europa_Beide_2000) <- NULL
                       Europa <- quadratcount(Europa_Beide_2000, nx= 10, ny= 5)
                     })

Filter_C7 <- readfile_selected2.2 %>%
  filter(species == "Coccinella septempunctata") %>%
  filter(year> 2000)
Filter_C7

separated_coords_C7  <- Filter_C7 %>%
  mutate(lat = unlist(map(Filter_C7$geometry,1)),
         long = unlist(map(Filter_C7$geometry,2)))
separated_coords_C7

Q_count_C7 <- by(separated_coords_C7, separated_coords_C7$year,
                 function(x){ 
                   Europa_C7 <- as.ppp(x)
                   marks(Europa_C7) <- NULL
                   count_Europa_C7 <- quadratcount(Europa_C7, nx= 10, ny= 5)
                 })
Q_count_C7


Filter_Harmonia <- readfile_selected2.2 %>%
  filter(species == "Harmonia axyridis")
Filter_Harmonia

separated_coords_Harmonia  <- Filter_Harmonia %>%
  mutate(lat = unlist(map(Filter_Harmonia$geometry,1)),
         long = unlist(map(Filter_Harmonia$geometry,2)))
separated_coords_Harmonia

Filter_Harmonia_ab_2001 <- readfile_selected2.2 %>%
  filter(species == "Harmonia axyridis") %>%
  filter(year > 2000)
Filter_Harmonia_ab_2001
separated_coords_Harmonia_ab_2001  <- Filter_Harmonia_ab_2001 %>%
  mutate(lat = unlist(map(Filter_Harmonia_ab_2001$geometry,1)),
         long = unlist(map(Filter_Harmonia_ab_2001$geometry,2)))
separated_coords_Harmonia_ab_2000

Q_count_Harmonia <- by(separated_coords_Harmonia_ab_2001, separated_coords_Harmonia_ab_2001$year,
                       function(x){
                         Europa_Harmonia <- as.ppp(x)
                         marks(Europa_Harmonia) <- NULL
                         count_Europa_Harmonia <- quadratcount(Europa_Harmonia, nx= 10, ny= 5 )
             
                       })
Q_count_Harmonia
#einfach von einander abziehen klappt leider noch nicht

Q_count_C7 - Q_count_Harmonia



Europa_Harmonia  <- as.ppp(separated_coords_Harmonia)
marks(Europa_Harmonia) <- NULL


H <- quadratcount(Europa_Harmonia, nx= 6, ny=3)
H

#Plott von beiden aufeinander
rbind(data.frame(separated_coord_Berlin_ab_2000, group="H. axyridis"), data.frame(separated_coord_Berlin_Coccinella, group="C7")) %>%
  filter(year==2019) %>%
  ggplot(aes(x=long,y=lat)) + 
  stat_density2d( aes(color = group)) + 
  # scale_fill_manual(values=c("a"="#FF0000", "b"="#00FF00")) +
  theme_minimal() +
  geom_point(aes(color = group, alpha=0.5))
  
#Plott von beiden aufeinander Berlin
rbind(data.frame(separated_coord_Berlin_ab_2000, group="H. axyridis"), data.frame(separated_coord_Berlin_Coccinella, group="C7")) %>%
  ggplot(aes(x=long,y=lat)) + 
  stat_density2d( aes(color = group)) + 
  # scale_fill_manual(values=c("a"="#FF0000", "b"="#00FF00")) +
  theme_minimal() +
  geom_point(aes(color = group, alpha=0.5))+
  facet_wrap(~year)

#Plott von beiden aufeinander
rbind(data.frame(separated_coord_Bremen, group="H. axyridis"), data.frame(separated_coord_Bremen_Coccinella, group="C7")) %>%
  ggplot(aes(x=long,y=lat)) + 
  stat_density2d( aes(color = group)) + 
  # scale_fill_manual(values=c("a"="#FF0000", "b"="#00FF00")) +
  theme_minimal() +
  geom_point(aes(color = group, alpha=0.5))+
  facet_wrap(~year)




#Differenz
Q_count_C7[[20]] - Q_count_Harmonia[[20]]
my_diff <- lapply(seq_along(Q_count_C7), function(i){
 # diff <- as.data.frame(Q_count_C7[[i]] - Q_count_Harmonia[[i]])
#  diff$year <- names(Q_count_C7)[i] 
 # diff
  Q_count_C7[[i]] - Q_count_Harmonia[[i]]
})
diff_pro_Jahr <- do.call(rbind, my_diff)
my_diff
#plot der Differenz 
?quadratcount()

my_diff[[20]]
str(my_diff[[20]])
rownames(my_diff[[20]])


as_tibble(diff_pro_Jahr) %>%
  filter(Freq != 0) %>%
  transform(log_diff = log10(abs(Freq))) %>%
  transform(log_diff = ifelse(Freq < 0, log_diff * (-1), log_diff)) %>%
ggplot(aes(x=year, y=log_diff, group=paste(x,y))) +
  geom_jitter() +
  geom_line()
 
ggplot(diff_pro_Jahr, aes(x=year, y=Freq)) +
  geom_boxplot() 


table()
diff_pro_Jahr
table(paste(diff_pro_Jahr$x, diff_pro_Jahr$y))

#Erstellung der für alle gültigen grids
filter_beide_Arten_2001<- readfile_selected2.2 %>%
  filter(year > 2000)


separated_coords_beide  <- filter_beide_Arten_2001 %>%
  mutate(lat = unlist(map(filter_beide_Arten_2001$geometry,1)),
         long = unlist(map(filter_beide_Arten_2001$geometry,2)))
separated_coords_beide
summary(separated_coords_beide)

Europa_beide_ab_2001  <- as.ppp(separated_coords_beide)
marks(Europa_beide_ab_2001) <- NULL


Q_count_beide_Arten_ab_2001<- quadratcount(Europa_beide_ab_2001, nx=10, ny=5)
Q_count_beide_Arten_ab_2001

x_grid_beide <- colnames(Q_count_beide_Arten_ab_2001)
y_grid_beide <- rownames(Q_count_beide_Arten_ab_2001)
x_grid_beide
y_grid_beide

list_x_grid <- as.list(x_grid_beide)
list_x_grid
grids_Käfer <- list_x_grid[c(1:9)]

add_grid <- "[27.9,34.1]"
x_grid_beide_abgeändert <- c(grids_Käfer, add_grid)
x_grid_beide_abgeändert <- as.character(x_grid_beide_abgeändert)

x_grid_beide_abgeändert <- as.vector(x_grid_beide_abgeändert)
class(x_grid_beide_abgeändert)
summary(separated_coords_Harmonia_ab_2001)

#Versuch beide in den grids zu plotten

count_Europa_beide <- quadratcount(Europa_beide, xbreaks = x_grid_beide, ybreaks = y_grid_beide) by(separated_coords_beide, separated_coords_beide$year,
                       function(x){
                         Europa_beide <- as.ppp(x)
                         marks(Europa_beide) <- NULL
                         count_Europa_beide <- quadratcount(Europa_beide, xbreaks = x_grid_beide, ybreaks = y_grid_beide)
                         
                       })
Q_count_beide

my_quadrat_count <- function(x){
  Europa_beide <- as.ppp(x)
  marks(Europa_beide) <- NULL
  count_Europa_beide <- quadratcount(Europa_beide, xbreaks = x_grid_richtig, ybreaks = y_grid_richtig)
}


my_quadrat_count(filter(Europa_beide_ab_2001, year=2018))

summary(separated_coords_beide)

Q_count_beide <- by(separated_coords_beide, separated_coords_beide$year,
                    function(x){
                      Europa_beide <- as.ppp(x)
                      marks(Europa_beide) <- NULL
                      count_Europa_beide <- quadratcount(Europa_beide, nx=10,ny=5)
                      
                    })

Q_count_beide
#versuch eine Spezies in den Grids zu plotten aber er gibt mir den Fehler, dass das Window nicht passt

Q_count_Harmonia_in_grids_1 <- by(separated_coords_Harmonia_ab_2001, separated_coords_Harmonia_ab_2001$year,
                                function(x){
                                  Europa_Harmonia_in_grids_1 <- as.ppp(x)
                                  marks(Europa_Harmonia_in_grids_1) <- NULL
                                  count_Europa_Harmonia_in_grids_1 <- quadratcount(Europa_Harmonia_in_grids_1, xbreaks=x_grid_richtig, ybreaks=y_grid_richtig)
                                  
                                })
Q_count_Harmonia_in_grids_1
Q_count_Coccinella_in_grids_1 <- by(separated_coords_C7, separated_coords_C7$year,
                                  function(x){
                                    Europa_Coccinella_in_grids_1 <- as.ppp(x)
                                    marks(Europa_Coccinella_in_grids_1) <- NULL
                                    count_Europa_Coccinella_in_grids_1 <- quadratcount(Europa_Coccinella_in_grids_1, xbreaks=x_grid_richtig, ybreaks=y_grid_richtig)
                                    
                                  })
Q_count_Coccinella_in_grids_1
#Wenn ich die grids manuell festlege und das letzte größer mache passt es
Q_count_Harmonia_in_grids <- by(separated_coords_Harmonia_ab_2001, separated_coords_Harmonia_ab_2001$year,
                       function(x){
                         Europa_Harmonia_in_grids <- as.ppp(x)
                         marks(Europa_Harmonia_in_grids) <- NULL
                         count_Europa_Harmonia_in_grids <- quadratcount(Europa_Harmonia_in_grids, xbreaks=c(-27.4, -21.2, -15.1, -8.94, -2.8, 3.33, 9.47, 15.6, 21.7, 27.9, 34.03), ybreaks=c(28.1, 36.1, 44.1, 52, 60, 68))
                         
                       })


y_grid_beide
summary(separated_coords_Harmonia_ab_2001)
summary(separated_coords_beide)
Q_count_Harmonia_in_grids

Q_count_Coccinella_in_grids <- by(separated_coords_C7, separated_coords_C7$year,
                                function(x){
                                  Europa_Coccinella_in_grids <- as.ppp(x)
                                  marks(Europa_Coccinella_in_grids) <- NULL
                                  count_Europa_Coccinella_in_grids <- quadratcount(Europa_Coccinella_in_grids, xbreaks=c(-27.4, -21.2, -15.1, -8.94, -2.8, 3.33, 9.47, 15.6, 21.7, 27.9, 34.03), ybreaks=c(28.1, 36.1, 44.1, 52, 60, 70))
                                  
                                })
Q_count_Coccinella_in_grids

#Anteil C7 an allen C7 + Harmonia
diff_C7_Harmonia <- lapply(seq_along(Q_count_Coccinella_in_grids_1), function(i){
  # diff <- as.data.frame(Q_count_C7[[i]] - Q_count_Harmonia[[i]])
  #  diff$year <- names(Q_count_C7)[i] 
  # diff
  Q_count_Coccinella_in_grids_1[[i]]/( Q_count_Coccinella_in_grids_1[[i]] + Q_count_Harmonia_in_grids_1[[i]])
})

diff_C7_Harmonia <- lapply(seq_along(Q_count_Coccinella_in_grids_1), function(i){
   diff <- as.data.frame(Q_count_Coccinella_in_grids_1[[i]] - Q_count_Harmonia_in_grids_1[[i]])
   diff$year <- names(Q_count_C7)[i] 
  diff
 
})

diff_C7_Harmonia 
diff_pro_Jahr_C7_Harmonia_1 <- do.call(rbind, diff_C7_Harmonia)
diff_pro_Jahr_C7_Harmonia_1
#übersichtlicher

diff_C7_Harmonia_2 <- lapply(seq_along(Q_count_Coccinella_in_grids_1), function(i){
  g <- Q_count_Coccinella_in_grids_1[[i]] + Q_count_Harmonia_in_grids_1[[i]]
  diff <- as.data.frame(Q_count_Coccinella_in_grids_1[[i]]/g)
   diff$year <- names(Q_count_Coccinella_in_grids)[i] 
  diff$total <- as.vector(g)
 diff$diff <- as.vector(Q_count_Coccinella_in_grids_1[[i]] - Q_count_Harmonia_in_grids_1[[i]])
  diff
})

diff_pro_Jahr_C7_Harmonia <- do.call(rbind, diff_C7_Harmonia_2)
diff_pro_Jahr_C7_Harmonia
diff_C7_Harmonia_2[1]

as_tibble(diff_pro_Jahr_C7_Harmonia_1) %>%
  filter(Freq != 0) %>%
  transform(log_diff = log10(abs(Freq))) %>%
  transform(log_diff = ifelse(Freq < 0, log_diff * (-1), log_diff)) %>%
  ggplot(aes(x=year, y=log_diff, group=paste(x,y))) +
  geom_jitter() +
  geom_line()


diff_C7_Harmonia_2
diff_C7_Harmonia_pro_Jahr <- do.call(rbind, diff_C7_Harmonia_2)
diff_C7_Harmonia_pro_Jahr

table(diff_C7_Harmonia_pro_Jahr$Freq)
diff_C7_Harmonia_pro_Jahr_Freq <- diff_C7_Harmonia_pro_Jahr %>%
  filter(Freq != 0) %>%
  group_by(Freq)

diff_C7_Harmonia_pro_Jahr_Freq


library(vioplot)
vioplot(diff_C7_Harmonia_pro_Jahr$Freq,
        horizontal = TRUE)
?vioplot

as_tibble(diff_C7_Harmonia_pro_Jahr) %>%
  filter(Freq != 0) %>%
  transform(log_diff = log10(abs(Freq))) %>%
  transform(log_diff = ifelse(Freq < 0, log_diff * (-1), log_diff)) %>%
  ggplot(aes(x=year, y=log_diff, group=paste(x,y))) +
  geom_jitter() +
  geom_line()

as_tibble(diff_C7_Harmonia_pro_Jahr) %>%
  filter(Freq != 0) %>%
  ggplot(aes(x=year, y=Freq, group=paste(x,y))) +
  geom_jitter() +
  geom_line()



Europa_Harmonia_in_grids <- as.ppp(separated_coords_Harmonia_ab_2001)
marks(Europa_Harmonia_in_grids) <- NULL
count_Europa_Harmonia_in_grids <- quadratcount(Europa_Harmonia_in_grids, xbreaks=x_grid_beide, ybreaks=y_grid_beide)


#Count für Deutschland

readfile_selected3 <- readfile_selected2.1.1 %>%
  filter(year>2000) %>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude))%>%
  filter(countryCode=="DE")
readfile_selected3

readfile_selected3.1 <- readfile_selected3 %>% 
  as.data.frame %>% 
  sf::st_as_sf(coords = c(2,3))
readfile_selected3.1

separated_coords_Deutschland_beide  <- readfile_selected3 %>%
  mutate(lat = unlist(map(readfile_selected3$geometry,1)),
         long = unlist(map(readfile_selected3$geometry,2)))
separated_coords_Deutschland_beide


Deutschland_beide_ab_2001  <- as.ppp(separated_coords_Deutschland_beide)
marks(Deutschland_beide_ab_2001) <- NULL


Q_count_beide_Arten_ab_2001<- quadratcount(Europa_beide_ab_2001, nx= 100, ny=50)
Q_count_beide_Arten_ab_2001

x_grid_beide <- rownames(Q_count_beide_Arten_ab_2001)
y_grid_beide <- colnames(Q_count_beide_Arten_ab_2001)


summary(separated_coords_Europa)

#load the dataset
#put the coords in meters
separated_coords_Europa$X <- separated_coords_Europa$lat/100
separated_coords_Europa$Y <- separated_coords_Europa$long/100

#creating the point pattern
(all_pp <- ppp(separated_coords_Europa[,"X"],separated_coords_Europa[,"Y"],owin(c(-28,35),c(28,68))))
readfile_selected




#Versuch einen Vektor für die Coordinaten zu generieren
x=c()
x[1] = -27.4
for(i in 2:11) {
  x[i]=6.2+x[i-1]
}
x
as.numeric(x)
class(x)

#Density Berlin beide
points_within_Berlin_ab_2001_beide <- readfile_selected3.1[st_within(readfile_selected3.1, buffer_sf, sparse = F), ]
points_within_Berlin_ab_2001_beide 
separated_coords_Berlin_beide  <- points_within_Berlin_ab_2000_beide %>%
  mutate(lat = unlist(map(points_within_Berlin_ab_2000_beide$geometry,1)),
         long = unlist(map(points_within_Berlin_ab_2000_beide$geometry,2)))
separated_coords_Berlin_beide

Berlin_beide_ab_2001  <- as.ppp(separated_coords_Berlin_beide)
marks(Berlin_beide_ab_2001) <- NULL


Q_count_beide_Arten_ab_2001_Berlin<- quadratcount(Berlin_beide_ab_2001, nx=10, ny=5)
Q_count_beide_Arten_ab_2001_Berlin

Filter_Harmonia_ab_2001_Berlin <- points_within_Berlin_ab_2000_beide %>%
  filter(species == "Harmonia axyridis") %>%
  filter(year > 2000)
Filter_Harmonia_ab_2001_Berlin
separated_coords_Harmonia_ab_2001_Berlin  <- Filter_Harmonia_ab_2001_Berlin %>%
  mutate(lat = unlist(map(Filter_Harmonia_ab_2001_Berlin$geometry,1)),
         long = unlist(map(Filter_Harmonia_ab_2001_Berlin$geometry,2)))
separated_coords_Harmonia_ab_2001_Berlin


Q_count_Harmonia_in_grids_Berlin <- by(separated_coords_Harmonia_ab_2001_Berlin, separated_coords_Harmonia_ab_2001_Berlin$year,
                                  function(x){
                                    Berlin_Harmonia_in_grids <- as.ppp(x)
                                    marks(Berlin_Harmonia_in_grids) <- NULL
                                    count_Berlin_Harmonia_in_grids <- quadratcount(Berlin_Harmonia_in_grids, xbreaks=c(13.20, 13.25, 13.28, 13.32, 13.36, 13.4,13.44,13.48, 13.6),ybreaks=c(52.0, 52.41, 52.49, 52.56, 52.64, 52.9))
                                                                            
                                    
                                  })
Q_count_Harmonia_in_grids_Berlin

Filter_C7_ab_2001_Berlin <- points_within_Berlin_ab_2000_beide %>%
  filter(species == "Coccinella septempunctata") %>%
  filter(year > 2000)
Filter_C7_ab_2001_Berlin
separated_coords_C7_ab_2001_Berlin  <- Filter_C7_ab_2001_Berlin %>%
  mutate(lat = unlist(map(Filter_C7_ab_2001_Berlin$geometry,1)),
         long = unlist(map(Filter_C7_ab_2001_Berlin$geometry,2)))
separated_coords_C7_ab_2001_Berlin


Q_count_C7_in_grids_Berlin <- by(separated_coords_C7_ab_2001_Berlin, separated_coords_C7_ab_2001_Berlin$year,
                                       function(x){
                                         Berlin_C7_in_grids <- as.ppp(x)
                                         marks(Berlin_C7_in_grids) <- NULL
                                         count_Berlin_C7_in_grids <- quadratcount(Berlin_C7_in_grids, xbreaks=c(13.20, 13.25, 13.28, 13.32, 13.36, 13.4,13.44,13.48, 13.6),ybreaks=c(52.0, 52.41, 52.49, 52.56, 52.64, 52.9))
                                         
                                         
                                       })
Q_count_C7_in_grids_Berlin

Q_count_Harmonia_in_grids_Berlin

#funktioniert nicht, weil wenn es für Jahre keine Daten gibt für das andere aber schon, kann es nicht voneinander abgezogen werdena.frame(Q_count_C7[[i]] - Q_count_Harmonia[[i]])
diff_C7_Harmonia_Berlin_1 <- lapply(seq_along(Q_count_C7_in_grids_Berlin), function(i){
#  diff$year <- names(Q_count_C7)[i] 
  # diff
  Q_count_C7_in_grids_Berlin[[i]] - Q_count_Harmonia_in_grids_Berlin[[i]]
})

diff_C7_Harmonia_Berlin

#übersichtlicher

diff_C7_Harmonia_Berlin <- lapply(seq_along(Q_count_C7_in_grids_Berlin), function(i){
  diff <- as.data.frame(Q_count_C7_in_grids_Berlin[[i]] - Q_count_Harmonia_in_grids_Berlin[[i]])
  diff$year <- names( Q_count_C7_in_grids_Berlin)[i] 
  diff
  
})



diff_C7_Harmonia_2
diff_C7_Harmonia_pro_Jahr <- do.call(rbind, diff_C7_Harmonia_2)
diff_C7_Harmonia_pro_Jahr


#numerischer Vektor x_grid

x_grid_beide_1 <- gsub("\\[|\\)|\\]", "", x_grid_beide)
x_grid_richtig <- as.numeric(unique(unlist(strsplit(x_grid_beide_1, ","))))

y_grid_beide_1 <- gsub("\\[|\\)|\\]", "", y_grid_beide)
y_grid_richtig <- sort(as.numeric(unique(unlist(strsplit(y_grid_beide_1, ",")))))
x_grid_richtig[x_grid_richtig == max(x_grid_richtig)] <- x_grid_richtig[x_grid_richtig == max(x_grid_richtig)] + 0.5
x_grid_richtig

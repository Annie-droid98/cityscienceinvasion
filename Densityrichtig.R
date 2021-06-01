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

by(separated_coord_Berlin_ab_2000, separated_coord_Berlin_ab_2000$year, 
   function(x){
  P   <- as.ppp(x) 
  marks(P) <- NULL
  quadratcount(P, nx=6, ny=3)
   })

#selbe Funktion funktioniert hier nicht
by(separated_coord_Bremen_Coccinella, separated_coord_Bremen_Coccinella$year,
   function(y){
     R <- as.ppp(y)
     marks(R) <- NULL
     quadratcount(R, nx=6, ny=3)
   })



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
register_google(key = "AIzaSyBH9ZncAfoSpsIbfLeL5Xbf9U0kkMY8zo8", write = TRUE)


AIzaSyDby8bdbN4Jt85W5bQrFnjJXNtsO1_YFrU
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

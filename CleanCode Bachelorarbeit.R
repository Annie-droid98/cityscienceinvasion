install.packages("rgbif")
install.packages("sf")
install.packages("dplyr")
install.packages("sp")
install.packages("wellknown")
install.packages("ggplot2")
install.packages("leaflet")
install.packages("ggmap")
install.packages("vroom")
install.packages("MASS")
library(data.table)
library(vroom)
library(ggmap)
library(ggplot2)
library(wellknown)
library(rgbif)
library(dplyr)
library(sf)
library(leaflet)
library(maps)
library(rgeos)
library(tidyverse)
library(maptools)
library(raster)
library(sf)
library(leaflet)
install.packages("spatstat")
library(spatstat)
library(rgdal)
library(broom)
library(MASS)


Mittelpunkte_der_Städte_Liste <- list(c(52.5200,13.4050),c(53.5511,9.9937),c(48.1351,11.5820),c(50.9375,6.9603),c(50.1109,8.6821),c(48.7758,9.1829),c(51.2277,6.7735),
                                      c(51.3397,12.3731),c(51.5136,7.4653),c(51.4556,7.0116),c(53.0793,8.8017),c(51.0504,13.7373),c(52.3759,9.7320),c(49.4521,11.0767))



#Funktion um alle Buffer zu erstellen muss aber hier noch auf einen Radius in km/m umstellen
Buffer_für_die_Städte <- lapply((Mittelpunkte_der_Städte_Liste), function(i){
  example_points <- data.frame(lat=numeric(), long= numeric())
  example_points[1,] <- c(i)
  coordinates(example_points) <- ~long + lat
  example_pointssf <- as(example_points, "sf")
  buffer_sf <- st_buffer(example_pointssf,  dist = 0.2, endCapStyle="ROUND")
  
})

#Plot um zu schauen ob es funktioniert hat
Buffer_für_die_Städte[[1]]%>%
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data=Buffer_für_die_Städte[[1]])

#Funktion um die Beobachtungen in den Buffern zu erfassen + später plotten der density
Versuch_Funktion_Buffer <- function() {
  points_within_the_Buffers <- readfile_selected2.2[st_within( readfile_selected2.2, Buffer_für_die_Städte, sparse = F), ]
  points_within_the_Buffers %>%
    mutate(lat = unlist(map(points_within_the_Buffers$geometry,1)),
           long = unlist(map(points_within_the_Buffers$geometry,2)))
}
Versuch_Funktion_Buffer()

#Einlesen der Tabelle

readfile <- vroom::vroom("0328735-200613084148143.csv",quote="")
  #read_tsv("0328735-200613084148143.csv",quote="")
readfile
readfile_selected <- readfile %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, taxonKey, class)


#Filter nach den Jahren ab 2000
readfile_selected2.1.1 <- data.frame(readfile_selected)
readfile_selected2.1.1


#EU_Länder <-  list("DE","BE", "BG", "CZ", "DK", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU","HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE") 
readfile_selected2.1 <- readfile_selected2.1.1 %>%
  filter(year>1999)%>%
  filter((! is.na(decimalLatitude)))%>%
  filter((! is.na(decimalLongitude)))
  
readfile_selected2.1

readfile_selected2.2 <- readfile_selected2.1 %>% 
  as.data.frame %>% 
  sf::st_as_sf(coords = c(2,3))
readfile_selected2.2

#Filter für C7 Beobachtungen
Filter_C7 <- readfile_selected2.2 %>%
  filter(species == "Coccinella septempunctata") %>%
  filter(year> 2000)
Filter_C7

#Separierung der Coordinaten
separated_coords_C7  <- Filter_C7 %>%
  mutate(lat = unlist(map(Filter_C7$geometry,1)),
         long = unlist(map(Filter_C7$geometry,2)))
separated_coords_C7

#Filter Harmoniabeobachtungen

Filter_Harmonia_ab_2001 <- readfile_selected2.2 %>%
  filter(species == "Harmonia axyridis") %>%
  filter(year > 2000)
Filter_Harmonia_ab_2001
separated_coords_Harmonia_ab_2001  <- Filter_Harmonia_ab_2001 %>%
  mutate(lat = unlist(map(Filter_Harmonia_ab_2001$geometry,1)),
         long = unlist(map(Filter_Harmonia_ab_2001$geometry,2)))
separated_coords_Harmonia_ab_2000

#Erstellung der für alle gültigen grids
filter_beide_Arten_2001<- readfile_selected2.2 %>%
  filter(year > 2000)


separated_coords_beide  <- filter_beide_Arten_2001 %>%
  mutate(lat = unlist(map(filter_beide_Arten_2001$geometry,1)),
         long = unlist(map(filter_beide_Arten_2001$geometry,2)))
separated_coords_beide


Europa_beide_ab_2001  <- as.ppp(separated_coords_beide)
marks(Europa_beide_ab_2001) <- NULL


Q_count_beide_Arten_ab_2001<- quadratcount(Europa_beide_ab_2001, nx=10, ny=10)
Q_count_beide_Arten_ab_2001

x_grid_beide <- colnames(Q_count_beide_Arten_ab_2001)
y_grid_beide <- rownames(Q_count_beide_Arten_ab_2001)

#Anpassen um sie als grids in der Funktion verwenden zu können 

x_grid_beide_1 <- gsub("\\[|\\)|\\]", "", x_grid_beide)
x_grid_richtig <- as.numeric(unique(unlist(strsplit(x_grid_beide_1, ","))))

y_grid_beide_1 <- gsub("\\[|\\)|\\]", "", y_grid_beide)
y_grid_richtig <- sort(as.numeric(unique(unlist(strsplit(y_grid_beide_1, ",")))))
x_grid_richtig[x_grid_richtig == max(x_grid_richtig)] <- x_grid_richtig[x_grid_richtig == max(x_grid_richtig)] + 0.5
x_grid_richtig
y_grid_richtig

#Q_count für die beiden Arten mit denselben Grids

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

# Funktion um die Differenz, total zu erhalten

diff_C7_Harmonia_2 <- lapply(seq_along(Q_count_Coccinella_in_grids_1), function(i){
  g <- Q_count_Coccinella_in_grids_1[[i]] + Q_count_Harmonia_in_grids_1[[i]]
  diff <- as.data.frame(Q_count_Coccinella_in_grids_1[[i]]/g)
  diff$year <- names(Q_count_Coccinella_in_grids_1)[i] 
  diff$total <- as.vector(g)
  diff$diff <- as.vector(Q_count_Coccinella_in_grids_1[[i]] - Q_count_Harmonia_in_grids_1[[i]])
  diff
})

diff_pro_Jahr_C7_Harmonia <- do.call(rbind, diff_C7_Harmonia_2)
diff_pro_Jahr_C7_Harmonia


#Filter ohne die NaN
diff_pro_Jahr_C7_Harmonia_without_NaN <- diff_pro_Jahr_C7_Harmonia %>%
  filter(! is.na(Freq))
diff_pro_Jahr_C7_Harmonia_without_NaN


#plotten Frequenz
ggplot(diff_pro_Jahr_C7_Harmonia_without_NaN, aes(x=year, y=Freq)) +
  geom_point() +
  facet_wrap(~year)
ggplot(diff_pro_Jahr_C7_Harmonia_without_NaN, aes(x=as.numeric(year), y=Freq)) +
  geom_jitter()+
  stat_smooth()

#plotten diff C7/Harmonia
ggplot(diff_pro_Jahr_C7_Harmonia_without_NaN, aes(x=year, y=diff)) +
  geom_point() +
  facet_wrap(~year)

#beste Grafik!
ggplot(diff_pro_Jahr_C7_Harmonia_without_NaN, aes(x=as.numeric(year), y=diff)) +
  geom_jitter()+
  stat_smooth()
#Diff mit log Transformation  der y Achse
as_tibble(diff_pro_Jahr_C7_Harmonia_without_NaN) %>%
  filter(total != 0) %>%
  transform(log_diff = log10(abs(diff))) %>%
  transform(log_diff = ifelse(diff < 0, log_diff * (-1), log_diff)) %>%
  ggplot(aes(x=year, y=log_diff, group=paste(x,y))) +
  geom_jitter() +
  geom_line()

#plotten der Hintergund map um die grids zu visualisieren
us <- c(left = -30, bottom = 20, right = 60, top = 75)
map_europa <- get_stamenmap(us, zoom = 2, maptype = "toner-lite") %>% ggmap() 
#map mit grids
map_europa + geom_hline(yintercept = y_grid_richtig) + geom_vline(xintercept = x_grid_richtig)



# FUnktion um über verschiedene Anzahlen an grids zu loopen
#ist noch sehr sperrig und muss noch runtergebrochen werden
function_Veränderung_grids_1 <- lapply(c(10,20,30), function(i) 
{Europa_beide_Käfer_in_grids_1 <- as.ppp(separated_coords_beide)
marks(Europa_beide_Käfer_in_grids_1) <- NULL
count_Europa_beide_Käfer_in_grids_1 <- quadratcount(Europa_beide_Käfer_in_grids_1, nx=i, ny=i)
x_grid_beide_experiment <- colnames(count_Europa_beide_Käfer_in_grids_1)

y_grid_beide_experiment <- rownames(count_Europa_beide_Käfer_in_grids_1)

x_grid_beide_ex_1 <- gsub("\\[|\\)|\\]", "", x_grid_beide_experiment)
x_grid_richtig_ex_1 <- as.numeric(unique(unlist(strsplit(x_grid_beide_ex_1, ","))))
x_grid_richtig_ex_1[x_grid_richtig_ex_1 == max(x_grid_richtig_ex_1)] <- x_grid_richtig_ex_1[x_grid_richtig_ex_1 == max(x_grid_richtig_ex_1)] + 0.5

y_grid_beide_ex_1 <- gsub("\\[|\\)|\\]", "", y_grid_beide_experiment)
y_grid_richtig_ex_1 <- sort(as.numeric(unique(unlist(strsplit(y_grid_beide_ex_1, ",")))))

Q_count_Coccinella_in_grids_1 <- by(separated_coords_C7, separated_coords_C7$year,
                                    function(x){
                                      Europa_Coccinella_in_grids_1 <- as.ppp(x)
                                      marks(Europa_Coccinella_in_grids_1) <- NULL
                                      count_Europa_Coccinella_in_grids_1 <- quadratcount(Europa_Coccinella_in_grids_1, xbreaks=x_grid_richtig_ex_1, ybreaks=y_grid_richtig_ex_1)
                                      
                                    })

Q_count_Harmonia_in_grids_1 <- by(separated_coords_Harmonia_ab_2001, separated_coords_Harmonia_ab_2001$year,
                                  function(x){
                                    Europa_Harmonia_in_grids_1 <- as.ppp(x)
                                    marks(Europa_Harmonia_in_grids_1) <- NULL
                                    count_Europa_Harmonia_in_grids_1 <- quadratcount(Europa_Harmonia_in_grids_1, xbreaks=x_grid_richtig_ex_1, ybreaks=y_grid_richtig_ex_1)
                                    
                                  })
diff_C7_Harmonia_2 <- lapply(seq_along(Q_count_Coccinella_in_grids_1), function(i){
  g <- Q_count_Coccinella_in_grids_1[[i]] + Q_count_Harmonia_in_grids_1[[i]]
  diff <- as.data.frame(Q_count_Coccinella_in_grids_1[[i]]/g)
  diff$year <- names(Q_count_Coccinella_in_grids_1)[i] 
  diff$total <- as.vector(g)
  diff$diff <- as.vector(Q_count_Coccinella_in_grids_1[[i]] - Q_count_Harmonia_in_grids_1[[i]])
  diff
})
})
function_Veränderung_grids_1 
Veränderung_in_verschiedenen_grids<- do.call(rbind, function_Veränderung_grids_1 )
Veränderung_in_verschiedenen_grids

#Umformung in einen dataframe
df_verschiedene_grids <- do.call(rbind.data.frame, Veränderung_in_verschiedenen_grids)
df_verschiedene_grids
class(df_verschiedene_grids)

#herausfiltern der NAN
df_verschiedene_grids_without_NaN <- df_verschiedene_grids %>%
  filter(! is.na(x)) %>%
  filter(! is.na(y)) %>%
  filter(! is.na(Freq))

df_verschiedene_grids_without_NaN


#Versuch Erstellung der grids in einem in meter transformierten CRS
Filter_beide_coords_2001 <- readfile_selected2.2 %>%
  filter(year> 2000)%>%
  mutate(lat = unlist(map(geometry,1)),
         long = unlist(map(geometry,2)))%>%
  st_set_crs(4326)%>%
  st_transform(5643)

  

Filter_C7_separated_coords_2001 <- readfile_selected2.2 %>%
  filter(species == "Coccinella septempunctata") %>%
  filter(year> 2000)%>%
  mutate(lat = unlist(map(geometry,1)),
         long = unlist(map(geometry,2)))%>%
  st_set_crs(4326)%>%
  st_transform(5643)

Filter_Harmonia_separated_coords_2001 <- readfile_selected2.2 %>%
  filter(species == "Harmonia axyridis") %>%
  filter(year> 2000)%>%
  mutate(long = unlist(map(geometry,2)),
         lat = unlist(map(geometry,1)))%>%
  st_set_crs(4326)%>%
  st_transform(5643)
Filter_Harmonia_separated_coords_2001
#Die Transformation funktioniert ist aber sehr umständlich

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

#richtiges
Versuch_grids_transformiert <- Polygon_europa_transformiertes_crs_tess %>%
  st_make_grid(cellsize = 10000, flat_topped = TRUE) %>%
  st_intersection(Polygon_europa_transformiertes_crs_tess)%>%
  st_cast("MULTIPOLYGON")%>%
  st_as_sf()%>%
  mutate(id = row_number())
Versuch_grids_transformiert
plot(Versuch_grids_transformiert$x)

st_crs(Versuch_grids_transformiert) <- st_crs(Filter_Harmonia_separated_coords_2001)


#Harmonia richtig
Europa_grid <- Versuch_grids_transformiert %>%
  st_join(st_sf(Filter_beide_coords_2001)) %>%
  group_by(species, year, id) %>%
  summarize(Anzahl_Harmonia = n(species = "Harmonia axyridis"),
                                Anzahl_C7 = n(species = "Coccinella septempunctata"))
Europa_grid
plot(Europa_grid["Anzahl_Harmonia"])
Filter_Harmonia_2001_transformiert$geometry

Harmonia_grid <- Versuch_grids_transformiert %>%
  st_join(st_sf(Filter_Harmonia_separated_coords_2001)) %>%
  group_by(year, id) %>%
  summarize(Anzahl_Harmonia = n())
Harmonia_grid

#Grid mit C7

C7_grid <- Versuch_grids_transformiert %>%
  st_join(st_sf(Filter_C7_separated_coords_2001)) %>%
  group_by(year, id) %>%
  summarize(Anzahl_C7 = n())
C7_grid
plot(C7_grid["Anzahl_C7"])


#Grid mit allen Insekten

Insecta_grid <- Versuch_grids_transformiert %>%
  st_join(st_sf(Filter_beide_coords_2001)) %>%
  group_by(year, id) %>%
  summarize(Anzahl_Insecta = n())

st_geometry(C7_grid) <- NULL
st_geometry(Harmonia_grid) <- NULL
st_geometry(Insecta_grid) <- NULL


#funktioniert noch nicht richtig
Marienkäfer_data <- left_join(Harmonia_grid, C7_grid, group_by = "id")
Marienkäfer_data[is.na(Marienkäfer_data)] <- 0

Marienkäfer_2 <-Marienkäfer_data%>%
  filter(year != 0)

Insecta_und_Marienkäfer <- left_join(Insecta_grid, Harmonia_grid, group_by = "id")
Insecta_und_Marienkäfer
Insecta_und_Marienkäfer[is.na(Insecta_und_Marienkäfer)] <- 0
Insecta_Marienkäfer_2 <-Insecta_und_Marienkäfer%>%
  filter(year != 0)
Insecta_Marienkäfer_2

Insecta_und_Marienkäfer_alle <- left_join(Insecta_Marienkäfer_2, C7_grid, group_by = "id")
Insecta_und_Marienkäfer_alle[is.na(Insecta_und_Marienkäfer_alle)] <- 0
Insecta_Marienkäfer_alle_2 <-Insecta_und_Marienkäfer_alle%>%
  filter(year != 0)
Insecta_Marienkäfer_alle_2

install.packages("glmmTMB")
library(glmmTMB)

glm_Raster <- glmmTMB(formula = Anzahl_C7 ~  log(1+Anzahl_Insecta) * log(1+Anzahl_Harmonia),
                            data = Insecta_Marienkäfer_alle_2,
                            family=nbinom1)
glm_Raster

Marienkäfer_ohne_NA <- Marienkäfer_data%>%
  #filter(! is.na(year.y))%>%
#filter(! is.na(year.x))
Marienkäfer_ohne_NA
#mit weniger grids zum ausprobieren der korrekten Syntax
Versuch_grids_transformiert_1000km <- Polygon_europa_transformiertes_crs_tess %>%
  st_make_grid(cellsize = 1000000, flat_topped = TRUE) %>%
  st_intersection(Polygon_europa_transformiertes_crs_tess)%>%
  st_cast("MULTIPOLYGON")%>%
  st_as_sf()%>%
  mutate(id = row_number())
Versuch_grids_transformiert_1000km
plot(Versuch_grids_transformiert_1000km$x)

st_crs(Versuch_grids_transformiert_1000km) <- st_crs(Filter_Harmonia_separated_coords_2001)

#grid mit allem in einzelnen Spalten
Filter_alle_Harmonia_C7 <-transform(Filter_beide_coords_2001, isHarmonia = species%in%"Harmonia axyridis", isC7 = species%in%"Coccinella septempunctata", isInsecta = class%in%"Insecta")
Filter_alle_Harmonia_C7
Europa_grid <- Versuch_grids_transformiert_1000km %>%
  st_join(st_sf(Filter_alle_Harmonia_C7)) %>%
  group_by(year,id) %>%
  count(isHarmonia = species%in%"Harmonia axyridis", isC7 = species%in%"Coccinella septempunctata", isInsecta = class%in%"Insecta")
  
  
transform(Europa_grid, isHarmonia = count(isHarmonia))



Europa_grid_2 <- Versuch_grids_transformiert_1000km %>%
  st_join(st_sf(Filter_Harmonia_separated_coords_2001)) %>%
  group_by(year,id) %>%
  summarize(Anzahl_Harmonia =n())
Europa_grid_2 

Europa_grid_3 <- Versuch_grids_transformiert_1000km %>%
  st_join(st_sf(Filter_C7_separated_coords_2001)) %>%
  group_by(year,id) %>%
  #count(species == "Coccinella septempunctata")
  #filter(species == "Coccinella septempunctata") %>%
  summarize(Anzahl_C7 =n())
  
Europa_grid_3

st_geometry(Europa_grid_3) <- NULL
blub <-left_join(Europa_grid_3, Europa_grid_2, group_by = "id")
blub <- blub%>%
  filter(! is.na(year))

blub
blub[is.na(blub)] <- 0

Filter_Harmonia_separated_coords_2001

glm_Raster_1000km <- glm.nb(formula = isC7 = TRUE ~  isInsecta = TRUE * isHarmonia =TRUE,
                            data = subset(Europa_grid,maxit = 1000))



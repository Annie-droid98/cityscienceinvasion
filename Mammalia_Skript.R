library(dplyr)
library(sf)
library(purrr)
library(tidyverse)
library(MASS)
library(spaMM)
#Einlesen Daten mit den Filtern: Mammalia Großbritanien, present, human observations, zwischen 2000-2021
Mammalia.GB <- vroom::vroom("0076293-210914110416597.csv",quote="",show_col_types = FALSE)


mammalia.GB_selected <- Mammalia.GB %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, taxonKey, class, order, datasetKey)

#Änderung des crs zu einem in m
Mammalia.GB_transformiert <- mammalia.GB_selected %>% 
  as.data.frame %>% 
  filter(year>1999)%>%
  filter((! is.na(decimalLatitude)))%>%
  sf::st_as_sf(coords = c(2,3))%>%
  st_set_crs(4326)%>%
  st_transform(5643)%>%
  mutate(long = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2)))

#Einlesen meiner Publisherliste
Mammalia_observations_GB <- vroom::vroom("EichhörnchenPublisheruntil1000obs.csv.2",quote="",show_col_types = FALSE)

Mammalia_GB_citizenscience_until1000obs <- Mammalia_observations_GB%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE")

Mammalia_GB_citizenscience<- Mammalia.GB_transformiert %>%
  filter(datasetKey %in% Mammalia_GB_citizenscience_until1000obs$datasetKey)

#mixed
Mammalia_GB_mixed_until1000obs <- Mammalia_observations_GB%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE" | Observer == "2"& FocusTaxaTorF == "FALSE")

Mammalia_GB_mixed<- Mammalia.GB_transformiert %>%
  filter(datasetKey %in% Mammalia_GB_mixed_until1000obs$datasetKey)

#alle (citizen science, mixed, scientific)
Mammalia_GB_alle_until1000obs <- Mammalia_observations_GB%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE" | Observer == "2"& FocusTaxaTorF == "FALSE" |Observer == "3"& FocusTaxaTorF == "FALSE")
Mammalia_GB_alle <- Mammalia.GB_transformiert %>%
  filter(datasetKey %in% Mammalia_GB_alle_until1000obs$datasetKey)

#Liste
Different_Publisher_categories <- list(Mammalia_GB_citizenscience,Mammalia_GB_mixed,Mammalia_GB_alle)

#einlesen der Grids (10km)

Grids <- readRDS("Differentgridsizes")
st_crs(Grids[[2]]) <- st_crs(Mammalia.GB_transformiert)

Mammalia.Gb_10km <- Grids[[2]] %>%
  st_join(st_sf(Different_Publisher_categories[[1]])) %>%
  transform(isVulgaris = species%in%"Sciurus vulgaris", isCarolinensis = species%in%"Sciurus carolinensis")%>%
  group_by(year,id) %>%
  count(isVulgaris, isCarolinensis, countMammalia = !isVulgaris&!isCarolinensis)%>%
  transform(what = ifelse(isVulgaris,"S.vulgaris", ifelse(isCarolinensis,"S.carolinensis", "countMammalia")))%>%
  spread(what,n)

#Mittelpunkt der grids
Mammalia.Gb_10km$Centergrid <-st_centroid(Mammalia.Gb_10km$x)
Mammalia.Gb_10km_2 <- Mammalia.Gb_10km%>%
  mutate(long = unlist(map(Centergrid,1)),
         lat = unlist(map(Centergrid,2)))


#Weiter zusammenfassen der Tabelle
Mammalia.GB_10km_2 <- Mammalia.Gb_10km_2 %>%
  unite('IDYear', id:year, remove = FALSE)


df_Mammalia_Gb_10km <- Mammalia.GB_10km_2%>% 
  arrange(IDYear) %>%
  group_by(IDYear) %>% fill(c(everything()), .direction = "downup") %>% 
  ungroup() %>% 
  distinct(IDYear, .keep_all = T) %>% 
  filter(!is.na(year))
  #filter(!is.na(countMammalia))

df_Mammalia_Gb_10km[is.na(df_Mammalia_Gb_10km)] <- 0
df_Mammalia_Gb_10km <- transform(df_Mammalia_Gb_10km, AllMammalia = countMammalia + S.carolinensis + S.vulgaris)

#speichern als RDS

saveRDS(df_Mammalia_Gb_10km, "Mammaliacounts_10km")
Mammaliacount_10km <- readRDS("Mammaliacounts_10km")
saveRDS(df_Mammalia_Gb_10km_mixed,"Mammalia.mixed")
Mammalia.mixed <- readRDS("Mammalia.mixed")

#Glm 
glm_10km_offset <- glm.nb(formula = S.vulgaris ~ offset(log(AllMammalia)) + scale(S.carolinensis) + I(2000-year) + lat +long, maxit = 1000,
                          data = Mammalia.mixed )

spatialglm.nb_Mammalia <- fitme(formula = S.vulgaris ~ offset(log(AllMammalia))
                                + scale(S.carolinensis) + I(2000-year)+ Matern(1|long + lat),
                                data = Mammaliacount_10km, family = negbin())




spatialglm.nb_Mammalia_10km <- fitme(formula = S.vulgaris ~ offset(log(AllMammalia))
                                + scale(S.carolinensis) + I(2000-year)+ Matern(1|long + lat), family = negbin(),
                                data = df_Mammalia_Gb__citizenscience_20km)

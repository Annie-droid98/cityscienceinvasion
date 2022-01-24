install.packages("spaMM")
library(spaMM)
library(dplyr)
library(sf)
library(tidyr)
library(purrr)
library(MASS)
#Einlesen der von gbif heruntergeladenen Tabelle und Transformation des CRS zu einem CRS in Meter

Insecta_weltweit <- vroom::vroom("0050297-210914110416597.csv",quote="",show_col_types = FALSE)
Insecta_weltweit_df <- Insecta_weltweit %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, taxonKey, class, order, datasetKey)

Insecta_weltweit_df<-Insecta_weltweit_df%>%
  as.data.frame %>% 
  filter(year>1999)%>%
  filter((! is.na(decimalLatitude)))%>%
  sf::st_as_sf(coords = c(2,3))%>%
  st_set_crs(4326)%>%
  st_transform(st_crs(Europe10grid))%>%
  mutate(long = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2)))

#Datei mit datasets die ok sind einlesen bisher bis mind. 1000 Beobachtungen

Insecta_Publisher_until1000observations <- vroom::vroom("PublisherlisteInsectauntil1000obs.csv",quote="",show_col_types = FALSE)

Insecta_Publisher_until1000ob_citizenscience <- Insecta_Publisher_until1000observations%>%
  filter(Observer == "1" & FocusTaxaTorF == "F")

Insecta_weltweit_citizenscience <- Insecta_weltweit_df %>%
  filter(datasetKey %in% Insecta_Publisher_until1000ob_citizenscience$datasetKey)

#einlesen der Grids


#Zählung der H.axyridis (invasiv), C.septempunctata (nativ) und der anderen Insecta in jedem grid (10*10km)
Insecta_citizenscience_10kmgrid <- Europe10grid %>%
  st_join(st_sf(Insecta_weltweit_citizenscience)) %>%
  transform(isC7 = species%in%"Coccinella septempunctata", isHarmonia = species%in%"Harmonia axyridis")%>%
  group_by(year,CellCode) %>%
  count(isC7, isHarmonia, countInsecta = !isC7&!isHarmonia)%>%
  transform(what = ifelse(isC7,"C.septempunctata", ifelse(isHarmonia,"H.axyridis", "countInsecta")))%>%
  spread(what,n)

#n() macht das selbe wie davor aberdann kann ich es bei gelegenheit ja nochmal ändern, weil das eindeutiger ist

#count_my_grids <- function(x){ 
 #  x %>%
 # st_join(st_sf(Insecta_weltweit_citizenscience)) %>%
 # transform(isC7 = species%in%"Coccinella septempunctata", isHarmonia = species%in%"Harmonia axyridis")%>%
 # group_by(year,id) %>%
 # count(isC7, isHarmonia, countInsecta = n)%>%
 # transform(what = ifelse(isC7,"C.septempunctata", ifelse(isHarmonia,"H.axyridis", "countInsecta")))%>%
 # spread(what,n)
#}

#lapply(Versuch_grids_transformiert, count_my_grids)


#Festlegung des Mittelpunktes von jedem grid für spätere spatial Analysen
#Insecta_citizenscience_10kmgrid$centroid <-st_centroid(Insecta_citizenscience_10kmgrid$x)

Insecta_citizenscience_10kmgrid <-Insecta_citizenscience_10kmgrid%>%
  mutate(long = unlist(map(centroid,1)),
         lat = unlist(map(centroid,2)))

#Ordnen des df und herausfiltern von allen grids wo keine Insekten drin sind (andere Insekten muss wahrscheinlich noch einmal den count ändern sodass 
# Alle Insekten und nicht alle außer den beiden Marienkäfern gleich gezählt werden)
Insecta_citizenscience_10kmgrid_1.1 <- Insecta_citizenscience_10kmgrid %>%
  unite('IDYear', CellCode:year, remove = FALSE)
Insecta_citizenscience_10kmgrid_1.1 <- Insecta_citizenscience_10kmgrid_1.1%>%
  arrange(IDYear) %>%
  group_by(IDYear) %>% fill(c(everything()), .direction = "downup") %>% 
  ungroup() %>% 
  distinct(IDYear, .keep_all = T) %>% 
  filter(!is.na(year))
  #filter(!is.na(countInsecta))

Insecta_citizenscience_10kmgrid_1.1[is.na(Insecta_citizenscience_10kmgrid_1.1)] <- 0
Insecta_citizenscience_10kmgrid_2 <- transform(Insecta_citizenscience_10kmgrid_1.1, AllInsecta = countInsecta + H.axyridis + C.septempunctata)

saveRDS(Insecta_citizenscience_10kmgrid_2, file = "Insectacounts_10km")
Insecta_Counts_10km <- readRDS("Insectacounts_10km")

Vegetation_europe_Ladybug_10km <- readRDS("Vegetation_europe_Ladybug_10km")
Insecta_with_vegetation <- merge(Insecta_Counts_10km, Vegetation_europe_Ladybug_10km, by="CellCode")




Insecta_counts_with_vegetation$Centergrid <-st_centroid(Insecta_counts_with_vegetation$geometry)
Insecta_counts_with_vegetation<- Insecta_counts_with_vegetation%>%
  mutate(long = unlist(map(Centergrid,1)),
         lat = unlist(map(Centergrid,2)))
saveRDS(Insecta_counts_with_vegetation, file = "Insecta_counts_with_vegetation")
Insecta_counts_with_vegetation <- readRDS("Insecta_counts_with_vegetation")

#Glm 
Insecta_citizenscience_glm <- glm.nb(formula = C.septempunctata ~ offset(log(AllInsecta)) + scale(H.axyridis) + I(2000-year) + long +lat, maxit = 1000,
                                           data = Insecta_counts_with_vegetation)

Insecta_counts_with_vegetation_2 <-Insecta_counts_with_vegetation%>%
  mutate_at(vars(clc_9, clc_11, clc_22, clc_25, clc_26, clc_39, clc_44), as.numeric)
 # transform(Vegetation = clc_9 +clc_11 + clc_22 + clc_25 + clc_26 + clc_39 + clc_44)
 # filter( Vegetation != 0)

#spaMM
library(spaMM)
Ladybugs_glm <- fitme(formula = C.septempunctata ~ offset(log(AllInsecta)) + scale(H.axyridis) + I(2000-year) + long +lat, family = negbin(), control.glm=list(maxit=1000),
                      data = Insecta_counts_with_vegetation)

#mit clc_22 converged es nicht, mit 25 schon
Ladybugs_glm_vegetation <- fitme(formula = C.septempunctata ~ offset(log(AllInsecta)) + scale(H.axyridis) + I(2000-year) + long + lat + scale(clc_9) + scale(clc_11) + scale(clc_22) + scale(clc_25) + scale(clc_26) +scale(clc_39) , family = negbin(), control.glm=list(maxit=10000),
                                 control.HLfit= list(max.iter.mean =500),
                                 data = Insecta_counts_with_vegetation_2)

Ladybugs_glm_vegetation_ohneagra <- fitme(formula = C.septempunctata ~ offset(log(AllInsecta)) + scale(H.axyridis) + I(2000-year) + long + lat + scale(clc_11) + scale(clc_22), family = negbin(), control.glm=list(maxit=1000),
                                 control.HLfit= list(max.iter.mean =500),
                                 data = Insecta_counts_with_vegetation_2)
Ladybugs_glm_vegetation_f_g_w_s <- fitme(formula = C.septempunctata ~ offset(log(AllInsecta)) + scale(H.axyridis) + I(2000-year) + long + lat + scale(clc_25) + scale(clc_26) +scale(clc_39) , family = negbin(), control.glm=list(maxit=1000),
                                      control.HLfit= list(max.iter.mean =500),
                                      data = Insecta_counts_with_vegetation_2)


Ladybugs_glmm_vegetation <- fitme(formula = C.septempunctata ~ offset(log(AllInsecta)) + scale(H.axyridis) + I(2000-year)
                + as.numeric(clc_9) + clc_11 + clc_22 + clc_25 + clc_26 + clc_39 + clc_44 + Matern(1|long + lat), family = negbin(), control.glm=list(maxit=1000),
                                 data = Insecta_counts_with_vegetation)
spatialglm.nb <- fitme(formula = C.septempunctata ~ offset(log(AllInsecta)) + scale(H.axyridis) + I(2000-year)+ Matern(1|long + lat), data = Insecta_Counts_10km, family = negbin() )


library(GGally)

as.tibble(Insecta_counts_with_vegetation)%>%
  dplyr::select(clc_9, clc_11, clc_22, clc_25, clc_26, clc_39, clc_44)%>%
  #mutate_at(vars(clc_9, clc_11, clc_22, clc_25, clc_26, clc_39, clc_44), as.numeric)
  #mutate_if(is.character,as.numeric)
 # dplyr::select_if(is.numeric)%>%
 # cor()
  ggpairs()


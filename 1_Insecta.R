#Einlesen der von gbif heruntergeladenen Tabelle und Transformation des CRS zu einem CRS in Meter

Insecta_weltweit <- vroom::vroom("0050297-210914110416597.csv",quote="",show_col_types = FALSE)
Insecta_weltweit_df <- Insecta_weltweit %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, taxonKey, class, order, datasetKey)%>% 
  as.data.frame %>% 
  filter(year>1999)%>%
  filter((! is.na(decimalLatitude)))%>%
  sf::st_as_sf(coords = c(2,3))%>%
  st_set_crs(4326)%>%
  st_transform(5643)%>%
  mutate(long = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2)))


#Datei mit datasets die ok sind einlesen bisher bis mind. 1000 Beobachtungen

Insecta_Publisher_until1000observations <- vroom::vroom("PublisherlisteInsectauntil1000obs.csv",quote="",show_col_types = FALSE)

Insecta_Publisher_until1000ob_citizenscience <- Insecta_Publisher_until1000observations%>%
  filter(Observer == "1" & FocusTaxaTorF == "F")

Insecta_weltweit_citizenscience <- Insecta_weltweit_df %>%
  filter(datasetKey %in% Insecta_Publisher_until1000ob_citizenscience$datasetKey)

#Zählung der H.axyridis (invasiv), C.septempunctata (nativ) und der anderen Insecta in jedem grid (10*10km)
Insecta_citizenscience_10kmgrid <- Versuch_grids_transformiert[[2]] %>%
  st_join(st_sf(Insecta_weltweit_citizenscience)) %>%
  transform(isC7 = species%in%"Coccinella septempunctata", isHarmonia = species%in%"Harmonia axyridis")%>%
  group_by(year,id) %>%
  count(isC7, isHarmonia, countInsecta = n)%>%
  transform(what = ifelse(isC7,"C.septempunctata", ifelse(isHarmonia,"H.axyridis", "countInsecta")))%>%
  spread(what,n)


count_my_grids <- function(x){ 
   x %>%
  st_join(st_sf(Insecta_weltweit_citizenscience)) %>%
  transform(isC7 = species%in%"Coccinella septempunctata", isHarmonia = species%in%"Harmonia axyridis")%>%
  group_by(year,id) %>%
  count(isC7, isHarmonia, countInsecta = n)%>%
  transform(what = ifelse(isC7,"C.septempunctata", ifelse(isHarmonia,"H.axyridis", "countInsecta")))%>%
  spread(what,n)
}
<- count_my_grids(Versuch_grids_transformiert[[2]])

lapply(Versuch_grids_transformiert, count_my_grids)


#Festlegung des Mittelpunktes von jedem grid für spätere spatial Analysen
Insecta_citizenscience_10kmgrid$centroid <-st_centroid(Insecta_citizenscience_10kmgrid$x)%>%
  mutate(long = unlist(map(centroid,1)),
         lat = unlist(map(centroid,2)))

#Ordnen des df und herausfiltern von allen grids wo keine Insekten drin sind (andere Insekten muss wahrscheinlich noch einmal den count ändern sodass 
# Alle Insekten und nicht alle außer den beiden Marienkäfern gleich gezählt werden)
Insecta_citizenscience_10kmgrid_1.1 <- Insecta_citizenscience_10kmgrid %>%
  unite('IDYear', id:year, remove = FALSE)%>% 
  arrange(IDYear) %>%
  group_by(IDYear) %>% fill(c(everything()), .direction = "downup") %>% 
  ungroup() %>% 
  distinct(IDYear, .keep_all = T) %>% 
  filter(!is.na(year)) %>%
  filter(!is.na(countInsecta))

Insecta_citizenscience_10kmgrid_1.1[is.na(Insecta_citizenscience_10kmgrid_1.1)] <- 0
Insecta_citizenscience_10kmgrid_2 <- transform(Insecta_citizenscience_10kmgrid_1.1, AllInsecta = countInsecta + H.axyridis + C.septempunctata)



#Glm 
Insecta_citizenscience_glm <- glm.nb(formula = C.septempunctata ~ offset(log(AllInsecta)) + scale(H.axyridis) + I(2000-year), maxit = 1000,
                                           data = Insecta_citizenscience_10kmgrid_2)


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

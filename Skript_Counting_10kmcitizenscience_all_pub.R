#Counting for all publisher
Mammalia.GB_2021 <- vroom::vroom("0169558-210914110416597.csv",quote="",show_col_types = FALSE)
mammalia.GB_selected_21 <- Mammalia.GB_2021 %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, taxonKey, class, order, datasetKey)

Grid_ohneduplices <- readRDS("10kmgrids.rds")

Mammalia.GB_10km <- mammalia.GB_selected_21 %>% 
  as.data.frame %>%
  filter((! is.na(decimalLatitude)))%>%
  sf::st_as_sf(coords = c(2,3))%>%
  st_set_crs(4326)%>%
  st_transform(st_crs(Grid_ohneduplices))

Mammalia.GB_10km <- Mammalia.GB_10km%>%
  dplyr::mutate(long = sf::st_coordinates(Mammalia.GB_10km_mixed)[,1],
                lat = sf::st_coordinates(Mammalia.GB_10km_mixed)[,2])

#Publisher categorisation
Mammalia_observations_GB <- vroom::vroom("EichhörnchenPublisheruntil1000obs.csv",
                                         quote="",show_col_types = FALSE)
#Filter observation data from citizen science only
Mammalia_citizenscience <-Mammalia_observations_GB%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE" )

Mammalia_citizenscience<- Mammalia.GB_10km %>%
  filter(datasetKey %in% Mammalia_GB_mixedpub_10km$datasetKey)

#Filter observation data from all publishers
Mammalia_GB_mixedpub_10km <- Mammalia_observations_GB%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE" | Observer == "2" & FocusTaxaTorF == "FALSE" | Observer == "3"& FocusTaxaTorF == "FALSE")
Mammalia_GB_mixed_pub10km<- Mammalia.GB_10km %>%
  filter(datasetKey %in% Mammalia_GB_mixedpub_10km$datasetKey)

Publisher_categories <- list(Mammalia_citizenscience, Mammalia_GB_mixed_pub10km)

#counting of S.carolinensis, S.vulgaris, M.martes and All mammal observations 
Mammalia_GB_count_10km <- lapply((Publisher_categories), function(i){

  Mammalia_GB_count_10km <- Grid_ohneduplices%>%
  st_join(st_sf(i)) %>%
  transform(isVulgaris = species%in%"Sciurus vulgaris", isCarolinensis = species%in%"Sciurus carolinensis", isMartes = species%in%"Martes martes")%>%
  group_by(year,CELLCODE) %>%
  count(isVulgaris, isCarolinensis, isMartes, countMammalia = !isVulgaris&!isCarolinensis&!isMartes)%>%
  transform(what = ifelse(isVulgaris,"S.vulgaris", ifelse(isCarolinensis,"S.carolinensis",
                                                          ifelse(isMartes,"M.martes", "countMammalia"))))%>%
  spread(what,n)
})

#middlepoint of each grid cell here citizen science count
Mammalia_GB_count_10km[[1]]$Centergrid <-st_centroid(Mammalia_GB_count_10km[[1]]$geometry)

Mammalia_GB_count_10km_2 <-Mammalia_GB_count_10km[[1]]%>%
  dplyr::mutate(lon = sf::st_coordinates(Mammalia_GB_count_10km[[1]]$Centergrid)[,1],
                lat = sf::st_coordinates(Mammalia_GB_count_10km[[1]]$Centergrid)[,2])

Mammalia_GB_count_10km_2_df <-Mammalia_GB_count_10km_2%>%
  unite('IDYear', CELLCODE:year, remove = FALSE)


Mammalia_GB_count_10km_2_df <- Mammalia_GB_count_10km_2_df%>% 
  arrange(IDYear) %>%
  group_by(IDYear) %>% fill(c(everything()), .direction = "downup") %>% 
  ungroup() %>% 
  distinct(IDYear, .keep_all = T) %>% 
  filter(!is.na(year))


Mammalia_GB_count_10km_2_df[is.na(Mammalia_GB_count_10km_2_df)] <- 0
Mammalia_GB_count_10km_2_df<- transform(Mammalia_GB_count_10km_2_df, 
                                               AllMammalia = countMammalia + S.vulgaris + M.martes + S.carolinensis)
Mammalia_GB_count_10km_2_df<- transform(Mammalia_GB_count_10km_2_df, 
                                               Proportion_carolinensis = S.carolinensis/AllMammalia)

Mammalia_GB_count_10km_2_df <- transform(Mammalia_GB_count_10km_2_df, Proportion_vulgaris = S.vulgaris/AllMammalia)

Mammalia_GB_count_10km_2_df <- transform(Mammalia_GB_count_10km_2_df, Proportion_marten = M.martes/AllMammalia)

Vegetation_europe_squirrels_10km <- readRDS()

Mammalia_GB_count_10km_tab <- merge(Mammalia_GB_count_10km_2_df , Vegetation_europe_squirrels_10km,
                                                    by="CELLCODE")
Mammalia_GB_count_10km_tab <-Mammalia_GB_count_10km_tab%>%
  mutate_at(vars(clc_9_s, clc_11_s, clc_22_s, clc_23_s, clc_24_s, clc_25_s, clc_39_s, clc_44_s),as.numeric)%>%
  transform(Vegetation = clc_9_s +clc_11_s + clc_22_s +clc_23_s + clc_24_s + clc_25_s + clc_39_s + clc_44_s)%>%
  filter( Vegetation != 0)

Mammalia_GB_count_10km_tab<- Mammalia_GB_count_10km_tab%>%
  rename(Grey_urban = clc_9_s, green_urban = clc_11_s, Agrar = clc_22_s, 
         Broadleafed_Forest = clc_23_s, Coniferous_Forest = clc_24_s, Mixed_Forest = clc_25_s,
         Other_seminatural =clc_39_s, Waterbodies =clc_44_s)
Mammalia_GB_count_10km_tab <- as.data.frame(Mammalia_GB_count_10km_tab)
Mammalia_GB_count_10km_tab <- Mammalia_GB_count_10km_tab[, c("CELLCODE", "year", "M.martes", "S.carolinensis", "S.vulgaris",
                                                                                             "AllMammalia", "Proportion_marten", "Proportion_carolinensis",
                                                                                             "Proportion_vulgaris", "lat", "lon","Grey_urban",
                                                                                             "green_urban", "Agrar", "Broadleafed_Forest",
                                                                                             "Coniferous_Forest", "Mixed_Forest",
                                                                                             "Other_seminatural", "Waterbodies")]



Mammalia_GB_count_10km_tab %>%
  mutate(across(c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris,
                  Proportion_marten), .names = "{.col}_z")) %>% ## turn a bunch of variable into their scaled form and add _z to their names
  mutate(AllMammalia_log = log(AllMammalia),
         year_from_2000 = year - 2000,
         lon = lon/1e5, lat = lat/1e5) %>% ## that seems to be important, since I guess your weird coordinate yield huge values in matrix computations
  dplyr::select(-c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris,
                   Proportion_marten, AllMammalia, year, CELLCODE)) -> d #d_10kmmixed for all publisher
#mesh for all publisher
#mesh_10km_mixed <- INLA::inla.mesh.2d(loc = d_10kmmixed[, c("lon", "lat")], max.n = 100, max.edge = c(3, 20))
mesh<- INLA::inla.mesh.2d(loc = d[, c("lon", "lat")], max.n = 100, max.edge = c(3, 20))
Mammalia.GB_moreVertebrata <- vroom::vroom("0179718-210914110416597.csv",quote="",show_col_types = FALSE)
Grid_ohneduplices <- readRDS("10kmgrids.rds")

Mammalia.GB_moreVertebrata_selected <- Mammalia.GB_moreVertebrata %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, taxonKey, class, order, datasetKey)
Mammalia.GB_moreVertebrata_2 <- Mammalia.GB_moreVertebrata_selected %>% 
  as.data.frame %>%
  filter((! is.na(decimalLatitude)))%>%
  sf::st_as_sf(coords = c(2,3))%>%
  st_set_crs(4326)%>%
  st_transform(st_crs(Grid_ohneduplices))

Mammalia.GB_moreVertebrata_2 <- Mammalia.GB_moreVertebrata_2  %>%
  dplyr::mutate(long = sf::st_coordinates(Mammalia.GB_moreVertebrata_2)[,1],
                lat = sf::st_coordinates(Mammalia.GB_moreVertebrata_2)[,2])


Mammalia.GB_moreVertebrata_publisher <- vroom::vroom("EichhörnchenPublisher(Mammalia,Amphibia,Reptilia,Aves).csv",
                                                     quote="",show_col_types = FALSE)

Mammalia.GB_moreVertebrata_publisher_citizenscience <- Mammalia.GB_moreVertebrata_publisher%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE")

Mammalia.GB_moreVertebrata_publisher_citizenscience_df <- Mammalia.GB_moreVertebrata_2 %>%
  filter(datasetKey %in% Mammalia.GB_moreVertebrata_publisher_citizenscience$Datasetkey)

Mammalia_GB_mixed_moreverte <- Mammalia.GB_moreVertebrata_publisher%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE" | Observer == "2"& FocusTaxaTorF == "FALSE" | Observer == "3"& FocusTaxaTorF == "FALSE")


Mammalia_GB_mixed_moreverte<- Mammalia.GB_moreVertebrata_2 %>%
  filter(datasetKey %in% Mammalia_GB_mixed_moreverte$Datasetkey)
Publisher_categories <- list(Mammalia.GB_moreVertebrata_publisher_citizenscience_df,Mammalia_GB_mixed_moreverte)

Mammalia_GB_countmoreVertebrata <- lapply((Publisher_categories), function(i){
  
  Mammalia_GB_countmoreVertebrata <- Grid_ohneduplices%>%
    #st_join(st_sf(Mammalia_GB_mixed_moreverte)) %>%
    st_join(st_sf(i)) %>%
    transform(isVulgaris = species%in%"Sciurus vulgaris", isCarolinensis = species%in%"Sciurus carolinensis", isMartes = species%in%"Martes martes")%>%
    group_by(year,CELLCODE) %>%
    count(isVulgaris, isCarolinensis, isMartes, countMammalia = !isVulgaris&!isCarolinensis&!isMartes)%>%
    transform(what = ifelse(isVulgaris,"S.vulgaris", ifelse(isCarolinensis,"S.carolinensis",
                                                            ifelse(isMartes,"M.martes", "countMammalia"))))%>%
    spread(what,n)
})

#table only for citizen science observations
Mammalia_GB_countmoreVertebrata[[1]]$Centergrid <-st_centroid(Mammalia_GB_countmoreVertebrata[[1]]$geometry)
Mammalia_GB_countmoreVertebrata <- Mammalia_GB_countmoreVertebrata[[1]]%>%
  dplyr::mutate(lon = sf::st_coordinates(Mammalia_GB_countmoreVertebrata[[1]]$Centergrid)[,1],
                lat = sf::st_coordinates(Mammalia_GB_countmoreVertebrata$Centergrid)[,2])

#Summarising the counting table
Mammalia_GB_countmoreVertebrata <- Mammalia_GB_countmoreVertebrata%>%
  unite('IDYear', CELLCODE:year, remove = FALSE)

df_Mammalia_GB_countmoreVertebrata <- Mammalia_GB_countmoreVertebrata %>% 
  arrange(IDYear) %>%
  group_by(IDYear) %>% fill(c(everything()), .direction = "downup") %>% 
  ungroup() %>% 
  distinct(IDYear, .keep_all = T) %>% 
  filter(!is.na(year))



df_Mammalia_GB_countmoreVertebrata[is.na(df_Mammalia_GB_countmoreVertebrata)] <- 0
df_Mammalia_GB_countmoreVertebrata <- transform(df_Mammalia_GB_countmoreVertebrata, 
                                                AllMammalia = countMammalia + S.vulgaris + M.martes + S.carolinensis)
df_Mammalia_GB_countmoreVertebrata <- transform(df_Mammalia_GB_countmoreVertebrata, 
                                                Proportion_carolinensis = S.carolinensis/AllMammalia)
df_Mammalia_GB_countmoreVertebrata <- transform(df_Mammalia_GB_countmoreVertebrata, Proportion_vulgaris = S.vulgaris/AllMammalia)
df_Mammalia_GB_countmoreVertebrata <- transform(df_Mammalia_GB_countmoreVertebrata, Proportion_marten = M.martes/AllMammalia)

Vegetation_europe_squirrels_10km<- readRDS("Vegetation_europe_squirrels_10km")

#combine the grid with the vegetationraster
df_Mammalia_GB_countmoreVertebrata_2 <- merge(df_Mammalia_GB_countmoreVertebrata, Vegetation_europe_squirrels_10km,
                                              by="CELLCODE")
df_Mammalia_moreVertebrata <-df_Mammalia_GB_countmoreVertebrata_2%>%
  mutate_at(vars(clc_9_s, clc_11_s, clc_22_s, clc_23_s, clc_24_s, clc_25_s, clc_39_s, clc_44_s),as.numeric)%>%
  transform(Vegetation = clc_9_s +clc_11_s + clc_22_s +clc_23_s + clc_24_s + clc_25_s + clc_39_s + clc_44_s)%>%
  filter( Vegetation != 0)
df_Mammalia_moreVertebrata <- df_Mammalia_moreVertebrata%>%
  rename(Grey_urban = clc_9_s, green_urban = clc_11_s, Agrar = clc_22_s, 
         Broadleafed_Forest = clc_23_s, Coniferous_Forest = clc_24_s, Mixed_Forest = clc_25_s,
         Other_seminatural =clc_39_s, Waterbodies =clc_44_s)
df_Mammalia_moreVertebrata <- as.data.frame(df_Mammalia_moreVertebrata)
df_Mammalia_moreVertebrata  <- df_Mammalia_moreVertebrata[, c("CELLCODE", "year", "M.martes", "S.carolinensis", "S.vulgaris",
                                                              "AllMammalia", "Proportion_marten", "Proportion_carolinensis",
                                                              "Proportion_vulgaris", "lat", "lon","Grey_urban",
                                                              "green_urban", "Agrar", "Broadleafed_Forest",
                                                              "Coniferous_Forest", "Mixed_Forest",
                                                              "Other_seminatural", "Waterbodies")]

df_Mammalia_moreVertebrata %>%
  mutate(across(c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris,
                  Proportion_marten), .names = "{.col}_z")) %>% ## turn a bunch of variable into their scaled form and add _z to their names
  mutate(AllMammalia_log = log(AllMammalia),
         year_from_2000 = year - 2000,
         lon = lon/1e5, lat = lat/1e5) %>% ## that seems to be important, since I guess your weird coordinate yield huge values in matrix computations
  dplyr::select(-c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris,
                   Proportion_marten, AllMammalia, year, CELLCODE)) -> d_moreverte
saveRDS(d_moreverte,"Datavertebratanormalalle.rds")
d_moreverte <-readRDS("Datavertebratanormalalle.rds")
#saveRDS(d_moreverte,"Datavertebratanormalicitizen.rds")
d_moreverte <- readRDS("Datavertebratanormalicitizen.rds")

mesh_moreverte <- INLA::inla.mesh.2d(loc = d_moreverte[, c("lon", "lat")], max.n = 100, max.edge = c(3, 20))
#mesh_moreverte_mixed <- INLA::inla.mesh.2d(loc = d_moreverte[, c("lon", "lat")], max.n = 100, max.edge = c(3, 20))
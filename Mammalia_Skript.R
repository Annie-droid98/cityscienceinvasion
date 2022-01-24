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
  st_transform(st_crs(GB_and_IE_grid_10km))%>%
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



Mammalia.Gb_10km <- GB_and_IE_grid_10km %>%
  st_join(st_sf(Different_Publisher_categories[[1]])) %>%
  transform(isVulgaris = species%in%"Sciurus vulgaris", isCarolinensis = species%in%"Sciurus carolinensis")%>%
  group_by(year,CELLCODE) %>%
  count(isVulgaris, isCarolinensis, countMammalia = !isVulgaris&!isCarolinensis)%>%
  transform(what = ifelse(isVulgaris,"S.vulgaris", ifelse(isCarolinensis,"S.carolinensis", "countMammalia")))%>%
  spread(what,n)

#Mittelpunkt der grids
Mammalia.Gb_10km$Centergrid <-st_centroid(Mammalia.Gb_10km$geometry)
Mammalia.Gb_10km_2 <- Mammalia.Gb_10km%>%
  mutate(long = unlist(map(Centergrid,1)),
         lat = unlist(map(Centergrid,2)))


#Weiter zusammenfassen der Tabelle
Mammalia.GB_10km_2 <- Mammalia.Gb_10km_2 %>%
  unite('IDYear', CELLCODE:year, remove = FALSE)


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
Vegetation_europe_squirrels_10km <- readRDS("Vegetation_europe_squirrels_10km")
Mammaliacount_10km_with_vegetation <- merge(Mammaliacount_10km, Vegetation_europe_squirrels_10km, by="CELLCODE")

saveRDS(Mammaliacount_10km_with_vegetation,"Mammaliacount_10km_with_vegetation")

#saveRDS(df_Mammalia_Gb_10km_mixed,"Mammalia.mixed")
#Mammalia.mixed <- readRDS("Mammalia.mixed")

Mammaliacount_10km_with_vegetation_2 <-Mammaliacount_10km_with_vegetation%>%
  mutate_at(vars(clc_9_s, clc_11_s, clc_22_s, clc_23_s, clc_24_s, clc_25_s, clc_39_s, clc_44_s), as.numeric)%>%
  transform(Vegetation = clc_9_s +clc_11_s + clc_22_s +clc_23_s + clc_24_s + clc_25_s + clc_39_s + clc_44_s)%>%
  filter( Vegetation != 0)

Mammaliacount_10km_with_vegetation_2 <- Mammaliacount_10km_with_vegetation_2%>%
  rename(Grey_urban = clc_9_s, green_urban = clc_11_s, Agrar = clc_22_s, 
         Broadleafed_Forest = clc_23_s, Coniferous_Forest = clc_24_s, Mixed_Forest = clc_25_s,
         Other_seminatural =clc_39_s, Waterbodies =clc_44_s)

Mammaliacount_10km_with_vegetation_2 <- Mammaliacount_10km_with_vegetation_2%>%
  transform(Proportion_carolinensis = S.carolinensis/AllMammalia)
#Glm 
library(spaMM)
glm_10km_offset <- fitme(formula = S.vulgaris ~ offset(log(AllMammalia)) + scale(S.carolinensis) + I(2000-year) + long +lat, family = negbin(),
                          data = Mammaliacount_10km_with_vegetation)

#ohne lat + long converged es mit noch nicht
glm_10km_offset_vegetation <- fitme(formula = S.vulgaris ~ offset(log(AllMammalia)) 
                                    + scale(Proportion_carolinensis) + I(2000-year) + scale(Grey_urban)
                                    + scale(green_urban) + scale(Agrar) 
                                    + scale(Broadleafed_Forest) + scale(Coniferous_Forest) 
                                    + scale(Mixed_Forest)+ scale(Other_seminatural) 
                                    + scale(Proportion_carolinensis):scale(Broadleafed_Forest) , family = negbin(),
                         data = Mammaliacount_10km_with_vegetation_2)

glm_10km_offset_vegetation_2 <- fitme(formula = S.vulgaris ~ offset(log(AllMammalia)) 
                                    + I(2000-year) + scale(Grey_urban)
                                    + scale(green_urban) + scale(Agrar) 
                                    + scale(Mixed_Forest)+ scale(Other_seminatural) 
                                    + scale(Proportion_carolinensis)*scale(Broadleafed_Forest)
                                    + scale(Proportion_carolinensis)*scale(Coniferous_Forest) , family = negbin(),
                                    data = Mammaliacount_10km_with_vegetation_2)
plot(glm_10km_offset_vegetation_2)

Squirrels_glm_nb <- glm.nb(formula = S.vulgaris ~ offset(log(AllMammalia)) 
                            + I(2000-year) + scale(Grey_urban)
                           + scale(green_urban) + scale(Agrar) 
                           + scale(Coniferous_Forest) 
                           + scale(Mixed_Forest)+ scale(Other_seminatural) 
                           + scale(Proportion_carolinensis)*scale(Broadleafed_Forest)
                           + scale(Proportion_carolinensis)*scale(Coniferous_Forest),
                           data = Mammaliacount_10km_with_vegetation_2)

prediction_squirrels <- predict(glm_10km_offset_vegetation)
plot(prediction_squirrels)

png(filename= "Predictions_squirrel.png")


plot_effects(glm_10km_offset_vegetation_2 , focal_var ="Proportion_carolinensis" )


dev.off()

png(filename= "Predictions_squirrel_broadleafed.png")
plot_effects(glm_10km_offset_vegetation_2,focal_var = "Proportion_carolinensis":"Broadleafed_Forest")
dev.off()

#pdep_effects(glm_10km_offset_vegetation_2,focal_var = "Proportion_carolinensis":"Broadleafed_Forest")

residuals_squirrel <- simulateResiduals(glm_10km_offset_vegetation)
plot(residuals_squirrel)
testOutliers(residuals_squirrel)

#Funktioniert nicht
ggeffects::ggpredict(glm_10km_offset_vegetation)

glm_10km_offset_vegetation_2 <- fitme(formula = S.vulgaris ~ offset(log(AllMammalia)) + scale(S.carolinensis) + I(2000-year)  +  scale(clc_9_s) + + scale(clc_11_s)+ scale(clc_22_s)+ scale(clc_23_s) + scale(clc_24_s) + scale(clc_25_s)+ scale(clc_39_s) , family = negbin(),
                                    data = Mammaliacount_10km_with_vegetation_2)


#Plot glm
glm_10km_offset_vegetation_plot <- plot(glm_10km_offset_vegetation)
glm_10km_offset_vegetation_predictions_plot <- plot(prediction_squirrels)



install.packages("ggcorrplot")
library(ggcorrplot)
Correlation_Vegetation_Mammalia <- as_tibble(Mammaliacount_10km_with_vegetation_2)%>%
 dplyr::select(Grey_urban, green_urban, Agrar, Broadleafed_Forest, Coniferous_Forest, Mixed_Forest, Other_seminatural, Waterbodies)%>%
 cor()
 # ggcorrplot()
library(ggplot2)
 Mammalia_vegetation_density <- as_tibble(Mammaliacount_10km_with_vegetation_2)%>%
  ggplot(aes(x= clc_9_s))+
  geom_density()
 
 png(filename= "Density_plot_urban.png")
 Mammalia_vegetation_density
 dev.off()

  
  #glm.nb(formula = S.vulgaris ~ offset(log(AllMammalia)) + scale(S.carolinensis) + I(2000-year) + long + lat, maxit = 1000,
   #                        data = Mammaliacount_10km)

spatialglmm.nb_Mammalia <- fitme(formula = S.vulgaris ~ offset(log(AllMammalia)) 
                                      + scale(S.carolinensis) + I(2000-year) + scale(Grey_urban)
                                      + scale(green_urban) + scale(Agrar) 
                                      + scale(Broadleafed_Forest) + scale(Coniferous_Forest) 
                                      + scale(Mixed_Forest)+ scale(Other_seminatural) + Matern(1|long + lat),
                                control.HLfit= list(max.iter.mean =5000), family = negbin(),
                                data = Mammaliacount_10km_with_vegetation_2)




spatialglm.nb_Mammalia_10km <- fitme(formula = S.vulgaris ~ offset(log(AllMammalia))
                                + scale(S.carolinensis) + I(2000-year)+ Matern(1|long + lat), family = negbin(),
                                data = df_Mammalia_Gb__citizenscience_20km)


Sciurus_carolinensis_citizenscience <- Mammalia_GB_citizenscience%>%
  filter(species=="Sciurus carolinensis")


library(scales)
library(viridis)
install.packages("gtsummary")
install.packages("gapminder")
install.packages("gt")
library(psych)
library(gt)
library(gapminder)
library(tidyverse)
library(gtsummary)

#Figure 1

species_liste <- list("Sciurus carolinensis", "Sciurus vulgaris", "Martes martes")
Mammalia_citizenscience_alle <-Mammalia_observations_GB%>%
  filter(Observer == "1")

Mammalia_citizenscience_alle<- Mammalia.GB_10km %>%
  filter(datasetKey %in% Mammalia_citizenscience_alle$datasetKey)

species <-  Mammalia_citizenscience_alle %>% #Mammalia_citizenscience
  count(year, sort = TRUE)

species$species <- "Mammalia"
species<- species%>%
  as.data.frame()%>%
  dplyr::select(year, species, n)

species_diagramm <- lapply((species_liste), function(i){
  species_diagramm_alle <-Mammalia_GB_citizenscience_21 %>%
    filter(species == i)%>%
    count(year,species,  sort = TRUE)%>%
    as.data.frame()%>%
    dplyr::select(year, species, n)
  
})
species_df <- rbind(species_diagramm[[1]],species_diagramm[[2]],species_diagramm[[3]],species)

Mammalia_GB_mixedpub_10km_alle <- Mammalia_observations_GB%>%
  filter(Observer == "1" | Observer == "2"  | Observer == "3")
Mammalia_GB_mixedpub_10km_alle<- Mammalia.GB_10km %>%
  filter(datasetKey %in% Mammalia_GB_mixedpub_10km_alle$datasetKey)


species_allepub <- Mammalia_GB_mixedpub_10km_alle%>%
  count(year, sort = TRUE)

species_allepub$species <- "Mammalia"
species_allepub<- species_allepub%>%
  as.data.frame()%>%
  dplyr::select(year, species, n)

species_diagramm_alle<- lapply((species_liste), function(i){
  species_diagramm_alle <-Mammalia_GB_mixedpub_10km_alle %>%
    filter(species == i)%>%
    count(year,species,  sort = TRUE)%>%
    as.data.frame()%>%
    dplyr::select(year, species, n)
  
})
species_df_2 <- rbind(species_diagramm_alle[[1]],species_diagramm_alle[[2]],species_diagramm_alle[[3]], species_allepub)%>%
  mutate(Pub ="B_All")

species_df <- species_df%>%
  mutate(Pub = "A_Citizenscience")
beidepubline <- rbind(species_df_2,species_df)


Balkendiagramm <- data.frame(Publisher = rev(c("All","All","Citizen science", "Citizen science", "Mixed","Mixed", "Scientific","Scientific")),            # Create example data
                             Observations = rev(c(703624,778206,151311,427814,283802,336016,170611,6596)),
                             Has_focus_taxon = rev(c("Yes","No","Yes","No","Yes","No", "Yes","No")))

pdf("Figure1.pdf", width= 18, height=8)
my_barplot <- ggplot(Balkendiagramm, aes(fill=Has_focus_taxon, y=Observations, x=Publisher)) + 
  geom_bar(position="stack", stat="identity",width = 0.5)+
  scale_fill_viridis(discrete = T)+
  coord_flip()+
  scale_x_discrete(
    limits=rev(c("All","Citizen science", "Mixed", "Scientific")),
    labels=rev(c("All","Citizen science", "Mixed", "Scientific"))
  ) +
  theme_minimal() +
  xlab("")+
  ylab("Number of Observations")+
  guides(fill=guide_legend(title="Has focus taxon"))+ 
  theme(legend.text = element_text(size=18),legend.title = element_text(size=20), axis.text=element_text(size=18), axis.title = element_text(size = 20),legend.justification = c("right", "top"),)

my_lineplot <- 
  ggplot(beidepubline, aes(x = year, y = n, color = species, linetype = Pub)) +
  geom_line()+
  scale_y_log10(labels = comma_format(big.mark = ".",
                                      decimal.mark = ","))+
  ylab("Number of observations")+
  theme_minimal()+
  # ggtitle("Number of citizenscience observations over the years")+
  scale_linetype(name="Publisher", labels = c("Citizen science","All"))+
  # ggtitle("Number of citizenscience observations over the years")+
  theme(legend.text = element_text(size=18, 
                                   face="italic"),legend.title = element_text(size=20), axis.text=element_text(size=18), axis.title = element_text(size = 20))

ggarrange(my_barplot,my_lineplot,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
dev.off()


#Grid -> Figure 2
writeRaster(clc_2018_landcover_squirrels, "clc_2018_landcover_categories_Squirrels")
clc_2018_landcover_squirrels <- raster("clc_2018_landcover_categories_Squirrels")
df_Mammalia_GB_count<- readRDS("df_Mammalia_GB_count")

Ausschnitt_GB <-extent(3542199, 3660000, 3140000, 3220000)
crop_vegetationsquirrel_Ausschnitt <- crop(x = clc_2018_landcover_squirrels, Ausschnitt_GB)


crop_Grid_Ausschnitt <- st_crop(x = df_Mammalia_GB_count, Ausschnitt_GB)
crop_Grid_Ausschnitt_2018 <- crop_Grid_Ausschnitt%>%
  filter(year == 2018)

crop_Grid_Ausschnitt_2018_try <- crop_Grid_Ausschnitt_2018%>%
  filter(lat < 3220000 & lat> 3140000 & lon < 3660000)
cuts_s=c(1,9,11,22,23,24,25,39,44)
crop_GB_and_IE_grid_10km_shp <- crop(x = GB_and_IE_grid_10km_shp, Ausschnitt_GB)

GB_Maße <- raster::extent(2800000,3930000,2880000,4300000)
r <- raster()
extent(r) <- GB_Maße

Squirrels_map_GB_crop <- crop(x = clc_2018_landcover_squirrels, y = GB_Maße)

Ausschnit_Ed <- extent(3400000,3520000,3680000,3760000)
crop_vegetationsquirrel_Ausschnitt_Ed <- crop(x = clc_2018_landcover_squirrels, Ausschnit_Ed)

crop_Grid_Ausschnitt_Ed <- st_crop(x = df_Mammalia_GB_count, Ausschnit_Ed)
crop_Grid_Ausschnitt_Ed_2018 <- crop_Grid_Ausschnitt_Ed%>%
  filter(year == 2018)

crop_Grid_Ausschnitt_2018_try_Ed <- crop_Grid_Ausschnitt_Ed_2018 %>%
  filter(lat < 3765000 & lat> 3675000 & lon < 3525000 & lon > 3395000)

crop_GB_and_IE_grid_10km_shp_Ed <- crop(x = GB_and_IE_grid_10km_shp, Ausschnit_Ed)


#average grid based on citizen science observations
df_counted_Mammalia <- readRDS("CountedMammaliacitizenscience.rds")

df_counted_Mammalia_try <- df_counted_Mammalia%>%
  dplyr::select(CELLCODE, Grey_urban,green_urban, Agrar,Broadleafed_Forest,Coniferous_Forest, Mixed_Forest,Other_seminatural, Waterbodies)

df_counted_Mammalia_try_2 <- unique(df_counted_Mammalia_try)
df_counted_Mammalia_try_2 <- colMeans(df_counted_Mammalia_try_2[2:9])%>%
  as.data.frame()

df_counted_Mammalia_try_2<-tibble::rownames_to_column(df_counted_Mammalia_try_2, "Vegetationtype")
colnames(df_counted_Mammalia_try_2)<- c("Landcover","Proportion")
df_counted_Mammalia_try_2 <- df_counted_Mammalia_try_2%>% 
  mutate(Year = "of total area")
df_counted_Mammalia_try_2 <- df_counted_Mammalia_try_2%>% 
  mutate_at(vars(Proportion),
            .funs = funs(. * 100))
df_counted_Mammalia_try_3 <- df_counted_Mammalia_try_2%>%
  mutate_at(vars(Proportion),
            funs(round(., 1)))

Mammalia_in_GB <- Mammalia_citizenscience%>%
  dplyr:: select(species, year, long, lat)%>%
  as.data.frame()
#Observations in each landcover type
Mammalia_in_Gb_2 <- SpatialPointsDataFrame(coords = Mammalia_in_GB[,3:4], data =Mammalia_in_GB)
extract_Mammalia <- raster:: extract(clc_2018_landcover_squirrels ,Mammalia_in_Gb_2 )
Values_Mammalia <- cbind(Mammalia_in_Gb_2,extract_Mammalia)
Values_Mammalia_df <- as.data.frame(Values_Mammalia)
Vegetation_of_each_obs <- Values_Mammalia_df%>%
  count(Vegetation,  sort = F)            
Percentage_obs_inVeg <-transform(Vegetation_of_each_obs, Percentage_Mamm = Vegetation_of_each_obs[2]/colSums(Vegetation_of_each_obs[2]))
Percentage_obs_inVeg<- Percentage_obs_inVeg[-c(9),]%>%
  mutate(Year = "of observations")%>%
  mutate_at(vars(n.1),
            .funs = funs(. * 100))%>%
  mutate_at(vars(n.1),
            funs(round(., 1)))
Percentage_obs_inVeg$Landcover <- c("Grey_urban", "green_urban", "Agrar", "Broadleafed_Forest", "Coniferous_Forest", "Mixed_Forest","Other_seminatural","Waterbodies")

Percentage_obs_inVeg_try<-Percentage_obs_inVeg%>%
  dplyr::select(Landcover,n.1, Year)
Percentage_obs_inVeg_try<- Percentage_obs_inVeg_try%>%
  rename(Proportion = n.1)
perc_try <-rbind(df_counted_Mammalia_try_3,Percentage_obs_inVeg_try)
perc_try_3 <-perc_try
perc_try_3$Year <-factor(perc_try_3$Year, levels = c("of total area", "of observations"))
perc_try_4 <- transform(perc_try_3, Year_num = ifelse(Year == "of observations",
                                                      as.numeric(factor(Year)) - .25,
                                                      as.numeric(factor(Year)) + .25) )

pdf("Figure2.pdf", width= 14, height=11)
par(mfrow=c(2, 2))

plot(crop_vegetationsquirrel_Ausschnitt,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"),xaxt = "n", yaxt = "n")
#axis(1, at = c(3540000, 3570000, 3600000, 3630000, 3660000))
#axis(2, at = c(3125000, 3150000, 31750000, 3200000, 3225000))
text(x = crop_Grid_Ausschnitt_2018_try$lon,
     y = crop_Grid_Ausschnitt_2018_try$lat,
     labels = crop_Grid_Ausschnitt_2018_try$AllMammalia, col = "black", font = 2, cex=1.5)
plot(crop_GB_and_IE_grid_10km_shp , add=T)

plot(Squirrels_map_GB_crop,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"),xaxt = "n", yaxt = "n")
#xlim=c(2900000,4500000), ylim=c(3000000,4300000)
plot(crop_vegetationsquirrel_Ausschnitt_Ed,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"), xaxt = "n", yaxt = "n") # ann
#xlim=c(3380000,3520000),ylim=c(3680000,3760000)
text(x = crop_Grid_Ausschnitt_2018_try_Ed$lon,
     y = crop_Grid_Ausschnitt_2018_try_Ed$lat,
     labels = crop_Grid_Ausschnitt_2018_try_Ed$AllMammalia, col = "black", font = 2, cex=1.5)
plot(crop_GB_and_IE_grid_10km_shp_Ed, add=T)


#legend(x = "topright", inset = c(-0.4, 0.30),                  
#      legend =c("Grey_urban", "green_urban", "Agrar", "Broadleafed_Forrest","Coniferous_Forest",
#               "Mixed_Forest", "Other semi-natural", "Waterbodies"),fill = c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"), xpd=T)

plot.new()              ## suggested by @Josh
vps <- baseViewports()
pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
vp1 <-plotViewport(c(2.8,2,1,2))

my_barggplot <- ggplot(perc_try_4, aes(x = Year, y = Proportion, fill = Landcover)) + 
  geom_bar(width = 0.5,stat = "identity") +
  # scale_x_discrete(limits = c("of total area", "of observations")) +
  geom_line( aes(x = Year_num), 
             position = position_stack())+
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#fec44f","#005a32","#8c2d04","#addd8e","#737373","#88419d","#dd3497","#0c2c84"), labels = c("Agricultural", "Broadleafed forest","Coniferous forest","Green urban", "Grey urban","Mixed forest","Other seminatural", "Waterbodies"))+
  # scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL) +
  theme(plot.title = element_text(size = rel(1.4),face ='bold'))
print(my_barggplot,vp = vp1)
dev.off()



#Correlationplot 
d <- readRDS("Citizensciencetab.rds")
Correlationtest_df <- d%>%
  select(c(6:13))%>%
  as.data.frame()
Correlationtest_df2 <- Correlationtest_df%>%
  rename("Grey urban"=Grey_urban_z, "Green urban" = green_urban_z, "Agricultural" = Agrar_z , 
         "Broadleafed Forest" =Broadleafed_Forest_z , "Coniferous Forest"=Coniferous_Forest_z , "Mixed Forest"= Mixed_Forest_z,
         "Other seminatural"=Other_seminatural_z, "Waterbodies"=Waterbodies_z)
corrtest <- cor(Correlationtest_df2)



png(filename = "Correlationplot.png")
corrplot(corrtest, order = 'AOE', addCoef.col = 'black',tl.col = 'black', col = COL2('RdBu'))
dev.off()


#Description different models

Modelle_1 <- c("Citizen science","All data" )
Modelle_2 <- c( "Main model presented", "Comparison with main model")
Modelle_3 <- c( "Comparison with main model",
                "Comparison with main model")
Tabelle_mod <- cbind(Modelle_1,Modelle_2,Modelle_3)%>%
  as.data.frame()%>%
  rename("Data / normalisation (offset)"= Modelle_1, "Mammalia" =Modelle_2, "Vertebrata"= Modelle_3)


Tabelle_mod<-Tabelle_mod %>%
  gt(rowname_col = "Data / normalisation (offset)")%>%
  tab_stubhead(label = "Data / normalisation (offset)")%>%
  tab_footnote(
    footnote = "Mammalia,Aves,Amphibia and Reptilia observations",
    locations = cells_column_labels(
      columns = c("Vertebrata")
    ))

#predictionmaps

myLocation_B<-c(-12.52, 49.71, 2.06, 59.53)
myMap_B_toner_2 <- get_stamenmap(bbox=myLocation_B, maptype="toner-lite", crop=TRUE)
saveRDS(myMap_B_toner_2,"MapGB+IE.rds")


#Vorbereitung dataframe
withzeros10km <- readRDS("10kmwithzeroobs.rds")

withzeros10km[is.na(withzeros10km)] <- 0
withzeros10km <- transform(withzeros10km, 
                           AllMammalia = countMammalia + S.vulgaris + M.martes + S.carolinensis)
withzeros10km <- transform(withzeros10km, 
                           Proportion_carolinensis = S.carolinensis/AllMammalia)
withzeros10km <- transform(withzeros10km, Proportion_vulgaris = S.vulgaris/AllMammalia)
withzeros10km<- transform(withzeros10km, Proportion_marten = M.martes/AllMammalia)
#df_Mammalia_GB_count_try <- transform(df_Mammalia_GB_count, Proportion_carolinensis = S.carolinensis/(AllMammalia-S.vulgaris))
#Vegetationraster
Vegetation_europe_squirrels_10km<- readRDS("Vegetation_europe_squirrels_10km")

#combine the grid with the vegetationraster
df_withzeros10km <- merge(withzeros10km, Vegetation_europe_squirrels_10km,
                          by="CELLCODE")


df_withzeros10km <-df_withzeros10km%>%
  mutate_at(vars(clc_9_s, clc_11_s, clc_22_s, clc_23_s, clc_24_s, clc_25_s, clc_39_s, clc_44_s),as.numeric)%>%
  transform(Vegetation = clc_9_s +clc_11_s + clc_22_s +clc_23_s + clc_24_s + clc_25_s + clc_39_s + clc_44_s)%>%
  filter( Vegetation != 0)



df_withzeros10km <- df_withzeros10km%>%
  rename(Grey_urban = clc_9_s, green_urban = clc_11_s, Agrar = clc_22_s, 
         Broadleafed_Forest = clc_23_s, Coniferous_Forest = clc_24_s, Mixed_Forest = clc_25_s,
         Other_seminatural =clc_39_s, Waterbodies =clc_44_s)


df_withzeros10km <- as.data.frame(df_withzeros10km)
df_withzeros10km <- df_withzeros10km[, c("CELLCODE", "year", "M.martes", "S.carolinensis", "S.vulgaris", "countMammalia",
                                         "AllMammalia", "Proportion_marten", "Proportion_carolinensis",
                                         "Proportion_vulgaris", "lat", "lon","Grey_urban",
                                         "green_urban", "Agrar", "Broadleafed_Forest",
                                         "Coniferous_Forest", "Mixed_Forest",
                                         "Other_seminatural", "Waterbodies")]

df_withzeros10km_18 <- df_withzeros10km%>%
  filter(year==2018)


df_withzeros10km_2 <-df_withzeros10km[!duplicated(df_withzeros10km[ , c("CELLCODE")]), ] 
df_10kmzeros <- df_withzeros10km_2 %>%
  filter(year != 2018)%>%
  mutate(Proportion_carolinensis = 0)%>%
  mutate(Proportion_marten = 0)%>%
  mutate(Proportion_vulgaris = 0)

tryyyyy <- rbind(df_withzeros10km_18, df_10kmzeros)
tryyyyy <- tryyyyy[!duplicated(tryyyyy[ , c("CELLCODE")]), ] 



tryyyyy[is.na(tryyyyy)] <- 0
df_10kmzeros <- tryyyyy %>%
  mutate(year = 2018)%>%
  mutate(AllMammalia = 100)


df_10kmzeros%>%
  mutate(across(c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris,
                  Proportion_marten), .names = "{.col}_z")) %>% ## turn a bunch of variable into their scaled form and add _z to their names
  mutate(AllMammalia_log = log(AllMammalia),
         year_from_2000 = year - 2000,
         lon = lon/1e5, lat = lat/1e5) %>% ## that seems to be important, since I guess your weird coordinate yield huge values in matrix computations
  dplyr::select(-c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris,
                   Proportion_marten, AllMammalia, year, CELLCODE)) -> df_10kmzeros_2

df_10kmzeros_predictions <- df_10kmzeros_2 %>%
  transform(predictions_vulgaris =predict(fits_vulgaris_1.25[[1]], newdata= df_10kmzeros_2, type = "response"))

df_10kmzeros_predictions <- df_10kmzeros_predictions %>%
  transform(predictions_caro=predict(fits_carolinensis_1.25[[1]], newdata= df_10kmzeros_2, type = "response"))


kmzeros_predictions  <- df_10kmzeros_predictions %>%
  mutate(lon = lon*1e5, lat = lat*1e5)%>%
  sf::st_as_sf(coords = c(6,5))%>%
  st_set_crs(3035)%>%
  st_transform(4326)

kmzeros_predictions_fürmap_2 <- kmzeros_predictions %>%
  dplyr::mutate(long = sf::st_coordinates(kmzeros_predictions)[,1],
                lat = sf::st_coordinates(kmzeros_predictions)[,2])  

kmzeros_predictions_fürmap_3 <-kmzeros_predictions_fürmap_2%>%
  transform(Vegetation = Grey_urban_z + green_urban_z + Agrar_z +
              Broadleafed_Forest_z + Coniferous_Forest_z + Mixed_Forest_z +
              Other_seminatural_z + Waterbodies_z)%>%
  filter(Vegetation > 0.98)

kmzeros_predictions_fürmap_3 <- kmzeros_predictions_fürmap_3%>%
  filter(Waterbodies_z < 0.9)

png(filename= "Britishislandspredwithzeros.png")

ggmap(myMap_B_toner_2) +
  geom_point(data=kmzeros_predictions_fürmap_3, aes(x=long, y=lat, color = predictions_vulgaris))+
  labs(color = "Predicted S.vulgaris observations")+
  #scale_color_viridis(discrete = F, option = "C")+
  #scale_fill_viridis(discrete = F, trans ="log10") 
  scale_colour_continuous(trans = "log10", type = "viridis")+
  theme(legend.key.size = unit(1.8, 'cm'),text = element_text(size = 12))
# scale_fill_gradient(name = "count", trans = 'log10')
#            breaks = c(0,15,30,45,60,75,90,105,120,135,150), labels = c(0,15,30,45,60,75,90,105,120,135,150))

dev.off()


png(filename= "Britishislandspredwithzeroscaro.png")
ggmap(myMap_B_toner_2) +
  geom_point(data=kmzeros_predictions_fürmap_3, aes(x=long, y=lat, color = predictions_caro))+
  labs(color = "Predicted S.carolinensis observations")+
  #scale_color_viridis(discrete = F, option = "C")+
  #scale_fill_viridis(discrete = F, trans ="log10") 
  scale_colour_continuous(trans = "log10", type = "viridis")+
  theme(legend.key.size = unit(1.6, 'cm'),text = element_text(size = 12))

dev.off()

#Predictions grey urban brauche ich
Values_predictions <- list(0.0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,1)
Values_for_caro <-lapply((Values_predictions), function(i){
  rep(c(i),times=4)
})

Values_for_caro_2 <- as.data.frame(unlist(Values_for_caro))

Values_for_Greyurban <- as.data.frame(rep(c(0.0, 0.3, 0.6, 0.9),times=11))
Values_for_Agrar <- as.data.frame(rep(rev(c(0.1, 0.4, 0.7,1)),times=11))
Predictiondf_1 <- cbind(Values_for_caro_2, Values_for_Greyurban, Values_for_Agrar)
setnames(Predictiondf_1, old = c("unlist(Values_for_caro)", "rep(c(0, 0.3, 0.6, 0.9), times = 11)", "rep(rev(c(0.1, 0.4, 0.7, 1)), times = 11)"), new = c("Proportion_carolinensis_z", "Grey_urban_z", "Agrar_z"), skip_absent = T)

#vulgaris Gebiet
Predictiondf_2 <- Predictiondf_1%>%
  mutate(year_from_2000 = 18)%>%
  mutate(green_urban_z = 0.00)%>%
  mutate(Broadleafed_Forest_z = 0.00)%>%
  mutate(Coniferous_Forest_z = 0.00)%>%
  mutate(Mixed_Forest_z = 0.00)%>%
  mutate(Other_seminatural_z = 0.00)%>%
  mutate(Waterbodies_z = 0.00)%>%
  mutate(Proportion_marten_z = 0.00)%>%
  mutate(lat = 36.15)%>%
  mutate(lon = 31.85) %>%
  mutate(AllMammalia_log = log(100))

Pseudodata_new <- Predictiondf_2                                         # Replicate data
Pseudodata_new$Grey_urban_z2 <- factor(Pseudodata_new$Grey_urban_z, 
                                       labels = c("0% Grey urban", "30% Grey urban", 
                                                  "60% Grey urban","90% Grey urban"))
predictionsgrey <- Pseudodata_new %>%
  transform(predictions=predict(fits_vulgaris_1.25[[1]], newdata=Pseudodata_new, type = "response"))
png(filename= "Predictiondifferentcarolinensisgreyurban.png")

plotpseudonew <- Pseudodata_new %>%
  transform(predictions=predict(fits_vulgaris_1.25[[1]], newdata=Pseudodata_new, type = "response")) %>% 
  ggplot(aes(Proportion_carolinensis_z, predictions)) +
  geom_point()+
  scale_x_continuous(breaks = c(0.00 ,0.20, 0.40, 0.60,0.80,1.00),
                     labels = c("0", "20", "40", "60","80","100"))+
  xlab("Number of S.carolinensis")+
  ylab("Predicted number of S.vulgaris")+
  labs(x=expression(paste("Number of ",italic("S.carolinensis"))))+
  labs(y=expression(paste("Predicted number of ",italic("S.vulgaris"))))+
  theme_minimal()

plotpseudonew +  facet_wrap(. ~ Grey_urban_z2,ncol=2)
dev.off()


Predwithuncert_prediction <- df_10kmzeros_2%>%
  transform( predictions_vul = predict(fits_vulgaris_1.25[[1]], newdata= df_10kmzeros_2))
Predwithuncert_prediction_resvar <- Predwithuncert_prediction %>%
  transform(Response_variance = get_respVar(fits_vulgaris_1.25[[1]], newdata= df_10kmzeros_2))

Predwithuncert_prediction_resvar <- Predwithuncert_prediction_resvar%>%
  transform( Predictions_caro = predict(fits_carolinensis_1.25[[1]], newdata= df_10kmzeros_2))
Predwithuncert_prediction_resvar <- Predwithuncert_prediction_resvar %>%
  transform(Response_variance_2 = get_respVar(fits_carolinensis_1.25[[1]], newdata= df_10kmzeros_2))

#Predwithuncert_anders <- predict(fits_vulgaris_1.25[[1]], newdata= df_10kmzeros_2,variances = list(respVar=TRUE))
#Predwithuncert_anders_2 <- df_10kmzeros_2%>%
# transform(variance_resp = get_respVar(fits_vulgaris_1.25[[1]], newdata= df_10kmzeros_2))


Predwithuncert_prediction_resvar_2 <- Predwithuncert_prediction_resvar%>%
  mutate(lon = lon*1e5, lat = lat*1e5)%>%
  sf::st_as_sf(coords = c(6,5))%>%
  st_set_crs(3035)%>%
  st_transform(4326)

Predwithuncert_prediction_resvar_2 <- Predwithuncert_prediction_resvar_2 %>%
  dplyr::mutate(long = sf::st_coordinates(Predwithuncert_prediction_resvar_2)[,1],
                lat = sf::st_coordinates(Predwithuncert_prediction_resvar_2)[,2])  

Predwithuncert_prediction_resvar_2 <- Predwithuncert_prediction_resvar_2 %>%
  transform(Vegetation = Grey_urban_z + green_urban_z + Agrar_z +
              Broadleafed_Forest_z + Coniferous_Forest_z + Mixed_Forest_z +
              Other_seminatural_z + Waterbodies_z)%>%
  filter(Vegetation > 0.98 & Waterbodies_z < 0.9)
png(filename= "Britishislandspredvariance.png")
ggmap(myMap_B_toner_2) +
  geom_point(data=Predwithuncert_prediction_resvar_2, aes(x=long, y=lat, color = Response_variance))+
  scale_colour_continuous(trans = "log10", type = "viridis")+
  theme(legend.key.size = unit(2, 'cm'),text = element_text(size = 14))+
  labs(color = "Response variance")

dev.off()

png(filename= "Britishislandspredvariancecaro.png")
ggmap(myMap_B_toner_2) +
  geom_point(data=Predwithuncert_prediction_resvar_2, aes(x=long, y=lat, color = Response_variance_2))+
  scale_colour_continuous(trans = "log10", type = "viridis")+
  guides(fill=guide_legend(title="Response variance"))+
  theme(legend.key.size = unit(2, 'cm'),text = element_text(size = 14))+
  labs(color = "Response variance")

dev.off()


#Distribution map 
myMap_B_toner_2
Squir_marten_df_allepub<- mammalia.GB_selected_21 %>%
  filter(datasetKey %in% Mammalia_GB_mixedpub_10km$datasetKey)
#weiter eingrenzen!

Squir_marten_df <- Squir_marten_df_allepub%>%
  filter(species == "Sciurus carolinensis" |species == "Sciurus vulgaris" | species =="Martes martes")

png(filename= "Distributionspecies.png")

ggmap(myMap_B_toner_2) +
  geom_point(data=Squir_marten_df, aes(x=decimalLongitude, y=decimalLatitude, color = species), size=0.5)
dev.off()
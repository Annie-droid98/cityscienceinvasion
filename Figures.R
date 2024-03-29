library(ggpubr)
#citizenscience observations over the years
species_liste <- list("Sciurus carolinensis", "Sciurus vulgaris", "Martes martes")

species <- Mammalia_GB_citizenscience_21 %>%
  count(year, sort = TRUE)
species$species <- "Mammalia"
species<- species%>%
  as.data.frame()%>%
  dplyr::select(year, species, n)

species_diagramm_alle<- lapply((species_liste), function(i){
  species_diagramm_alle <-Mammalia_GB_citizenscience_21 %>%
    filter(species == i)%>%
    count(year,species,  sort = TRUE)%>%
    as.data.frame()%>%
    dplyr::select(year, species, n)
  
})
species_df <- rbind(species_diagramm_alle[[1]],species_diagramm_alle[[2]],species_diagramm_alle[[3]], species)

png(filename= "Line_plot_allspecies.png")

ggplot(species_df, aes(x = year, y = n, color = species)) +
  geom_line()+
  scale_y_log10()+
  ylab("Number of observations")+
  theme_minimal()+
  ggtitle("Number of citizenscience observations over the years")+
  theme(legend.text = element_text(size=10, 
                                   face="italic"))

dev.off()


df_counted_Mammalia <- readRDS("df_counted_Mammalia")

png(filename= "Greyurbanobservations.png")

ggplot(df_counted_Mammalia, aes(x = Grey_urban, y = AllMammalia)) +
  geom_point(alpha = 0.5)+
  scale_y_log10()+
  stat_smooth() +
  geom_density2d()
 # facet_wrap(~year)

dev.off()

png(filename= "Greyurbanobservationscaro.png")

ggplot(df_counted_Mammalia, aes(x = Grey_urban, y = S.carolinensis)) +
  geom_point()+
  facet_wrap(~year)

dev.off()

png(filename= "Greenurbanobservations.png")

ggplot(df_counted_Mammalia, aes(x = green_urban, y = AllMammalia)) +
  geom_point(alpha = 0.5)+
  scale_y_log10()+
  stat_smooth()
# facet_wrap(~year)

dev.off()


png(filename= "Agrarobservations.png")

ggplot(df_counted_Mammalia, aes(x = Agrar, y = AllMammalia)) +
  geom_point(alpha = 0.5)+
  scale_y_log10()+
  stat_smooth() +
  geom_density2d()

dev.off()

png(filename= "Broadleafobservations.png")

ggplot(df_counted_Mammalia, aes(x = Broadleafed_Forest, y = AllMammalia)) +
  geom_point(alpha = 0.5)+
  scale_y_log10()+
  stat_smooth() +
  geom_density2d()
dev.off()

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

png(filename= "Vegetationaverage.png")

ggplot(df_counted_Mammalia_try_3, aes(x = Year, y = Proportion, fill = Vegetationtype)) +
  geom_col() +
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#fec44f","#005a32","#88419d","#addd8e","#737373","#8c2d04","#dd3497","#0c2c84"))+
# scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)

dev.off()

#Average with colsums
df_counted_Mammalia_try_sums <- unique(df_counted_Mammalia_try)
df_counted_Mammalia_try_sums <- colSums(df_counted_Mammalia_try_sums[2:9])%>%
  as.data.frame()
df_counted_Mammalia_try_sums<-tibble::rownames_to_column(df_counted_Mammalia_try_sums, "Vegetationtype")
colnames(df_counted_Mammalia_try_sums)<- c("Vegetationtype","Sums")
df_counted_Mammalia_try_sums <- df_counted_Mammalia_try_sums%>% 
  mutate(Year = "2018")

png(filename= "Vegetationaveragesums.png")

ggplot(df_counted_Mammalia_try_sums, aes(x = Year, y = Sums, fill = Vegetationtype)) +
  geom_col() +
  geom_text(aes(label = paste0(Sums, "")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#fec44f","#005a32","#88419d","#addd8e","#737373","#8c2d04","#dd3497","#0c2c84"))+
  # scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 16) +
  ylab("Sums") +
  xlab(NULL)

dev.off()

#Vegetation Map
cuts_s=c(1,9,11,22,23,24,25,39,44)
Squirrels_map_GB_crop <- crop(x = clc_2018_landcover_squirrels, y = GB_and_IE_grid_10km )
png(filename= "MapSquirrels.png")
#Vegetation_europe_map_squirrels <- plot(Squirrels_map_GB_crop,  legend = FALSE, breaks=cuts_s,col =brewer.pal(n = 8, name = "Set1"))
par(mar= c(5,4,4,10), xpd=TRUE)
plot(Squirrels_map_GB_crop,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"),xlim=c(2900000,4500000), ylim=c(3000000,4500000), main='Landcover of the British islands')
  

legend(x = "topright", inset = c(-0.65, 0),                  
       legend =c("Grey_urban", "green_urban", "Agrar", "Broadleafed_Forrest","Coniferous_Forest",
                  "Mixed_Forest", "Other semi-natural", "Waterbodies"),fill = c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"), xpd=T)


#legend(x = 2500000, y = 4500000, legend = c("Grey_urban", "green_urban", "Agrar", "Broadleafed_Forrest","Coniferous_Forest",
                                          #  "Mixed_Forest", "Other semi-natural", "Waterbodies"),fill = c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"), cex = 1, inset = 1)
dev.off()
# scale_color_manual(values = c("#737373","#b10026","#ffeda0","#005a32","#8c2d04","#7fcdbb","#88419d","#0c2c84"))
#,fill =brewer.pal(n = 8, name = "Set1"), cex = 0.7, inset = 0.9)


#ggplot(data, aes(fill=condition, y=value, x=specie)) + 
 # geom_bar(position="dodge", stat="identity")



clc_2018_landcover_squirrels <- raster("clc_2018_landcover_categories_Squirrels")
Mammalia_in_GB <- Mammalia_GB_citizenscience_21%>%
  dplyr:: select(species, year, long, lat)%>%
  as.data.frame()

Mammalia_in_Gb_2 <- SpatialPointsDataFrame(coords = Mammalia_in_GB[,3:4], data =Mammalia_in_GB)
extract_Mammalia <- raster:: extract(clc_2018_landcover_squirrels ,Mammalia_in_Gb_2 )
Values_Mammalia <- cbind(Mammalia_in_Gb_2,extract_Mammalia)

Values_Mammalia_df <- as.data.frame(Values_Mammalia)

Values_Mammalia_df  <- Values_Mammalia_df%>%
  rename(Vegetation = c.39..44..11..9..22..9..22..9..22..9..9..9..39..9..22..9..11..)

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

png(filename= "ObservationsineachVeg.png")

ggplot(Percentage_obs_inVeg, aes(x = Year, y = n.1, fill = Vegetationtype)) +
  geom_col() +
  geom_text(aes(label = paste0(n.1, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#fec44f","#005a32","#88419d","#addd8e","#737373","#8c2d04","#dd3497","#0c2c84"))+
  # scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)

dev.off()


df_counted_Mammalia_try_3

Percentage_obs_inVeg_try<-Percentage_obs_inVeg%>%
  dplyr::select(Landcover,n.1, Year)
Percentage_obs_inVeg_try<- Percentage_obs_inVeg_try%>%
  rename(Proportion = n.1)
perc_try <-rbind(df_counted_Mammalia_try_3,Percentage_obs_inVeg_try)

png(filename= "ObservationsineachVeg_try.png")

ggplot(perc_try, aes(x = Year, y = Proportion, fill = Vegetationtype)) +
  geom_col(width = 0.9) +
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#fec44f","#005a32","#88419d","#addd8e","#737373","#8c2d04","#dd3497","#0c2c84"))+
  # scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)

dev.off()

perc_try_3 <-perc_try
perc_try_3$Year <-factor(perc_try_3$Year, levels = c("of total area", "of observations"))
perc_try_4 <- transform(perc_try_3, Year_num = ifelse(Year == "of observations",
                                       as.numeric(factor(Year)) - .25,
                                       as.numeric(factor(Year)) + .25) )
 

png(filename= "Stackedbarplotlines.png")
 ggplot(perc_try_4, aes(x = Year, y = Proportion, fill = Landcover)) + 
  geom_bar(width = 0.5,stat = "identity") +
 # scale_x_discrete(limits = c("of total area", "of observations")) +
  geom_line( aes(x = Year_num), 
           position = position_stack())+
geom_text(aes(label = paste0(Proportion, "%")),
          position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#fec44f","#005a32","#8c2d04","#addd8e","#737373","#88419d","#dd3497","#0c2c84"))+
  # scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)


dev.off()


#make the table pretty
install.packages("pixiedust")
install.packages("kableExtra")
library(pixiedust)
library(kableExtra)


Table_carolinensis_Martenproportion <- dust(Model_table_carolinensis_3)%>%
  #sprinkle_colnames()
  kable()%>%
  kable_paper("hover", full_width = F)
  #kable_styling()

Table_vulgaris_Martenproportion <- dust(Model_table_vulgaris_3)%>%
  #sprinkle_colnames()
  kable()%>%
  kable_paper("hover", full_width = F)


Table_carolinensis_MartenTorF <- dust(Model_table_carolinensismartentorf_3)%>%
  #sprinkle_colnames()
  kable()%>%
  kable_paper("hover", full_width = F)

Table_vulgaris_MartenTorF <- dust(Model_table_vulgarismartentorf_3)%>%
  #sprinkle_colnames()
  kable()%>%
  kable_paper("hover", full_width = F)

# Das Beste, was bisher funktioniert!
crop_vegetationsquirrel_Ausschnitt <- crop(x = clc_2018_landcover_squirrels, Ausschnitt_GB)

png(filename= "MapwithGrid.png")

plot(crop_vegetationsquirrel_Ausschnitt,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#88419d","#8c2d04","#dd3497","#0c2c84"),xaxt = "n", yaxt = "n")
axis(1, at = c(3540000, 3570000, 3600000, 3630000, 3660000))
axis(2, at = c(3125000, 3150000, 31750000, 3200000, 3225000))
text(x = crop_Grid_Ausschnitt_2018$lon,
     y = crop_Grid_Ausschnitt_2018$lat,
     labels = crop_Grid_Ausschnitt_2018$AllMammalia, col = "black")
plot(crop_GB_and_IE_grid_10km_shp , add=T)



legend("topright", inset=c(-0.9, .5),legend = c("Grey urban", "Green urban", "Agrar", "Broadleafed_Forrest",
                                                "Mixed_Forest","Coniferous_Forest", "Other semi-natural", "Waterbodies"),fill = c("#737373","#addd8e","#fec44f","#005a32","#88419d","#8c2d04","#dd3497","#0c2c84"))
dev.off()

cutsgrey =c(1,9,44)
png(filename= "MapwithGridgreyurban.png")

plot(crop_vegetationsquirrel_Ausschnitt,  legend = FALSE, breaks=cutsgrey, col= c("#737373","#ece7f2"),xaxt = "n", yaxt = "n")
axis(1, at = c(3540000, 3570000, 3600000, 3630000, 3660000))
axis(2, at = c(3125000, 3150000, 31750000, 3200000, 3225000))
text(x = crop_Grid_Ausschnitt_2018$lon,
     y = crop_Grid_Ausschnitt_2018$lat,
     labels = crop_Grid_Ausschnitt_2018$AllMammalia, col = "black")
plot(crop_GB_and_IE_grid_10km_shp , add=T)



legend("topright", inset=c(-0.9, .5),legend = c("Grey urban", "Green urban", "Agrar", "Broadleafed_Forrest",
                                                "Mixed_Forest","Coniferous_Forest", "Other semi-natural", "Waterbodies"),fill = c("#737373","#addd8e","#fec44f","#005a32","#88419d","#8c2d04","#dd3497","#0c2c84"))
dev.off()



Squirrel_observations <- Mammalia_GB_citizenscience_21%>%
  filter(species == "Sciurus vulgaris" & year == 2020 | species == "Sciurus carolinensis" & year == 2020 )

png(filename= "Mapwithbothsquirrels.png")
map_bothsquirrels <- ggplot(Squirrel_observations) +
  geom_point(aes(x = long, y= lat, color = species)) +
  ggtitle("Observations of the native and the invasive squirrel in 2020")

map_bothsquirrels_2 <- map_bothsquirrels + geom_segment(aes(x = 3200000, y = 3700000, xend = 3185000, yend = 3615000),
                 arrow = arrow(length = unit(0.5, "cm")))
 map_bothsquirrels_2 + geom_segment(aes(x = 3500000, y = 3500000, xend = 3485000, yend = 3445000),
                                 arrow = arrow(length = unit(0.5, "cm")))
#map_bothsquirrels_3 + geom_polygon(data = GB_and_IE_grid_10km_shp, aes( x = long  , y= lat))

dev.off()


#png(filename= "Mapcombined.png")
pdf("plot.pdf", width=15,height=8)
#par(mar= c(5,4,4,10), xpd=TRUE)

par(mfrow=c(1, 2))

plot(Squirrels_map_GB_crop,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"),xlim=c(2900000,4500000), ylim=c(3000000,4500000), main='Landcover of the British islands')


#legend(x = "topright", inset = c(-0.65, 0),                  
       #legend =c("Grey_urban", "green_urban", "Agrar", "Broadleafed_Forrest","Coniferous_Forest",
                # "Mixed_Forest", "Other semi-natural", "Waterbodies"),fill = c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"), xpd=T)
 plot.new()              
 vps <- baseViewports()
 pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
 vp1 <-plotViewport(c(1.8,1,0,1)) 
 
 my_ggplot <- ggplot(perc_try_4, aes(x = Year, y = Proportion, fill = Landcover)) + 
  geom_bar(width = 0.5,stat = "identity") +
  # scale_x_discrete(limits = c("of total area", "of observations")) +
  geom_line( aes(x = Year_num), 
             position = position_stack())+
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#fec44f","#005a32","#8c2d04","#addd8e","#737373","#88419d","#dd3497","#0c2c84"))+
  # scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)
 print(my_ggplot,vp = vp1)  

dev.off()


pdf("Lineandbar.pdf", width= 18, height=8)

my_barggplot <- ggplot(perc_try_4, aes(x = Year, y = Proportion, fill = Landcover)) + 
  geom_bar(width = 0.5,stat = "identity") +
  # scale_x_discrete(limits = c("of total area", "of observations")) +
  geom_line( aes(x = Year_num), 
             position = position_stack())+
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#fec44f","#005a32","#8c2d04","#addd8e","#737373","#88419d","#dd3497","#0c2c84"))+
  # scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)

my_lineggplot <- ggplot(species_df, aes(x = year, y = n, color = species)) +
  geom_line()+
  scale_y_log10()+
  ylab("Number of observations")+
  theme_minimal()+
  ggtitle("Number of citizenscience observations over the years")+
  theme(legend.text = element_text(size=10, 
                                   face="italic"))

ggarrange(my_lineggplot,my_barggplot,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
dev.off()



pdf("GridAusschnitttry.pdf", width=15,height=8)
par(mfrow=c(1, 2))

plot(Squirrels_map_GB_crop,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"),xlim=c(2900000,4500000), ylim=c(3000000,4500000), main='Landcover of the British islands')


plot(crop_vegetationsquirrel_Ausschnitt,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#88419d","#8c2d04","#dd3497","#0c2c84"),xaxt = "n", yaxt = "n")
axis(1, at = c(3540000, 3570000, 3600000, 3630000, 3660000))
axis(2, at = c(3125000, 3150000, 31750000, 3200000, 3225000))
text(x = crop_Grid_Ausschnitt_2018$lon,
     y = crop_Grid_Ausschnitt_2018$lat,
     labels = crop_Grid_Ausschnitt_2018$AllMammalia, col = "black")
plot(crop_GB_and_IE_grid_10km_shp , add=T)

dev.off()

#png(filename= "AlleFiguren.png")
pdf("Allepltsfigure1.pdf", width=7,height=5)
par(mfrow = c(2,2))

myGBmapplot <- plot(Squirrels_map_GB_crop,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"),xlim=c(2900000,4500000), ylim=c(3000000,4500000), main='Landcover of the British islands')


 plot(crop_vegetationsquirrel_Ausschnitt,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#88419d","#8c2d04","#dd3497","#0c2c84"),xaxt = "n", yaxt = "n")
axis(1, at = c(3540000, 3570000, 3600000, 3630000, 3660000))
axis(2, at = c(3125000, 3150000, 31750000, 3200000, 3225000))
 text(x = crop_Grid_Ausschnitt_2018$lon,
     y = crop_Grid_Ausschnitt_2018$lat,
     labels = crop_Grid_Ausschnitt_2018$AllMammalia, col = "black")
 mygridplot <- plot(crop_GB_and_IE_grid_10km_shp , add=T)

 mybarplot <- ggplot(perc_try_4, aes(x = Year, y = Proportion, fill = Landcover)) + 
  geom_bar(width = 0.5,stat = "identity") +
  # scale_x_discrete(limits = c("of total area", "of observations")) +
  geom_line( aes(x = Year_num), 
             position = position_stack())+
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#fec44f","#005a32","#8c2d04","#addd8e","#737373","#88419d","#dd3497","#0c2c84"))+
  # scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)

 mylineplot <-ggplot(species_df, aes(x = year, y = n, color = species)) +
  geom_line()+
  scale_y_log10()+
  ylab("Number of observations")+
  theme_minimal()+
  ggtitle("Number of citizenscience observations over the years")+
  theme(legend.text = element_text(size=10, 
                                   face="italic"))

plot_grid(myGBmapplot, mygridplot, mybarplot, mylineplot, labels = "AUTO", ncol = 2, nrow = 2)
dev.off()



pdf("GridAusschnitttry3.pdf", width= 18, height=8)

par(mar= c(5,4,4,12),mfrow=c(1, 2), xpd=TRUE)
plot(Squirrels_map_GB_crop,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"),xlim=c(2900000,4500000), ylim=c(3000000,4300000), main='Landcover of the British islands')


legend(x = "topright", inset = c(-0.4, 0.30),                  
       legend =c("Grey_urban", "green_urban", "Agrar", "Broadleafed_Forrest","Coniferous_Forest",
                 "Mixed_Forest", "Other semi-natural", "Waterbodies"),fill = c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"), xpd=T)
plot(crop_vegetationsquirrel_Ausschnitt,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"),xaxt = "n", yaxt = "n")
axis(1, at = c(3540000, 3570000, 3600000, 3630000, 3660000))
axis(2, at = c(3125000, 3150000, 31750000, 3200000, 3225000))
text(x = crop_Grid_Ausschnitt_2018$lon,
     y = crop_Grid_Ausschnitt_2018$lat,
     labels = crop_Grid_Ausschnitt_2018$AllMammalia, col = "black", font = 2, cex=1.5)
plot(crop_GB_and_IE_grid_10km_shp , add=T)

dev.off()

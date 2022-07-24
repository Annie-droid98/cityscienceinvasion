install.packages("fasterize")
install.packages("exactextractr")
library(exactextractr)
library(raster)
library(fasterize)
library(sf)
library(dplyr)




clc_2018_landcover <- raster("u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")


#clc_2018_landcover <- readAll(clc_2018_landcover)

#Squirrels 
rasterOptions(tmpdir="path")
clc_2018_landcover_squirrels <- clc_2018_landcover
#"grey urban" 

clc_2018_landcover_squirrels[clc_2018_landcover_squirrels <= 9] <- 9

#green urban
clc_2018_landcover_squirrels[clc_2018_landcover_squirrels %in% c(10, 11)] <- 11

#Agra
clc_2018_landcover_squirrels[clc_2018_landcover_squirrels %in% c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)] <- 22

#Forrest 23, 24, 25 stay



#26-39 other seminatural areas
clc_2018_landcover_squirrels[clc_2018_landcover_squirrels %in% c(26,27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39)] <- 39

# Waterbodies
clc_2018_landcover_squirrels[clc_2018_landcover_squirrels %in% c(40, 41, 42, 43, 44)] <- 44 # this changes all values at once

#save
writeRaster(clc_2018_landcover_squirrels, "clc_2018_landcover_categories_Squirrels")
clc_2018_landcover_squirrels <- raster("clc_2018_landcover_categories_Squirrels")

unique(values(clc_2018_landcover_squirrels)) #[1] NA 40 35  1 12 23
#extract
clc_2018_landcover_squirrels <- exact_extract(x = clc_2018_landcover_squirrels, y = Europe10grid)
clc_2018_landcover_squirrels_2 <- lapply(clc_2018_landcover_squirrels, function(x) x[(names(x) %in% c("value"))])

# empty vectors to store the results
clc_9_s <- vector()
clc_11_s <- vector()
clc_22_s <- vector()
clc_23_s <- vector()
clc_24_s <- vector()
clc_25_s <- vector()
clc_39_s <- vector()
clc_44_s <- vector()
# loop along the list
for (i in 1:length(clc_2018_landcover_squirrels_2)){
  tmp_s <- clc_2018_landcover_squirrels_2[[i]]
  # count each of the values
  tmp_9_s <- sum(tmp_s == 9)
  tmp_11_s <- sum(tmp_s == 11)
  tmp_22_s <- sum(tmp_s == 22)
  tmp_23_s <- sum(tmp_s == 23)
  tmp_24_s <- sum(tmp_s == 24)
  tmp_25_s <- sum(tmp_s == 25)
  tmp_39_s <- sum(tmp_s == 39)
  tmp_44_s <- sum(tmp_s == 44)
  # add the counts to a vector following the same order as the grids
  clc_9_s[i] <- tmp_9_s
  clc_11_s[i] <- tmp_11_s
  clc_22_s[i] <- tmp_22_s
  clc_23_s[i] <- tmp_23_s
  clc_24_s[i] <- tmp_24_s
  clc_25_s[i] <- tmp_25_s
  clc_39_s[i] <- tmp_39_s
  clc_44_s[i] <- tmp_44_s
}

# put the vectors into a df
counts_df_squirrels <- cbind.data.frame(clc_9_s, clc_11_s, clc_22_s,clc_23_s, clc_24_s, clc_25_s, clc_39_s, clc_44_s)
counts_df_squirrels$CellCode <- Europe10grid$CellCode



rowSums(counts_df_Ladybug[, -6]) # 10000 pixels in each grid cell
head(counts_df)


### get proportions 
prop_table_squirrels <- counts_df_squirrels[,1:8]/10000

Vegetation_europe_squirrels_10km <- cbind(prop_table_squirrels, "CELLCODE"=Europe10grid$CellCode)
saveRDS(Vegetation_europe_squirrels_10km, "Vegetation_europe_squirrels_10km")


Squirrels_in_Gb <- Mammalia_GB_citizenscience%>%
  filter(species == "Sciurus vulgaris"| species == "Sciurus carolinensis")%>%
  dplyr:: select(species, year, long, lat)%>%
  as.data.frame()
Squirrels_in_Gb_2 <- SpatialPointsDataFrame(coords = Squirrels_in_Gb[,3:4], data =Squirrels_in_Gb)
extract_squirrels <- raster:: extract(clc_2018_landcover_squirrels ,Squirrels_in_Gb_2 )
Values_squirrel <- cbind(Squirrels_in_Gb_2,extract_squirrels )

Values_squirrel_df <- as.data.frame(Values_squirrel)

Values_squirrel_df  <- Values_squirrel_df%>%
  rename(Vegetation = c.22..11..24..11..11..11..22..11..9..11..11..9..11..9..22..11..)

Vegetation_each_squirrelobs <- Values_squirrel_df$Vegetation

# change fill and outline color manually 
Squirrels_hist <- ggplot(Values_squirrel_df, aes(x = Vegetation)) +
  geom_histogram(aes(color = species, fill = species), 
                 position = "identity", bins = 30, alpha = 0.8) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))


legend(x=52, y=7000, legend = c("Grey urban", "Green urban", "Agrar", "Broadleaf Forest",
                                "Coniferous Forest", "Mixed Forest", "Other semi-natural", "Waterbodies"), cex = 0.7, inset = 0.9)

clc_2018_landcover_squirrels_1km <- raster("clc_2018_landcover_categories_Squirrels")

Grid_1km <- readRDS("1kmGrid.rds")
Grid_1km <- Grid_1km%>%
  st_set_crs(3035)


clc_2018_landcover_squirrels_1km_Grid <- exact_extract(x = clc_2018_landcover_squirrels_1km, y = Grid_1km)
clc_2018_landcover_squirrels_1km_2 <- lapply(clc_2018_landcover_squirrels_1km_Grid, function(x) x[(names(x) %in% c("value"))])


# empty vectors to store the results
clc_9_1 <- vector()
clc_11_1 <- vector()
clc_22_1 <- vector()
clc_23_1 <- vector()
clc_24_1 <- vector()
clc_25_1 <- vector()
clc_39_1 <- vector()
clc_44_1 <- vector()
# loop along the list
for (i in 1:length(clc_2018_landcover_squirrels_1km_2)){
  tmp_s <- clc_2018_landcover_squirrels_1km_2[[i]]
  # count each of the values
  tmp_9_1 <- sum(tmp_s == 9)
  tmp_11_1 <- sum(tmp_s == 11)
  tmp_22_1 <- sum(tmp_s == 22)
  tmp_23_1 <- sum(tmp_s == 23)
  tmp_24_1 <- sum(tmp_s == 24)
  tmp_25_1 <- sum(tmp_s == 25)
  tmp_39_1 <- sum(tmp_s == 39)
  tmp_44_1 <- sum(tmp_s == 44)
  # add the counts to a vector following the same order as the grids
  clc_9_1[i] <- tmp_9_1
  clc_11_1[i] <- tmp_11_1
  clc_22_1[i] <- tmp_22_1
  clc_23_1[i] <- tmp_23_1
  clc_24_1[i] <- tmp_24_1
  clc_25_1[i] <- tmp_25_1
  clc_39_1[i] <- tmp_39_1
  clc_44_1[i] <- tmp_44_1
}

# put the vectors into a df
counts_df_squirrels_1km <- cbind.data.frame(clc_9_1, clc_11_1, clc_22_1,clc_23_1, clc_24_1, clc_25_1, clc_39_1, clc_44_1)
counts_df_squirrels_1km_try <- cbind.data.frame(clc_9_1, clc_11_1, clc_22_1,clc_23_1, clc_24_1, clc_25_1, clc_39_1, clc_44_1)
counts_df_squirrels_1km$CellCode <- Grid_1km$CellCode
counts_df_squirrels_1km_try$CellCode <- Grid_1km$CELLCODE

prop_table_squirrels_1km <- counts_df_squirrels_1km_try[,1:8]/100

Vegetation_europe_squirrels_1km <- cbind(prop_table_squirrels_1km, "CELLCODE"=Grid_1km$CELLCODE)
saveRDS(Vegetation_europe_squirrels_1km, "Vegetation_europe_squirrels_1km.rds")

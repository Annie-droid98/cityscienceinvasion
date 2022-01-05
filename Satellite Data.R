install.packages("fasterize")
install.packages("exactextractr")
library(exactextractr)
library(raster)
library(fasterize)
library(sf)
library(dplyr)


#Copernicusdata

clc_2018_landcover <- raster("u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
#writeRaster(clc_2018_landcover, "EU_CLC_Landcover")

#CLC_Landcover_eu <- raster("EU_CLC_Landcover")




# from 35 to 39 are wetland, merge then togheter


clc_2018_landcover[clc_2018_landcover %in% c(35, 36, 37, 38, 39)] <- 35
#clc_2018_landcover[clc_2018_landcover == 128] <- NA

## same 40 to 44
clc_2018_landcover[clc_2018_landcover %in% c(40, 41, 42, 43, 44)] <- 40 # this changes all values at once
# the others
clc_2018_landcover[clc_2018_landcover <= 11] <- 1
clc_2018_landcover[clc_2018_landcover %in% c(12, 13, 14, 15, 16, 17, 18 ,19, 20, 21, 22)] <- 12
clc_2018_landcover[clc_2018_landcover %in% c(23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34)] <- 23

unique(values(clc_2018_landcover)) #[1] NA 40 35  1 12 23
length(unique(values(clc_2018_landcover)))
writeRaster(clc_2018_landcover, "clc_2018_landcover_Level1")



#all values at once
clc_2018_landcover_Level1 <- raster("clc_2018_landcover_Level1")

clc_extract_gb_and_ie <- exact_extract(x = clc_2018_landcover_Level1, y = GB_and_IE_grid_10km)

clc_extract_gb_and_ie_2 <- clc_extract_gb_and_ie

clc_extract_gb_and_ie_2 <- lapply(clc_extract_gb_and_ie_2, function(x) x[(names(x) %in% c("value"))])

saveRDS(clc_extract_gb_and_ie,"Values_GB_and_IE")



## Because not all categories are in all cells, there is problems if you just put
# the count together.

# Thus, now a fun loop to summarise the count

# empty vectors to store the results
clc_1 <- vector()
clc_12 <- vector()
clc_23 <- vector()
clc_35 <- vector()
clc_40 <- vector()

# loop along the list
for (i in 1:length(clc_extract_gb_and_ie_2)){
  tmp <- clc_extract_gb_and_ie_2[[i]]
  # count each of the values
  tmp_1 <- sum(tmp == 1)
  tmp_12 <- sum(tmp == 12)
  tmp_23 <- sum(tmp == 23)
  tmp_35 <- sum(tmp == 35)
  tmp_40 <- sum(tmp == 40)
  # add the counts to a vector following the same order as the grids
  clc_1[i] <- tmp_1
  clc_12[i] <- tmp_12
  clc_23[i] <- tmp_23
  clc_35[i] <- tmp_35
  clc_40[i] <- tmp_40
}

# put the vectors into a df
counts_df <- cbind.data.frame(clc_1, clc_12, clc_23, clc_35, clc_40)
counts_df$CELLCODE <- GB_and_IE_grid_10km$CELLCODE

summary(counts_df)


rowSums(counts_df[, -6]) # 10000 pixels in each grid cell
head(counts_df)


### get proportions 
prop_table <- counts_df[,1:5]/10000

saveRDS(prop_table, "Vegetation_GB.rds")

#all values at once europe
clc_2018_landcover_Level1 <- raster("clc_2018_landcover_Level1")

clc_extract_europe <- exact_extract(x = clc_2018_landcover_Level1, y = Europe10grid)


clc_extract_europe_2 <- lapply(clc_extract_europe, function(x) x[(names(x) %in% c("value"))])




## Because not all categories are in all cells, there is problems if you just put
# the count together.

# Thus, now a fun loop to summarise the count

# empty vectors to store the results
clc_1_e <- vector()
clc_12_e <- vector()
clc_23_e <- vector()
clc_35_e <- vector()
clc_40_e <- vector()

# loop along the list
for (i in 1:length(clc_extract_europe_2)){
  tmp_e <- clc_extract_europe_2[[i]]
  # count each of the values
  tmp_1_e <- sum(tmp_e == 1)
  tmp_12_e <- sum(tmp_e == 12)
  tmp_23_e <- sum(tmp_e == 23)
  tmp_35_e <- sum(tmp_e == 35)
  tmp_40_e <- sum(tmp_e == 40)
  # add the counts to a vector following the same order as the grids
  clc_1_e[i] <- tmp_1_e
  clc_12_e[i] <- tmp_12_e
  clc_23_e[i] <- tmp_23_e
  clc_35_e[i] <- tmp_35_e
  clc_40_e[i] <- tmp_40_e
}

# put the vectors into a df
counts_df_europe <- cbind.data.frame(clc_1_e, clc_12_e, clc_23_e, clc_35_e, clc_40_e)
counts_df_europe$CellCode <- Europe10grid$CellCode

summary(counts_df_europe)


rowSums(counts_df_europe[, -6]) # 10000 pixels in each grid cell
head(counts_df_europe)


### get proportions 
prop_table_europe <- counts_df_europe[,1:5]/10000

Vegetation_europe_10km <- cbind(prop_table_europe, "CellCode"=Europe10grid$CellCode) 

saveRDS(Vegetation_europe_10km, "Vegetation_Europe.rds")

Vegetation_europe <- readRDS("Vegetation_Europe.rds")


clc_2018_landcover <- raster("u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
#writeRaster(clc_2018_landcover, "EU_CLC_Landcover")

#CLC_Landcover_eu <- raster("EU_CLC_Landcover")


#Categories for the Insecta

clc_2018_landcover_Insecta <- raster("u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")

# Categorie grey urban (Categories 1-9)


clc_2018_landcover_Insecta[clc_2018_landcover_Insecta %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9)] <- 1

#green urban(Categories 10-11) bekomme nur eine error message bisher
#clc_2018_landcover_Insecta[clc_2018_landcover_Insecta %in% c(10, 11)] <- 10

#Agra
clc_2018_landcover_Insecta[clc_2018_landcover_Insecta %in% c(12, 13, 14, 15, 16, 17, 18 ,19, 20, 21, 22)] <- 12


clc_2018_landcover[clc_2018_landcover %in% c(12, 13, 14, 15, 16, 17, 18 ,19, 20, 21, 22)] <- 12
clc_2018_landcover[clc_2018_landcover %in% c(23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34)] <- 23

unique(values(clc_2018_landcover)) #[1] NA 40 35  1 12 23
length(unique(values(clc_2018_landcover)))
writeRaster(clc_2018_landcover, "clc_2018_landcover_Level1")


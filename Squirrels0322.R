install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"),dep=TRUE)
install.packages("geometry")
install.packages("spaMM_3.10.28.tar.gz", type="source", repos=NULL)
library(dplyr)
library(sf)
library(purrr)
library(tidyverse)
library(MASS)
library(spaMM)


#Reading in the Gbif table
Mammalia.GB_2021 <- vroom::vroom("0169558-210914110416597.csv",quote="",show_col_types = FALSE)

#select needed columns
mammalia.GB_selected_21 <- Mammalia.GB_2021 %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, taxonKey, class, order, datasetKey)

#change the crs of the data to match the crs of the grid + exclude the data without coordinates
Mammalia.GB <- mammalia.GB_selected_21 %>% 
  as.data.frame %>%
  filter((! is.na(decimalLatitude)))%>%
  sf::st_as_sf(coords = c(2,3))%>%
  st_set_crs(4326)%>%
  st_transform(st_crs(Grid_ohneduplices))%>%
  mutate(long = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2)))

#Publisherliste
Mammalia_observations_GB <- vroom::vroom("EichhörnchenPublisheruntil1000obs.csv",
                                         quote="",show_col_types = FALSE)

#Filters only citizen science publisher without a focus taxa
Mammalia_GB_citizenscience_until1000obs <- Mammalia_observations_GB%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE")

Mammalia_GB_citizenscience_21<- Mammalia.GB %>%
  filter(datasetKey %in% Mammalia_GB_citizenscience_until1000obs$datasetKey)

#Count of S.vulgaris, S.carolinensis, M.martes, all Mammalia in the 10km grid
# grid ohne duplices: Grid_ohneduplices
Mammalia_GB_count <- Grid_ohneduplices%>%
  st_join(st_sf(Mammalia_GB_citizenscience_21)) %>%
  transform(isVulgaris = species%in%"Sciurus vulgaris", isCarolinensis = species%in%"Sciurus carolinensis", isMartes = species%in%"Martes martes")%>%
  group_by(year,CELLCODE) %>%
  count(isVulgaris, isCarolinensis, isMartes, countMammalia = !isVulgaris&!isCarolinensis&!isMartes)%>%
  transform(what = ifelse(isVulgaris,"S.vulgaris", ifelse(isCarolinensis,"S.carolinensis",
                   ifelse(isMartes,"M.martes", "countMammalia"))))%>%
  spread(what,n)

#middelpoints of the grids habe plötzlich probleme mit dem mutate?? (9.4.22)
Mammalia_GB_count$Centergrid <-st_centroid(Mammalia_GB_count$geometry)
#Mammalia_GB_count <- Mammalia_GB_count%>%
 # mutate(long = unlist(map(Centergrid,1)),
      #   lat = unlist(map(Centergrid,2)))
#das scheint aber zu funktionieren
Mammalia_GB_count <- Mammalia_GB_count%>%
dplyr::mutate(lon = sf::st_coordinates(Mammalia_GB_count$Centergrid)[,1],
              lat = sf::st_coordinates(Mammalia_GB_count$Centergrid)[,2])
#Summarising the counting table
Mammalia_GB_count <- Mammalia_GB_count %>%
  unite('IDYear', CELLCODE:year, remove = FALSE)

df_Mammalia_GB_count <- Mammalia_GB_count %>% 
  arrange(IDYear) %>%
  group_by(IDYear) %>% fill(c(everything()), .direction = "downup") %>% 
  ungroup() %>% 
  distinct(IDYear, .keep_all = T) %>% 
  filter(!is.na(year))

#for a later step in the visualisation of prediction maps
df_Mammalia_GB_count_forPseudodataset <- df_Mammalia_GB_count

df_Mammalia_GB_count[is.na(df_Mammalia_GB_count)] <- 0
df_Mammalia_GB_count <- transform(df_Mammalia_GB_count, 
                        AllMammalia = countMammalia + S.vulgaris + M.martes + S.carolinensis)
df_Mammalia_GB_count <- transform(df_Mammalia_GB_count, 
                        Proportion_carolinensis = S.carolinensis/AllMammalia)
df_Mammalia_GB_count <- transform(df_Mammalia_GB_count, Proportion_vulgaris = S.vulgaris/AllMammalia)
df_Mammalia_GB_count <- transform(df_Mammalia_GB_count, Proportion_marten = M.martes/AllMammalia)
#df_Mammalia_GB_count_try <- transform(df_Mammalia_GB_count, Proportion_carolinensis = S.carolinensis/(AllMammalia-S.vulgaris))
#Vegetationraster
Vegetation_europe_squirrels_10km<- readRDS("Vegetation_europe_squirrels_10km")

#combine the grid with the vegetationraster
df_Mammalia_with_vegetation <- merge(df_Mammalia_GB_count, Vegetation_europe_squirrels_10km,
                                     by="CELLCODE")


df_Mammalia <-df_Mammalia_with_vegetation%>%
  mutate_at(vars(clc_9_s, clc_11_s, clc_22_s, clc_23_s, clc_24_s, clc_25_s, clc_39_s, clc_44_s),as.numeric)%>%
  transform(Vegetation = clc_9_s +clc_11_s + clc_22_s +clc_23_s + clc_24_s + clc_25_s + clc_39_s + clc_44_s)%>%
  filter( Vegetation != 0)


df_Mammalia <- df_Mammalia%>%
  rename(Grey_urban = clc_9_s, green_urban = clc_11_s, Agrar = clc_22_s, 
         Broadleafed_Forest = clc_23_s, Coniferous_Forest = clc_24_s, Mixed_Forest = clc_25_s,
         Other_seminatural =clc_39_s, Waterbodies =clc_44_s)

df_Mammalia <- as.data.frame(df_Mammalia)
df_Mammalia <- df_Mammalia[, c("CELLCODE", "year", "M.martes", "S.carolinensis", "S.vulgaris",
                               "AllMammalia", "Proportion_marten", "Proportion_carolinensis",
                               "Proportion_vulgaris", "lat", "lon","Grey_urban",
                                "green_urban", "Agrar", "Broadleafed_Forest",
                               "Coniferous_Forest", "Mixed_Forest",
                                                   "Other_seminatural", "Waterbodies")]

#df_Mammalia[is.na(df_Mammalia)] <- 0 Das ist der Fehler gewesen!
#save
saveRDS(df_Mammalia, "df_counted_Mammalia")
df_counted_Mammalia <- readRDS("df_counted_Mammalia")

#Models
df_counted_Mammalia %>%
  mutate(across(c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris,
                  Proportion_marten), .fns = scale, .names = "{.col}_z")) %>% ## turn a bunch of variable into their scaled form and add _z to their names
  mutate(AllMammalia_log = log(AllMammalia),
         year_from_2000 = year - 2000,
         long = lon / 1e5, lat = lat/1e5) %>% ## that seems to be important, since I guess your weird coordinate yield huge values in matrix computations
  dplyr::select(-c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris,
                   Proportion_marten, AllMammalia, year, CELLCODE)) -> d ## ditch all columns we don't use

str(unique(d[, c("long", "lat")])) 

mesh <- INLA::inla.mesh.2d(loc = d[, c("long", "lat")], max.n = 100, max.edge = c(3, 20)) 

####  First fit with alpha=1.25 (Matern smoothness=0.25)

# Declare the model with fixed alpha:
corrfamily1.25 <- MaternIMRFa(mesh=mesh, fixed=c(alpha=1.25))

fit1.25 <- fitme(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
                   Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
                   Proportion_marten_z + Proportion_carolinensis_z +
                   Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
                   Proportion_carolinensis_z:Mixed_Forest_z + 
                   Proportion_carolinensis_z:Broadleafed_Forest_z +
                   Proportion_carolinensis_z:Coniferous_Forest_z +
                   corrFamily(1|long+lat), 
                 covStruct=list(corrFamily=corrfamily1.25),
                 family = negbin(link = "log"),
                 init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
                 #  control.HLfit=list(LevenbergM=TRUE),                 # maybe
                 verbose = c(TRACE = TRUE), method="PQL/L",
                 data = d)

####  Second fit with variable alpha

# Declare the model 
corrfamily <- MaternIMRFa(mesh=mesh)

fit_vulgaris <- fitme(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
               Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z +
                 Proportion_marten_z:Proportion_carolinensis_z + Proportion_marten_z +
               Proportion_carolinensis_z*(Mixed_Forest_z + Broadleafed_Forest_z +
                                            Coniferous_Forest_z) +
               corrFamily(1|long+lat), 
             covStruct=list(corrFamily=corrfamily),
             family = negbin(link = "log"),
             init=list(corrPars=list("1"=c(alpha=1.25,kappa=0.74)),NB_shape=2.99, lambda=10),
             verbose = c(TRACE = TRUE), method="PQL/L",
             control.HLfit=list(LevenbergM=TRUE),
             data = d)

### refitting with an IMRF term
if (FALSE) { # NO LONGER NECESSARY
  ranpars <- get_inits_from_fit(fit)$init
  alphafix <- ranpars$corrPars[[1]][["alpha"]]
  
  fittedMRF <- INLA::inla.spde2.matern(mesh=mesh,alpha=alphafix)
  
  ranpars$corrPars[[1]] <- as.list(ranpars$corrPars[[1]]["kappa"])
  
  refit <- fitme(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
                   Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z +
                   Proportion_carolinensis_z*(Mixed_Forest_z + Broadleafed_Forest_z + 
                   Coniferous_Forest_z) +
                   IMRF(1|long+lat, model=fittedMRF),                  #    IMRF model with fixed alpha
                 family = negbin(link = "log",shape = ranpars$NB_shape),      #    <= one fixed value goes here
                 fixed=ranpars,                                               #    others here
                 verbose = c(TRACE = TRUE),
                 method="PQL/L", data = d)
  
  predict(refit, newdata=d[1:6,])
}
#carolinensis

fit1.25_caro <- fitme(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
                        Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z +
                        Proportion_marten_z +
                        Proportion_vulgaris_z*(Mixed_Forest_z + Broadleafed_Forest_z +
                        Coniferous_Forest_z) +
                        corrFamily(1|long+lat), 
                      covStruct=list(corrFamily=corrfamily1.25),
                      family = negbin(link = "log"),
                      init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
                      #control.HLfit=list(LevenbergM=TRUE),                 # maybe
                      verbose = c(TRACE = TRUE), method="PQL/L",
                      data = d)

fit_caro <- fitme(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
                    Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z +
                    Proportion_marten_z:Proportion_vulgaris_z + Proportion_marten_z +
                    Proportion_vulgaris_z*(Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z) +
                    corrFamily(1|long+lat), 
                  covStruct=list(corrFamily=corrfamily),
                  family = negbin(link = "log"),
                  init=list(corrPars=list("1"=c(alpha=1.25,kappa=0.74)),NB_shape=2.99, lambda=10),
                  verbose = c(TRACE = TRUE), method="PQL/L",
                  control.HLfit=list(LevenbergM=TRUE),
                  data = d)


#Prediction maps

my_mapMM <-function (fitobject, Ztransf = NULL, coordinates, add.points, 
                     decorations = NULL, plot.title = NULL, plot.axes = NULL, 
                     envir = -3, nd, ...) 
{
  if (!missing(add.points)) 
    warning("'add.points' is obsolete, use 'decorations'")
  if (missing(coordinates)) {
    info_olduniqueGeo <- attr(fitobject, "info.uniqueGeo")
    if (!is.array(info_olduniqueGeo)) {
      coordinates <- unique(unlist(lapply(info_olduniqueGeo, 
                                          colnames)))
    }
    else coordinates <- colnames(info_olduniqueGeo)
  }
  if (length(coordinates) != 2L) {
    stop(paste0("'mapMM' plots only 2D maps, while coordinates are of length ", 
                length(coordinates)))
  }
  pred <- predict(fitobject, binding = "fitted", newdata = nd)
  x <- pred[, coordinates[1]]
  y <- pred[, coordinates[2]]
  Zvalues <- pred[, attr(pred, "fittedName")]
  if (!is.null(Ztransf)) {
    Zvalues <- do.call(Ztransf, list(Z = Zvalues))
  }
  spaMMplot2D(x = x, y = y, z = Zvalues, decorations = eval(decorations, 
                                                            envir), plot.title = eval(plot.title, envir), plot.axes = eval(plot.axes, 
                                                                                                                           envir), ...)
}

#newdata for the predictions with 0 pine marten in each grid
d_0marten <- d
d_0marten$Proportion_marten_z <- -0.1923294


#new data with proportion 0.33 pine marten in each grid
d_2marten <- d
d_2marten$Proportion_marten_z <- 2.3589747
predict(fit_vul, d_0marten)

#map for vulgaris
mapMM(fitobject = fit,coordinates= c("long", "lat"), Ztransf = function(Z) log(Z))
#map for carolinensis
mapMM(fitobject = fit_caro,coordinates= c("long", "lat"), Ztransf = function(Z) log(Z), nlevel= 4)
my_mapMM(fitobject = fit_caro,coordinates= c("long", "lat"), Ztransf = function(Z) log(Z), nd = d_2marten, nlevel = 4)

### refitting with an IMRF term
if (FALSE) { # NO LONGER NECESSARY
  ranpars <- get_inits_from_fit(fit)$init
  alphafix <- ranpars$corrPars[[1]][["alpha"]]
  
  fittedMRF <- INLA::inla.spde2.matern(mesh=mesh,alpha=alphafix)
  
  ranpars$corrPars[[1]] <- as.list(ranpars$corrPars[[1]]["kappa"])
  
  refit <- fitme(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
                   Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z +
                   Proportion_carolinensis_z*(Mixed_Forest_z + Broadleafed_Forest_z +
                   Coniferous_Forest_z) + IMRF(1|long+lat, model=fittedMRF),                  #    IMRF model with fixed alpha
                 family = negbin(link = "log",shape = ranpars$NB_shape),      #    <= one fixed value goes here
                 fixed=ranpars,                                               #    others here
                 verbose = c(TRACE = TRUE),
                 method="PQL/L", data = d)
  
  predict(refit, newdata=d[1:6,])
}

diff_glmm_formula <- list(
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z  + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            corrFamily(1|long+lat)))

fits_vulgaris <- mclapply((diff_glmm_formula), function(i){
  fitme(i,covStruct=list(corrFamily=corrfamily),
        family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(alpha=1.25,kappa=0.74)),NB_shape=2.99, lambda=10),
        verbose = c(TRACE = TRUE), method="PQL/L",
        control.HLfit=list(LevenbergM=TRUE),
        data = d)
},mc.cores=15)
#,mc.cores=1

diff_glmm_formula_caro <- list(
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + 
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + 
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z +
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + 
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z  +
            corrFamily(1|long+lat)))


fits_carolinensis <- mclapply((diff_glmm_formula_caro), function(i){
  fitme(i,covStruct=list(corrFamily=corrfamily),
        family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(alpha=1.25,kappa=0.74)),NB_shape=2.99, lambda=10),
        verbose = c(TRACE = TRUE), method="PQL/L",
        control.HLfit=list(LevenbergM=TRUE),
        data = d)
},mc.cores=15)

pValues_carolinensis <- lapply((2:17), function(i){
  anova(fits_carolinensis[[1]], fits_carolinensis[[i]])
})

pValues_vulgaris <- lapply((2:17), function(i){
  anova(fits_vulgaris[[1]], fits_vulgaris[[i]])
})

species_diagramm <- Mammalia_GB_citizenscience_21%>%
  filter(species == "Sciurus vulgaris"|species == "Sciurus carolinensis"| species == "Martes martes")
png(filename= "Line_plot_species.png")

ggplot(species_diagramm,aes(year,color = species, fill = species))
+geom_line(aes(fill=..count..),stat="bin",binwidth=1)
+scale_y_log10()

dev.off()


p_values_for_vulgaris_2 <- lapply(pValues_vulgaris, "[[", "basicLRT")%>%
  do.call(rbind, .)


Vulgaris_table <- as.data.frame(spaMM:::.make_beta_table(fits_vulgaris[[1]]))
#p_values_for_vulgaris <- rbind(pValues_vulgaris[[1]]$basicLRT, pValues_vulgaris[[2]]$basicLRT,pValues_vulgaris[[3]]$basicLRT,
 #     pValues_vulgaris[[4]]$basicLRT,pValues_vulgaris[[5]]$basicLRT, pValues_vulgaris[[6]]$basicLRT, 
  #    pValues_vulgaris[[7]]$basicLRT, pValues_vulgaris[[8]]$basicLRT, pValues_vulgaris[[9]]$basicLRT, 
   #   pValues_vulgaris[[10]]$basicLRT, pValues_vulgaris[[11]]$basicLRT, pValues_vulgaris[[12]]$basicLRT, 
    #  pValues_vulgaris[[13]]$basicLRT)
p_values_for_vulgaris<- p_values_for_vulgaris_2 %>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)
Model_table_vulgaris <- cbind(Vulgaris_table, p_values_for_vulgaris)
Model_table_vulgaris_1.1 <- round(Model_table_vulgaris, digits = 3)
Model_table_vulgaris_2 <-Model_table_vulgaris_1.1%>%  
mutate(translation = format(Model_table_vulgaris_1.1$p_value, scientific = FALSE, big.mark = ","))
colnames(Model_table_vulgaris_2) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_vulgaris_2<- tibble::rownames_to_column(Model_table_vulgaris_2, "Predictor")
Model_table_vulgaris_3 <- Model_table_vulgaris_2%>%
  dplyr::select(-c(p_valuescientific))


tab_df(Model_table_vulgaris_3,
       file="Model_table_vulgaris.doc")

Carolinensis_table <- as.data.frame(spaMM:::.make_beta_table(fits_carolinensis[[1]]))

p_values_for_carolinensis_2 <- lapply(pValues_carolinensis, "[[", "basicLRT")%>%
  do.call(rbind, .)
#p_values_for_carolinensis <- rbind(pValues_carolinensis[[1]]$basicLRT, pValues_carolinensis[[2]]$basicLRT,pValues_carolinensis[[3]]$basicLRT,
                            #   pValues_carolinensis[[4]]$basicLRT,pValues_carolinensis[[5]]$basicLRT, pValues_carolinensis[[6]]$basicLRT, 
                             #  pValues_carolinensis[[7]]$basicLRT, pValues_carolinensis[[8]]$basicLRT, pValues_carolinensis[[9]]$basicLRT, 
                              # pValues_carolinensis[[10]]$basicLRT, pValues_carolinensis[[11]]$basicLRT, pValues_carolinensis[[12]]$basicLRT, 
                               #pValues_carolinensis[[13]]$basicLRT)

p_values_for_carolinensis <- p_values_for_carolinensis_2 %>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)
Model_table_carolinensis <- cbind(Carolinensis_table, p_values_for_carolinensis)
Model_table_carolinensis_1.1 <- round(Model_table_carolinensis, digits = 3)
Model_table_carolinensis_2 <-Model_table_carolinensis_1.1%>% 
mutate(translation = format(Model_table_carolinensis_1.1$p_value, scientific = FALSE, big.mark = ","))
colnames(Model_table_carolinensis_2) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_carolinensis_2<- tibble::rownames_to_column(Model_table_carolinensis_2, "Predictor")

Model_table_carolinensis_3 <- Model_table_carolinensis_2%>%
  dplyr::select(-c(p_valuescientific))


library(sjPlot)
tab_df(Model_table_carolinensis_3,
       file="Model_table_carolinensis.doc")




png(filename= "Predictioncarolinensis.png")
plot(y= predict(fit_vulgaris), x= d$S.vulgaris,
     ylab='Predicted Values',
     xlab='Observed Values',
     main='Predicted vs. Observed')
# geom_smooth()+
#  scale_x_log10()+
#  scale_y_log10())

dev.off()


Values_predictions <- list(0.0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,1)
Values_for_caro <-lapply((Values_predictions), function(i){
  rep(c(i),times=11)
})

Values_for_caro_2 <- as.data.frame(unlist(Values_for_caro))
Values_for_marten <- as.data.frame(rep(c(0.0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,1),times=11))
Predictiondf_1 <- cbind(Values_for_caro_2, Values_for_marten)

#Values_for_martenforcaro <-lapply((Values_predictions), function(i){
  #rep(c(i),times=11)
#})
#Values_for_martenforcaro2 <- as.data.frame(unlist(Values_for_martenforcaro))
#Values_for_caroselbst <- as.data.frame(rep(c(0.0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,1),times=11))
#Predicitiondf_2 <- cbind(Values_for_caroselbst, Values_for_martenforcaro2)

#library(data.table)
setnames(Predictiondf_1, old = c("unlist(Values_for_caro)", "rep(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), times = 11)"), new = c("Proportion_carolinensis", "Proportion_marten"), skip_absent = T)
#setnames(Predicitiondf_2, old = c("rep(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), times = 11)", "unlist(Values_for_martenforcaro)"), new = c("Proportion_carolinensis", "Proportion_marten"), skip_absent = T)
#rbind(Predictiondf_1, Predicitiondf_2)



Predictiondf_1.1 <- Predictiondf_1%>%
  mutate(Year = 2020)%>%
  mutate(Grey_urban = 0.05)%>%
  mutate(green_urban = 0.01)%>%
  mutate(Agrar = 0.471)%>%
  mutate(Broadleafed_Forest = 0.016)%>%
  mutate(Coniferous_Forest = 0.039)%>%
  mutate(Mixed_Forest = 0.01)%>%
  mutate(Other_seminatural = 0.201)%>%
  mutate(Waterbodies = 0.201)

Predictiondf_1.2 <- transform(Predictiondf_1.1,
                    Proportions_beide = Proportion_carolinensis + Proportion_marten)

#weil bei 100 % eigentlich keine roten mehr predicted werden sollten
Predictiondf_1.2 <-Predictiondf_1.2%>%
  filter(Proportions_beide <= 0.9)
#0 bis 80 pine marten
Predictiondf_1.2 <- Predictiondf_1.2 %>%
  filter(Proportion_marten < 0.8)

#Predictiondf_1.3 <- Predictiondf_1.2%>%
 # mutate(across(c( Proportion_carolinensis, Proportion_marten,Grey_urban), .fns = scale, .names = "{.col}_z"))

saveRDS(Predictiondf_1.2, "Predicitondatensatz.rds")


scale_Proportion_caro <- scale(Predictiondf_1.2$Proportion_carolinensis, 
                        center=mean(df_counted_Mammalia$Proportion_carolinensis),
                        scale=sd(df_counted_Mammalia$Proportion_carolinensis))
scale_Proportion_m <- scale(Predictiondf_1.2$Proportion_marten, 
                      center=mean(df_counted_Mammalia$Proportion_marten),
                      scale=sd(df_counted_Mammalia$Proportion_marten))
scale_Proportion_Grey_urban <- scale(Predictiondf_1.2$Grey_urban, 
                               center=mean(df_counted_Mammalia$Grey_urban), 
                               scale=sd(df_counted_Mammalia$Grey_urban))  
scale_Proportion_Gree_urban <- scale(Predictiondf_1.2$green_urban, 
                               center=mean(df_counted_Mammalia$green_urban),
                               scale=sd(df_counted_Mammalia$green_urban))
scale_Proportion_Agrar <- scale(Predictiondf_1.2$Agrar,
                          center=mean(df_counted_Mammalia$Agrar), 
                          scale=sd(df_counted_Mammalia$Agrar))
scale_Proportion_Broadleafed_Forest <- scale(Predictiondf_1.2$Broadleafed_Forest,
                                       center=mean(df_counted_Mammalia$Broadleafed_Forest),
                                       scale=sd(df_counted_Mammalia$Broadleafed_Forest))
scale_Proportion_Coniferous_Forest <- scale(Predictiondf_1.2$Coniferous_Forest, 
                                      center=mean(df_counted_Mammalia$Coniferous_Forest),
                                      scale=sd(df_counted_Mammalia$Coniferous_Forest))
scale_Proportion_Mixed_Forest <- scale(Predictiondf_1.2$Mixed_Forest,
                                 center=mean(df_counted_Mammalia$Mixed_Forest),
                                 scale=sd(df_counted_Mammalia$Mixed_Forest))
scale_Proportion_Other_seminatural <- scale(Predictiondf_1.2$Other_seminatural,
                                      center=mean(df_counted_Mammalia$Other_seminatural),
                                      scale=sd(df_counted_Mammalia$Other_seminatural))
scale_Proportion_Waterbodies <- scale(Predictiondf_1.2$Waterbodies,
                                center=mean(df_counted_Mammalia$Waterbodies),
                                scale=sd(df_counted_Mammalia$Waterbodies))



Pseudodata <- as.data.frame(cbind(scale_Proportion_caro, scale_Proportion_m,
              scale_Proportion_Grey_urban, scale_Proportion_Gree_urban,
              scale_Proportion_Agrar, scale_Proportion_Broadleafed_Forest,
              scale_Proportion_Coniferous_Forest, scale_Proportion_Mixed_Forest,
              scale_Proportion_Other_seminatural, scale_Proportion_Waterbodies))
setnames(Pseudodata, old = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10"),
         new = c("Proportion_carolinensis_z", "Proportion_marten_z", "Grey_urban_z",
                 "green_urban_z", "Agrar_z", "Broadleafed_Forest_z", "Coniferous_Forest_z",
                 "Mixed_Forest_z","Other_seminatural_z", "Waterbodies_z"))
Pseudodata_vulgarisGebiet <- Pseudodata%>%
  mutate(year_from_2000 = 20)%>%
  mutate(lat = 36.15)%>%
  mutate(long = 31.85) %>%
  mutate(AllMammalia_log = log(100))

Pseudodata_carolinensisgebiet <- Pseudodata%>%
  mutate(year_from_2000 = 20)%>%
  mutate(lat = 34.45)%>%
  mutate(long = 34.85) %>%
  mutate(AllMammalia_log = log(100))

Pseudodata_Gebietmitbeiden <- Pseudodata%>%
  mutate(year_from_2000 = 20)%>%
  mutate(lat = 35.95)%>%
  mutate(long = 35.05) %>%
  mutate(AllMammalia_log = log(100))

png(filename= "Predictiondifferentcarolinensis.png")
plot(y=(predict(fit_vulgaris, newdata = Pseudodata)), x=(Pseudodata$Proportion_carolinensis_z),
     ylab='Predicted Values',
     xlab='Diff erentProportioncaro',
     main='Predicted vs. Observed')
# geom_smooth()+
#  scale_x_log10()+
#  scale_y_log10())
dev.off()

Pseudodata_new <- Pseudodata_vulgarisGebiet                                         # Replicate data
Pseudodata_new$Proportion_marten_z2 <- factor(Pseudodata_new$Proportion_marten_z, 
                                       labels = c("0 Pine marten", "10 Pine marten",
                                                  "20 Pine marten", "30 Pine marten", 
                                                  "40 Pine marten", "50 Pine marten",
                                                  "60 Pine marten", "70 Pine marten"))

png(filename= "Predictiondifferentcarolinensismarten.png")

plotpseudonew <- Pseudodata_new %>%
  transform(predictions=predict(fits_vulgaris[[1]], newdata=Pseudodata_new, type = "response")) %>% 
 ggplot(aes(Proportion_carolinensis_z, predictions)) +
geom_point()+
 scale_x_continuous(breaks = c(-0.4940462 ,0.7418800, 1.9778062, 3.2137323, 4.4496585),
 labels = c("0", "20", "40", "60","80"))+
 xlab("Number of S.carolinensis")+
ylab("Predicted proportion of S.vulgaris")
 # facet_wrap(~Proportion_marten_z), 
  plotpseudonew +  facet_wrap(. ~ Proportion_marten_z2,ncol=2)
 dev.off()
 
 Pseudodata_new_carolinensisGebiet <- Pseudodata_carolinensisgebiet                                         # Replicate data
 Pseudodata_new_carolinensisGebiet$Proportion_marten_z2 <- factor(Pseudodata_new_carolinensisGebiet$Proportion_marten_z, labels = c("0 Pine marten", "10 Pine marten", "20 Pine marten", "30 Pine marten", "40 Pine marten", "50 Pine marten", "60 Pine marten", "70 Pine marten"))
 png(filename= "PredictiondifferentcarolinensismartencarolinensisGebiet.png")
 
 plotpseudonew_caroGebiet <- Pseudodata_new_carolinensisGebiet %>%
   transform(predictions=predict(fits_vulgaris[[1]], newdata=Pseudodata_new_carolinensisGebiet,
   type = "response")) %>% 
   ggplot(aes(Proportion_carolinensis_z, predictions)) +
   geom_point()+
   scale_x_continuous(breaks = c(-0.4940462 ,0.7418800, 1.9778062, 3.2137323, 4.4496585),
   labels = c("0", "20", "40", "60","80"))+
   xlab("Number of S.carolinensis")+
   ylab("Predicted proportion of S.vulgaris")
 # facet_wrap(~Proportion_marten_z), 
 plotpseudonew_caroGebiet +  facet_wrap(. ~ Proportion_marten_z2,ncol=2)
 dev.off()
 
 Pseudodata_new_Gebietmitbeiden <- Pseudodata_Gebietmitbeiden                                         # Replicate data
 Pseudodata_new_Gebietmitbeiden$Proportion_marten_z2 <- factor(Pseudodata_new_Gebietmitbeiden$Proportion_marten_z, labels = c("0 Pine marten", "10 Pine marten", "20 Pine marten", "30 Pine marten", "40 Pine marten", "50 Pine marten", "60 Pine marten", "70 Pine marten"))
 

 
 png(filename= "PredictiondifferentcarolinensismartenGebietmitbeiden.png")
 
 plotpseudonew_Gebietmitbeiden <- Pseudodata_new_Gebietmitbeiden %>%
   transform(predictions=predict(fits_vulgaris[[1]], newdata=Pseudodata_new_Gebietmitbeiden,
   type = "response")) %>% 
   ggplot(aes(Proportion_carolinensis_z, predictions)) +
   geom_point()+
   scale_x_continuous(breaks = c(-0.4940462 ,0.7418800, 1.9778062, 3.2137323, 4.4496585),
   labels = c("0", "20", "40", "60","80"))+
   xlab("Number of S.carolinensis")+
   ylab("Predicted proportion of S.vulgaris")
 # facet_wrap(~Proportion_marten_z), 
 plotpseudonew_Gebietmitbeiden +  facet_wrap(. ~ Proportion_marten_z2,ncol=2)
 dev.off()
 
#glm.nb(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
 #       Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z +
  #      Proportion_carolinensis_z + Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z, data = d)
 
 
 Predictiondf_1.2_vulgaris <- Predictiondf_1.2%>%
   rename(Proportion_vulgaris = Proportion_carolinensis)
 scale_Proportion_vulgaris <- scale(Predictiondf_1.2_vulgaris$Proportion_vulgaris,
                              center=mean(df_counted_Mammalia$Proportion_vulgaris),
                              scale=sd(df_counted_Mammalia$Proportion_vulgaris))
 Pseudodatavulgaris <- as.data.frame(cbind(scale_Proportion_vulgaris, 
                                     scale_Proportion_m, scale_Proportion_Grey_urban,
                                     scale_Proportion_Gree_urban,
                                   scale_Proportion_Agrar, scale_Proportion_Broadleafed_Forest,
                                   scale_Proportion_Coniferous_Forest,
                                   scale_Proportion_Mixed_Forest, scale_Proportion_Other_seminatural,
                                   scale_Proportion_Waterbodies))
 setnames(Pseudodatavulgaris, old = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10"),
          new = c("Proportion_vulgaris_z", "Proportion_marten_z", "Grey_urban_z",
                  "green_urban_z", "Agrar_z", "Broadleafed_Forest_z", "Coniferous_Forest_z",
                  "Mixed_Forest_z","Other_seminatural_z", "Waterbodies_z"))
 Pseudodatavulgaris <- Pseudodatavulgaris%>%
   mutate(year_from_2000 = 12)%>%
   mutate(lat = 33.75)%>%
   mutate(long = 29.25) %>%
   mutate(AllMammalia_log = log(100))
 
 Pseudodata_vulgaris_new <- Pseudodatavulgaris                                            # Replicate data
 Pseudodata_vulgaris_new$Proportion_marten_z2 <- factor(Pseudodata_vulgaris_new$Proportion_marten_z, labels = c("0 Pine marten", "10 Pine marten", "20 Pine marten", "30 Pine marten", "40 Pine marten", "50 Pine marten", "60 Pine marten", "70 Pine marten"))
 
 png(filename= "Predictiondifferentvulgarismarten.png")
 
 plotpseudovulgarisnew <- Pseudodata_vulgaris_new 
 %>% transform(predictions=predict(fits_carolinensis[[1]], newdata=Pseudodata_vulgaris_new)) %>% 
   ggplot(aes(Proportion_vulgaris_z, predictions)) +
   geom_point()+
   scale_x_continuous(breaks = c(-0.2433489 , 1.0924730 , 2.4282950, 3.7641170 , 5.0999389),
   labels = c("0", "20", "40", "60","80"))+
   xlab("Number of S.vulgaris")+
   ylab("Predicted proportion of S.carolinensis")
 # facet_wrap(~Proportion_marten_z), 
 plotpseudovulgarisnew +  facet_wrap(. ~ Proportion_marten_z2, ncol=2)
 dev.off()
 
 
vulgaris_grids_2020 <- df_counted_Mammalia%>%
   filter(year == 2020)

#Prediction maps:Pseudodataset with 2 times grey squirrel

Pseudodataforpredictionmaps <- df_Mammalia_GB_count_forPseudodataset
Pseudodataforpredictionmaps[is.na(Pseudodataforpredictionmaps)] <- 0
Pseudodataforpredictionmaps$S.carolinensis <- 0
#Pseudodataforpredictionmaps$M.martes <- 0
#Pseudodataforpredictionmaps$S.carolinensis <- Pseudodataforpredictionmaps$S.carolinensis * 10

Pseudodataforpredictionmaps <- transform(Pseudodataforpredictionmaps,
                               AllMammalia = countMammalia + S.vulgaris + M.martes + S.carolinensis)
Pseudodataforpredictionmaps <- transform(Pseudodataforpredictionmaps,
                               Proportion_carolinensis = S.carolinensis/AllMammalia)
Pseudodataforpredictionmaps <- transform(Pseudodataforpredictionmaps,
                               Proportion_vulgaris = S.vulgaris/AllMammalia)
Pseudodataforpredictionmaps <- transform(Pseudodataforpredictionmaps,
                               Proportion_marten = M.martes/AllMammalia)
#Pseudodataforpredictionmaps[is.na(Pseudodataforpredictionmaps)] <- 0
Pseudodataforpredictionmaps_df <- merge(Pseudodataforpredictionmaps, 
                                  Vegetation_europe_squirrels_10km, by="CELLCODE")


df_Pseudodataforpredictionmaps <-Pseudodataforpredictionmaps_df%>%
  mutate_at(vars(clc_9_s, clc_11_s, clc_22_s, clc_23_s, clc_24_s, clc_25_s, clc_39_s, clc_44_s), as.numeric)%>%
  transform(Vegetation = clc_9_s +clc_11_s + clc_22_s +clc_23_s + clc_24_s + clc_25_s + clc_39_s + clc_44_s)%>%
  filter( Vegetation != 0)


df_Pseudodataforpredictionmaps <- df_Pseudodataforpredictionmaps%>%
  rename(Grey_urban = clc_9_s, green_urban = clc_11_s, Agrar = clc_22_s, 
         Broadleafed_Forest = clc_23_s, Coniferous_Forest = clc_24_s, Mixed_Forest = clc_25_s,
         Other_seminatural =clc_39_s, Waterbodies =clc_44_s)

df_Pseudodataforpredictionmaps <- as.data.frame(df_Pseudodataforpredictionmaps)
df_Pseudodataforpredictionmaps <- df_Pseudodataforpredictionmaps[, c("CELLCODE",
                                  "year", "M.martes", "S.carolinensis", "S.vulgaris",
                               "AllMammalia", "Proportion_marten", "Proportion_carolinensis",
                               "Proportion_vulgaris", "lat", "lon","Grey_urban",
                               "green_urban", "Agrar", "Broadleafed_Forest", "Coniferous_Forest",
                               "Mixed_Forest", "Other_seminatural", "Waterbodies")]
df_Pseudodatapredictionmaps_scaled <- transform(df_Pseudodataforpredictionmaps,
                                      Proportion_carolinensis = scale(df_Pseudodataforpredictionmaps$Proportion_carolinensis,
                                      center=mean(df_counted_Mammalia$Proportion_carolinensis),
                                      scale=sd(df_counted_Mammalia$Proportion_carolinensis)))
df_Pseudodatapredictionmaps_scaled_2 <- transform(df_Pseudodatapredictionmaps_scaled,
                                        Proportion_marten = scale(df_Pseudodatapredictionmaps_scaled$Proportion_marten, 
                                        center=mean(df_counted_Mammalia$Proportion_marten),
                                        scale=sd(df_counted_Mammalia$Proportion_marten)))
df_Pseudodatapredictionmaps_scaled_3 <- transform(df_Pseudodatapredictionmaps_scaled_2,
                                        Proportion_vulgaris =scale(df_Pseudodatapredictionmaps_scaled_2$Proportion_vulgaris,
                                        center=mean(df_counted_Mammalia$Proportion_vulgaris),
                                        scale=sd(df_counted_Mammalia$Proportion_vulgaris)))

df_Pseudodatapredictionmaps_scaled_3 %>%
  mutate(across(c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris,
  Proportion_marten), .names = "{.col}_z")) %>% ## turn a bunch of variable into their scaled form and add _z to their names
  mutate(AllMammalia_log = log(AllMammalia),
         year_from_2000 = year - 2000,
         long = lon / 1e5, lat = lat/1e5) %>% ## that seems to be important, since I guess your weird coordinate yield huge values in matrix computations
  dplyr::select(-c(Grey_urban:Waterbodies, Proportion_carolinensis,
  Proportion_vulgaris, Proportion_marten, AllMammalia, year, CELLCODE)) -> df_Pseudodatadoublecaro## ditch all columns we don't use




df_Pseudodatapredictionmaps_scaled_try <- df_Pseudodatadoublecaro%>%
  mutate(across(c(Grey_urban_z:Waterbodies_z),.fns = scale))

png(filename= "Predictionmapfünffachecaros.png")

png(filename= "Predictionmapkeinecaros.png")

my_mapMM(fitobject = fits_vulgaris[[1]],coordinates= c("long", "lat"),
Ztransf = function(Z) log(Z), nd = df_Pseudodatapredictionmaps_scaled_try, nlevels = 10)
dev.off()

png(filename= "Predictionmapnormalecarosecaros.png")
#my_mapMM(fitobject = fits_vulgaris[[1]],coordinates= c("long", "lat"), Ztransf = function(Z) log(Z), nd = d, nlevels = 10)
mapMM(fitobject = fits_vulgaris[[1]],coordinates= c("long", "lat"),
      Ztransf = function(Z) log(Z), nlevels= 10)
dev.off()

df_Pseudodatapredictionmaps_scaled_try_zerocaros <-df_Pseudodatapredictionmaps_scaled_try
df_Pseudodatapredictionmaps_scaled_try_zerocaros$Proportion_carolinensis_z <- -0.4940462
d_zerocaros <- d
d_zerocaros$Proportion_carolinensis_z <- -0.4940462
d_zerocaros$Proportion_marten_z <- -0.1854681

png(filename= "Predictionmapzeroecaros.png")
my_mapMM(fitobject = fits_vulgaris[[1]],coordinates= c("long", "lat"),
         Ztransf = function(Z) log(Z), nd = d_zerocaros, nlevels = 10)
dev.off()

#Wenn in jedem Grid in dem nur rote sind nun auch graue dazu kommen
df_greyandred <- df_counted_Mammalia%>% 
  mutate(Count_scarolinensis = case_when(
  S.vulgaris > 0 & S.carolinensis == 0 ~ 200
))

df_greyandred[is.na(df_greyandred)] <- 0

df_greyandred_2<- transform(df_greyandred,Number_caro = Count_scarolinensis + S.carolinensis)
df_greyandred_2<- transform(df_greyandred_2,  AllMammalia_s = AllMammalia + Count_scarolinensis)
df_greyandred_2<- transform(df_greyandred_2,  Proportion_carolinensis = Number_caro/AllMammalia_s)
df_greyandred_2<- transform(df_greyandred_2,  Proportion_vulgaris = S.vulgaris/AllMammalia_s)
df_greyandred_2<- transform(df_greyandred_2,  Proportion_marten = M.martes/AllMammalia_s)


df_greyandred_2 %>%
  mutate(across(c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris,
        Proportion_marten), .names = "{.col}_z")) %>% ## turn a bunch of variable into their scaled form and add _z to their names
  mutate(AllMammalia_log = log(AllMammalia_s),
         year_from_2000 = year - 2000,
         long = lon / 1e5, lat = lat/1e5) %>% ## that seems to be important, since I guess your weird coordinate yield huge values in matrix computations
  dplyr::select(-c(Grey_urban:Waterbodies, Proportion_carolinensis,
  Proportion_vulgaris, Proportion_marten, AllMammalia, year, CELLCODE,
  Number_caro, Count_scarolinensis)) -> df_df_greyandred

df_df_greyandred_scaled <- transform(df_df_greyandred,
                           Proportion_carolinensis_z = scale(df_df_greyandred$Proportion_carolinensis_z,
                           center=mean(df_counted_Mammalia$Proportion_carolinensis),
                           scale=sd(df_counted_Mammalia$Proportion_carolinensis)))
df_df_greyandred_scaled <- transform(df_df_greyandred_scaled,
                           Proportion_marten_z = scale(df_df_greyandred_scaled$Proportion_marten_z,
                           center=mean(df_counted_Mammalia$Proportion_marten),
                           scale=sd(df_counted_Mammalia$Proportion_marten)))
df_df_greyandred_scaled <- transform(df_df_greyandred_scaled,
                           Proportion_vulgaris_z =scale(df_df_greyandred_scaled$Proportion_vulgaris_z,
                           center=mean(df_counted_Mammalia$Proportion_vulgaris),
                           scale=sd(df_counted_Mammalia$Proportion_vulgaris)))

df_df_greyandred_scaledtry <- df_df_greyandred_scaled %>%
  mutate(across(c(Grey_urban_z:Waterbodies_z),.fns = scale))
df_df_greyandred_scaledtry$Proportion_marten_z <- -0.1854681

png(filename= "Predictionmappluscaroswonurrotewaren.png")
my_mapMM(fitobject = fits_vulgaris[[1]],coordinates= c("long", "lat"),
         Ztransf = function(Z) log(Z), nd = df_df_greyandred_scaledtry, nlevels = 10)
dev.off()

nur_graue <- df_counted_Mammalia%>%
  filter(Proportion_carolinensis == 1)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
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
  st_transform(st_crs(GB_and_IE_grid_10km))%>%
  mutate(long = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2)))

#Publisherliste
Mammalia_observations_GB <- vroom::vroom("EichhörnchenPublisheruntil1000obs.csv",quote="",show_col_types = FALSE)

#Filters only citizen science publisher without a focus taxa
Mammalia_GB_citizenscience_until1000obs <- Mammalia_observations_GB%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE")

Mammalia_GB_citizenscience_21<- Mammalia.GB %>%
  filter(datasetKey %in% Mammalia_GB_citizenscience_until1000obs$datasetKey)

#Count of S.vulgaris, S.carolinensis, M.martes, all Mammalia in the 10km grid
Mammalia_GB_count <- GB_and_IE_grid_10km %>%
  st_join(st_sf(Mammalia_GB_citizenscience_21)) %>%
  transform(isVulgaris = species%in%"Sciurus vulgaris", isCarolinensis = species%in%"Sciurus carolinensis", isMartes = species%in%"Martes martes")%>%
  group_by(year,CELLCODE) %>%
  count(isVulgaris, isCarolinensis, isMartes, countMammalia = !isVulgaris&!isCarolinensis&!isMartes)%>%
  transform(what = ifelse(isVulgaris,"S.vulgaris", ifelse(isCarolinensis,"S.carolinensis", ifelse(isMartes,"M.martes", "countMammalia"))))%>%
  spread(what,n)

#middelpoints of the grids
Mammalia_GB_count$Centergrid <-st_centroid(Mammalia_GB_count$geometry)
Mammalia_GB_count <- Mammalia_GB_count%>%
  mutate(long = unlist(map(Centergrid,1)),
         lat = unlist(map(Centergrid,2)))


#Summarising the counting table
Mammalia_GB_count <- Mammalia_GB_count %>%
  unite('IDYear', CELLCODE:year, remove = FALSE)

df_Mammalia_GB_count <- Mammalia_GB_count %>% 
  arrange(IDYear) %>%
  group_by(IDYear) %>% fill(c(everything()), .direction = "downup") %>% 
  ungroup() %>% 
  distinct(IDYear, .keep_all = T) %>% 
  filter(!is.na(year))

df_Mammalia_GB_count[is.na(df_Mammalia_GB_count)] <- 0
df_Mammalia_GB_count <- transform(df_Mammalia_GB_count, AllMammalia = countMammalia + S.carolinensis + S.vulgaris + M.martes)
df_Mammalia_GB_count <- transform(df_Mammalia_GB_count, Proportion_carolinensis = S.carolinensis/(AllMammalia-S.vulgaris))
df_Mammalia_GB_count <- transform(df_Mammalia_GB_count, Proportion_vulgaris = S.vulgaris/(AllMammalia-S.carolinensis))
df_Mammalia_GB_count <- transform(df_Mammalia_GB_count, Proportion_marten = M.martes/(AllMammalia-S.vulgaris))

#Vegetationraster
Vegetation_europe_squirrels_10km<- readRDS("Vegetation_europe_squirrels_10km")

#combine the grid with the vegetationraster
df_Mammalia_with_vegetation <- merge(df_Mammalia_GB_count, Vegetation_europe_squirrels_10km, by="CELLCODE")

df_Mammalia <-df_Mammalia_with_vegetation%>%
  mutate_at(vars(clc_9_s, clc_11_s, clc_22_s, clc_23_s, clc_24_s, clc_25_s, clc_39_s, clc_44_s), as.numeric)%>%
  transform(Vegetation = clc_9_s +clc_11_s + clc_22_s +clc_23_s + clc_24_s + clc_25_s + clc_39_s + clc_44_s)%>%
  filter( Vegetation != 0)

df_Mammalia <- df_Mammalia%>%
  rename(Grey_urban = clc_9_s, green_urban = clc_11_s, Agrar = clc_22_s, 
         Broadleafed_Forest = clc_23_s, Coniferous_Forest = clc_24_s, Mixed_Forest = clc_25_s,
         Other_seminatural =clc_39_s, Waterbodies =clc_44_s)

df_Mammalia <- as.data.frame(df_Mammalia)
df_Mammalia <- df_Mammalia[, c("CELLCODE", "year", "S.vulgaris",
                                                   "S.carolinensis", "M.martes", "AllMammalia", "Proportion_marten", "Proportion_carolinensis", "Proportion_vulgaris", "lat", "long","Grey_urban",
                                                   "green_urban", "Agrar", "Broadleafed_Forest", "Coniferous_Forest", "Mixed_Forest",
                                                   "Other_seminatural", "Waterbodies")]
df_Mammalia[is.na(df_Mammalia)] <- 0 
#save
saveRDS(df_Mammalia, "df_counted_Mammalia")
df_counted_Mammalia <- readRDS("df_counted_Mammalia")

#Models
df_counted_Mammalia %>%
  mutate(across(c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris, Proportion_marten), .fns = scale, .names = "{.col}_z")) %>% ## turn a bunch of variable into their scaled form and add _z to their names
  mutate(AllMammalia_log = log(AllMammalia),
         year_from_2000 = year - 2000,
         long = long / 1e5, lat = lat/1e5) %>% ## that seems to be important, since I guess your weird coordinate yield huge values in matrix computations
  dplyr::select(-c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris, Proportion_marten, AllMammalia, year, CELLCODE)) -> d ## ditch all columns we don't use

str(unique(d[, c("long", "lat")])) # 3080 locations

mesh <- INLA::inla.mesh.2d(loc = d[, c("long", "lat")], max.n = 100, max.edge = c(3, 20)) 

####  First fit with alpha=1.25 (Matern smoothness=0.25)

# Declare the model with fixed alpha:
corrfamily1.25 <- MaternIMRFa(mesh=mesh, fixed=c(alpha=1.25))

fit1.25 <- fitme(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
                   Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
                   Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
                   Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
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
               Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z +
               Proportion_carolinensis_z*(Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z) +
               corrFamily(1|long+lat), 
             covStruct=list(corrFamily=corrfamily),
             family = negbin(link = "log"),
             init=list(corrPars=list("1"=c(alpha=1.25,kappa=0.74)),NB_shape=2.99, lambda=10),
             verbose = c(TRACE = TRUE), method="PQL/L",
             control.HLfit=list(LevenbergM=TRUE),
             data = d)






#carolinensis

fit1.25_caro <- fitme(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
                        Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z +
                        Proportion_vulgaris_z*(Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z) +
                        corrFamily(1|long+lat), 
                      covStruct=list(corrFamily=corrfamily1.25),
                      family = negbin(link = "log"),
                      init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
                      #control.HLfit=list(LevenbergM=TRUE),                 # maybe
                      verbose = c(TRACE = TRUE), method="PQL/L",
                      data = d)

fit_caro <- fitme(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
                    Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z +
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
                   Proportion_carolinensis_z*(Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z) +
                   IMRF(1|long+lat, model=fittedMRF),                  #    IMRF model with fixed alpha
                 family = negbin(link = "log",shape = ranpars$NB_shape),      #    <= one fixed value goes here
                 fixed=ranpars,                                               #    others here
                 verbose = c(TRACE = TRUE),
                 method="PQL/L", data = d)
  
  predict(refit, newdata=d[1:6,])
}


diff_glmm_formula <- list(
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + 
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z + Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z  + Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z  +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
             Broadleafed_Forest_z + Coniferous_Forest_z +
             Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
            Mixed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),  
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Mixed_Forest_z  + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z  +
            corrFamily(1|long+lat)))

fits_vulgaris <- lapply((diff_glmm_formula), function(i){
  fitme(i,covStruct=list(corrFamily=corrfamily),
        family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(alpha=1.25,kappa=0.74)),NB_shape=2.99, lambda=10),
        verbose = c(TRACE = TRUE), method="PQL/L",
        control.HLfit=list(LevenbergM=TRUE),
        data = d)
})


diff_glmm_formula_caro <- list(
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + 
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z + Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z + Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z  + Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z  +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_vulgaris_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_vulgaris_z +
            Mixed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Mixed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
 formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z + Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            corrFamily(1|long+lat)))


fits_carolinensis <- lapply((diff_glmm_formula_caro), function(i){
  fitme(i,covStruct=list(corrFamily=corrfamily),
        family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(alpha=1.25,kappa=0.74)),NB_shape=2.99, lambda=10),
        verbose = c(TRACE = TRUE), method="PQL/L",
        control.HLfit=list(LevenbergM=TRUE),
        data = d)
})

pValues_vulgaris <- lapply((2:11), function(i){
  anova(fits_carolinensis[[1]], fits_carolinensis[[i]])
})

species_diagramm <- Mammalia_GB_citizenscience_21%>%
  filter(species == "Sciurus vulgaris"|species == "Sciurus carolinensis"| species == "Martes martes")
png(filename= "Line_plot_species.png")

ggplot(species_diagramm,aes(year,color = species, fill = species))
+geom_line(aes(fill=..count..),stat="bin",binwidth=1)
+scale_y_log10()

dev.off()





spaMM:::.make_beta_table(fit_mixed)


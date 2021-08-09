library(vroom)
library(tidyverse)

readfile <- vroom::vroom("0328735-200613084148143.csv",quote="",show_col_types = FALSE)
#read_tsv("0328735-200613084148143.csv",quote="")

readfile_selected <- readfile %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, taxonKey, class)

readfile_selected2.2 <- readfile_selected %>% 
  as.data.frame %>% 
  sf::st_as_sf(coords = c(2,3))

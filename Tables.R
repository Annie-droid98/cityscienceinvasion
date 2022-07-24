install.packages("gtsummary")
install.packages("gapminder")
install.packages("gt")
library(psych)
library(gt)
library(gapminder)
library(tidyverse)
library(gtsummary)



Vulallepubtab <- readRDS("AllePub1.25Tabelle.rds")

Vulallepubtab$Predictor <- c("Intercept", "Year","Grey urban","green_urban", "Agrar",
                             "Other natural", "Proportion marten", "Proportion carolinensis",
                             "Mixed Forest","Broadleafed forest", "Coniferous forest","Proportion marten:Proportion carolinensis", "Grey urban:Proportion carolinensis",
                             "Green urban:Proportion carolinensis", "Mixed forest:Proportion carolinensis","Broadleafed forest:Proportion carolinensis", "Coniferous forest:Proportion carolinensis")
Vulallepubtab$`p-value`[Vulallepubtab$`p-value`=="0.000"] <-"0.001"


Tabelle <- Vulallepubtab%>%
  gt()





Model_table_vulgaris_3_verte

Tabellecaroundvul_2 <- cbind(Vulcitizenscience10kmtab,Model_table_carolinensis_3)
colnames(Tabellecaroundvul_2) <- c("Predictor","Estimate","Cond.SE","t-value","Chi2_LR", "df","p-value","Predictor*","Estimate*","Cond.SE*","t-value*","Chi2_LR*", "df*","p-value*")


Tabelle_fürbeide <-Tabellecaroundvul_2%>%
  gt()%>%
  tab_spanner(label = "Predictor", columns = c("Predictor"))%>%
  tab_spanner(label = md("*S.vulgaris*"), columns = c("Estimate","Cond.SE","t-value","Chi2_LR", "df","p-value"))%>%
  tab_spanner(label = "Predictor*", columns = c("Predictor*"))%>%
  tab_spanner(label = md("*S.carolinensis*"), columns = c("Estimate*","Cond.SE*","t-value*","Chi2_LR*", "df*","p-value*"))

#for S,vulgaris
Tabelle_2 <-Model_table_vulgaris_1.25_3%>%
  gt()%>%
  tab_spanner(label = "Predictor", columns = c("Predictor"))%>%
  tab_spanner(label = md("*S.vulgaris*"), columns = c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p-value"))


#vul cit

Vulcitizenscience10kmtab <- readRDS("Tabellevulgariscitizen.rds")


Vulcitizenscience10kmtab$Predictor <- c("Intercept", "Year","Grey urban","Green urban", "Agricultural",
                                        "Other semi-natural", "Proportion marten", "Proportion carolinensis",
                                        "Mixed Forest","Broadleafed forest", "Coniferous forest","Proportion marten:Proportion carolinensis", "Grey urban:Proportion carolinensis",
                                        "Green urban:Proportion carolinensis", "Mixed forest:Proportion carolinensis","Broadleafed forest:Proportion carolinensis", "Coniferous forest:Proportion carolinensis")
Vulcitizenscience10kmtab$`p-value`[Model_table_vulgaris_1.25_3$`p-value`=="0.000"] <-"0.001"
Tabelle_vulcit <- Vulcitizenscience10kmtab%>%
  gt()%>%
  tab_spanner(label = "Predictor", columns = c("Predictor"))%>%
  tab_spanner(label = md("*S.vulgaris*"), columns = c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p-value"))

#caro cit

Tabellecarocitizenscience <- readRDS("Tabellecarocitizenscience.rds")
Tabellecarocitizenscience$Predictor <- c("Intercept", "Year","Grey urban","Green urban", "Agricultural",
                                         "Other semi-natural", "Proportion marten", "Proportion vulgaris",
                                         "Mixed Forest","Broadleafed forest", "Coniferous forest","Proportion marten:Proportion vulgaris", "Grey urban:Proportion vulgaris",
                                         "Green urban:Proportion vulgaris", "Mixed forest:Proportion vulgaris","Broadleafed forest:Proportion vulgaris", "Coniferous forest:Proportion vulgaris")
Tabellecarocitizenscience$`p-value`[Tabellecarocitizenscience$`p-value`=="0.000"] <-"0.001"

Tabellecarocitizenscience.rds
Tabelle_3 <- Tabellecarocitizenscience%>%
  gt()%>% 
  tab_spanner(label = "Predictor", columns = c("Predictor"))%>%
  tab_spanner(label = md("*S.carolinensis*"), columns = c("Estimate","Cond.SE","t-value","Chi2_LR", "df","p-value"))

#Vulgaris all publisher 


Vul_alle_pub <- readRDS("AllePub1.25Tabelle.rds")

Vul_alle_pub$Predictor <- c("Intercept", "Year","Grey urban","Green urban", "Agricultural",
                            "Other semi-natural", "Proportion marten", "Proportion carolinensis",
                            "Mixed Forest","Broadleafed forest", "Coniferous forest","Proportion marten:Proportion carolinensis", "Grey urban:Proportion carolinensis",
                            "Green urban:Proportion carolinensis", "Mixed forest:Proportion carolinensis","Broadleafed forest:Proportion carolinensis", "Coniferous forest:Proportion carolinensis")
Vul_alle_pub$`p-value`[Vul_alle_pub$`p-value`=="0.000"] <-"0.001"

Tabelle_allepub <- Vul_alle_pub%>%
  gt()%>%
  tab_spanner(label = "Predictor", columns = c("Predictor"))%>%
  tab_spanner(label = md("*S.vulgaris*"), columns = c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p-value"))


#caro
Tabellecaroallepub <- readRDS("Carolinensisalle1.25.rds")
Tabellecaroallepub$Predictor <- c("Intercept", "Year","Grey urban","Green urban", "Agricultural",
                                  "Other semi-natural", "Proportion marten", "Proportion vulgaris",
                                  "Mixed Forest","Broadleafed forest", "Coniferous forest","Proportion marten:Proportion vulgaris", "Grey urban:Proportion vulgaris",
                                  "Green urban:Proportion vulgaris", "Mixed forest:Proportion vulgaris","Broadleafed forest:Proporton vulgaris", "Coniferous forest:Proportion vulgaris")
Tabellecaroallepub$`p-value`[Tabellecaroallepub$`p-value`=="0.000"] <-"0.001"

Tabelle_allepubcaro <-Tabellecaroallepub%>%
  gt()%>%
  tab_spanner(label = "Predictor", columns = c("Predictor"))%>%
  tab_spanner(label = md("*S.carolinensis*"), columns = c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p-value"))

Vertebratavulgaristabelle <- readRDS("Vertebratavulgaristabellecit.rds")
Vertebratavulgaristabelle$Predictor <- c("Intercept", "Year","Grey urban","Green urban", "Agricultural",
                                         "Other semi-natural", "Proportion marten", "Proportion carolinensis",
                                         "Mixed Forest","Broadleafed forest", "Coniferous forest","Proportion marten:Proportion carolinensis", "Grey urban:Proportion carolinensis",
                                         "Green urban:Proportion carolinensis", "Mixed forest:Proportion carolinensis","Broadleafed forest:Proportion carolinensis", "Coniferous forest:Proportion carolinensis")
Vertebratavulgaristabelle$`p-value`[Vertebratavulgaristabelle$`p-value`=="0.000"] <-"0.001"

Tabelle_moreverte_cit <- Vertebratavulgaristabelle%>%
  gt()%>%
  tab_spanner(label = "Predictor", columns = c("Predictor"))%>%
  tab_spanner(label = md("*S.vulgaris*"), columns = c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p-value"))

#caro verte cit

Caromoreverte_cit <- readRDS("Carolinensismorevertetabellecit.rds")

Caromoreverte_cit$Predictor <- c("Intercept", "Year","Grey urban","Green urban", "Agricultural",
                                 "Other semi-natural", "Proportion marten", "Proportion vulgaris",
                                 "Mixed Forest","Broadleafed forest", "Coniferous forest","Proportion marten:Proportion vulgaris", "Grey urban:Proportion vulgaris",
                                 "Green urban:Proportion vulgaris", "Mixed forest:Proportion vulgaris","Broadleafed forest:Proporton vulgaris", "Coniferous forest:Proportion vulgaris")
Caromoreverte_cit$`p-value`[Caromoreverte_cit$`p-value`=="0.000"] <-"0.001"

Tabellecaromorevert_cit <- Caromoreverte_cit%>%
  gt()%>%
  tab_spanner(label = "Predictor", columns = c("Predictor"))%>%
  tab_spanner(label = md("*S.carolinensis*"), columns = c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p-value"))

#alle
Vertevul_alle <- readRDS("Vertebratavulgaristabellealle.rds")


Vertevul_alle$Predictor <- c("Intercept", "Year","Grey urban","Green urban", "Agricultural",
                             "Other semi-natural", "Proportion marten", "Proportion carolinensis",
                             "Mixed Forest","Broadleafed forest", "Coniferous forest","Proportion marten:Proportion carolinensis", "Grey urban:Proportion carolinensis",
                             "Green urban:Proportion carolinensis", "Mixed forest:Proportion carolinensis","Broadleafed forest:Proportion carolinensis", "Coniferous forest:Proportion carolinensis")
Vertevul_alle$`p-value`[Vertevul_alle$`p-value`=="0.000"] <-"0.001"

Tabelle_moreverte_alle <- Vertevul_alle %>%
  gt()%>%
  tab_spanner(label = "Predictor", columns = c("Predictor"))%>%
  tab_spanner(label = md("*S.vulgaris*"), columns = c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p-value"))


#alle caro
Caromoreverte_alle <- readRDS("fits_carolinensis_moreverteallepub.rds")

Caromoreverte_alle$Predictor <- c("Intercept", "Year","Grey urban","Green urban", "Agricultural",
                                  "Other semi-natural", "Proportion marten", "Proportion vulgaris",
                                  "Mixed Forest","Broadleafed forest", "Coniferous forest","Proportion marten:Proportion vulgaris", "Grey urban:Proportion vulgaris",
                                  "Green urban:Proportion vulgaris", "Mixed forest:Proportion vulgaris","Broadleafed forest:Proporton vulgaris", "Coniferous forest:Proportion vulgaris")
Caromoreverte_alle$`p-value`[Caromoreverte_alle$`p-value`=="0.000"] <-"0.001"

Tabellecaromorevert_alle <- Caromoreverte_alle%>%
  gt()%>%
  tab_spanner(label = "Predictor", columns = c("Predictor"))%>%
  tab_spanner(label = md("*S.carolinensis*"), columns = c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p-value"))




citizenvul <-  Mammalia_observations_GB%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE" )

cityvul<- mammalia.GB_selected_21 %>% 
  as.data.frame %>%
  filter((! is.na(decimalLatitude)))%>%
  sf::st_as_sf(coords = c(2,3))%>%
  st_set_crs(4326)

Mammalia_GB_mixed_pub10km<- cityvul%>%
  filter(datasetKey %in% citizenvul$datasetKey)

Mammalia_GB_mixed_pub10km<- Mammalia_GB_mixed_pub10km%>%
  filter(year==2020 & species == "Sciurus vulgaris")

Mammalia_GB_mixed_pub10km<- Mammalia_GB_mixed_pub10km%>%
  dplyr::mutate(long = sf::st_coordinates(Mammalia_GB_mixed_pub10km)[,1],
                lat = sf::st_coordinates(Mammalia_GB_mixed_pub10km)[,2])

png(filename= "Vulmap.png")
ggmap(myMap_B_toner_2) +
  geom_point(data=Mammalia_GB_mixed_pub10km, aes(x=long, y=lat, color = species))
dev.off()


Tabelle_Modelle<- vroom::vroom("ModelleEichhörnchen.csv",quote="",show_col_types = FALSE)

Tabelle_Modelle_2 <-Tabelle_Modelle%>%
  gt()

#Table to compare the different models

TabelleModelle_hochkant<- read.csv("Vulgarismodellehoch.csv",sep = ';')
TabelleModelle_hochkant <- TabelleModelle_hochkant%>%
  gt()
nm2 <- c("Modell","Citizenscience_10km","AllePublisher_10km","X.Citizenscience_10km","X.AllePublisher_10km")

for(i in seq_along(nm2)) {
  
  TabelleModelle_hochkant <- TabelleModelle_hochkant %>%
    tab_style(
      style = list(
        cell_text(color = "red", weight = "bold")
        
      ),
      locations = cells_body(
        columns = nm2[i],
        
        rows = TabelleModelle_hochkant$`_data`[[nm2[i]]] == "neg" 
      )
    ) %>%
    
    tab_style(
      style = list(
        cell_text(color = "green", weight = "bold")
        
      ),
      locations = cells_body(
        columns = nm2[i],
        
        rows = TabelleModelle_hochkant$`_data`[[nm2[i]]] == "pos"
      )
    ) 
  
}

TabelleModelle_caro<- read.csv("CarolinensisModelle.csv",sep = ';')
TabellealleModelle_caro <- TabelleModelle_caro%>%
  gt()
nm2 <- c("Modell","Citizenscience_10km","AllePublisher_10km","X.Citizenscience_10km","X.AllePublisher_10km")

for(i in seq_along(nm2)) {
  
  TabellealleModelle_caro <- TabellealleModelle_caro %>%
    tab_style(
      style = list(
        cell_text(color = "red", weight = "bold")
        
      ),
      locations = cells_body(
        columns = nm2[i],
        
        rows = TabellealleModelle_caro$`_data`[[nm2[i]]] == "neg" 
      )
    ) %>%
    
    tab_style(
      style = list(
        cell_text(color = "green", weight = "bold")
        
      ),
      locations = cells_body(
        columns = nm2[i],
        
        rows = TabellealleModelle_caro$`_data`[[nm2[i]]] == "pos"
      )
    ) 
  
}


TabelleModelle<- read.csv("Vulgarismodellehoch.csv",sep = '')


#nm1 <- c("Estimate_year",	"Grey_urban", "Green_urban", 	"Agricultural", "other_seminatural",
#        "Proportion_marten", "Proportion_carolinensis","Mixed_forest","Broadleaf_forest", "Coniferous_forest","Proportion_marten.Proportion_carolinensis",
#       "Grey_urban.Proportion_carolinensis","Green_urban.Proportion_carolinensis","Mixed_forest.Proportion_carolinensis","Broadleaf_forest.Proportion_carolinensis", "Coniferous_forest.Proportion_carolinensis")
TabellealleModelle <- TabelleModelle%>%
  gt()



TabellealleModelle3 <- TabellealleModelle %>%
  tab_style(
    style = cell_text(color = "green", weight = "bold"),
    locations = cells_body(
      columns = 2, 
      rows = Citizenscience_10km %in% c(0.126, 4.899,3.684)
    )
  )%>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = 2, 
      rows = Citizenscience_10km %in% c(-0.575,-1.238,-4.625)
    )
  )%>%
  tab_style(
    style = cell_text(color = "green", weight = "bold"),
    locations = cells_body(
      columns = 3, 
      rows = AllePublisher_10km %in% c(0.062, 6.756, 2.184,3.083,3.228,22.284 )
    )
  )%>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = 3, 
      rows = AllePublisher_10km %in% c(-0.518,-0.636,-4.632)
    )
  )%>%
  tab_style(
    style = cell_text(color = "green", weight = "bold"),
    locations = cells_body(
      columns = 4, 
      rows = X.Citizenscience_10km %in% c(0.065,-0.140,5.704)
    )
  )%>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = 4, 
      rows = X.Citizenscience_10km %in% c(-3.466,-1.665)
    )
  )%>%
  tab_style(
    style = cell_text(color = "green", weight = "bold"),
    locations = cells_body(
      columns = 5, 
      rows = X.AllePublisher_10km %in% c(0.141,0.782,4.650,99.789)
    )
  )%>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = 5, 
      rows = X.AllePublisher_10km %in% c(-4.412,-1.248)
    )
  )%>%
  tab_footnote(
    footnote = "Normelisationtaxon : Mammalia,Aves,Amphibia and Reptilia observations",
    locations = cells_column_labels(
      columns = c("X.Citizenscience_10km")
    ))%>%
  tab_footnote(
    footnote = "Normelisationtaxon : Mammalia,Aves,Amphibia and Reptilia observations",
    locations = cells_column_labels(
      columns = c("X.AllePublisher_10km")
    ))



#caro 
TabelleModelle_caro<- read.csv("CarolinensisModelle2.csv",sep = ';')
TabelleModelle_caro<- TabelleModelle_caro[-c(17), ] 

#nm1 <- c("Estimate_year",	"Grey_urban", "Green_urban", 	"Agricultural", "other_seminatural",
#        "Proportion_marten", "Proportion_carolinensis","Mixed_forest","Broadleaf_forest", "Coniferous_forest","Proportion_marten.Proportion_carolinensis",
#       "Grey_urban.Proportion_carolinensis","Green_urban.Proportion_carolinensis","Mixed_forest.Proportion_carolinensis","Broadleaf_forest.Proportion_carolinensis", "Coniferous_forest.Proportion_carolinensis")
TabellealleModelle_carol <- TabelleModelle_caro%>%
  gt()


TabellealleModelle_carol2 <- TabellealleModelle_carol %>%
  tab_style(
    style = cell_text(color = "green", weight = "bold"),
    locations = cells_body(
      columns = 2, 
      rows = Citizenscience_10km %in% c(0.013,2.074,2.670,0.403,1.741,2.487)
    )
  )%>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = 2, 
      rows = Citizenscience_10km %in% c(-0.469,-0.817)
    )
  )%>%
  tab_style(
    style = cell_text(color = "green", weight = "bold"),
    locations = cells_body(
      columns = 3, 
      rows = AllePublisher_10km %in% c(0.037,2.028,2.317,0.395,1.339,2.268)
    )
  )%>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = 3, 
      rows = AllePublisher_10km %in% c(-0.472,-1.145)
    )
  )%>%
  tab_style(
    style = cell_text(color = "green", weight = "bold"),
    locations = cells_body(
      columns = 4, 
      rows = X.Citizenscience_10km %in% c(0.029,2.475,1.00,0.624,3.908,58.061)
    )
  )%>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = 4, 
      rows = X.Citizenscience_10km %in% c(-1.217,-10.075)
    )
  )%>%
  tab_style(
    style = cell_text(color = "green", weight = "bold"),
    locations = cells_body(
      columns = 5, 
      rows = X.AllePublisher_10km %in% c(0.111,2.055,0.316,0.981,0.354,3.199,1.269,75.925)
    )
  )%>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = 5, 
      rows = X.AllePublisher_10km %in% c(-1.127,-12.645)
    )
  )%>%
  tab_footnote(
    footnote = "Normelisationtaxon : Mammalia,Aves,Amphibia and Reptilia observations",
    locations = cells_column_labels(
      columns = c("X.Citizenscience_10km")
    ))%>%
  tab_footnote(
    footnote = "Normelisationtaxon : Mammalia,Aves,Amphibia and Reptilia observations",
    locations = cells_column_labels(
      columns = c("X.AllePublisher_10km")
    ))

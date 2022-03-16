
#citizenscience observations over the years
species_liste <- list("Sciurus carolinensis", "Sciurus vulgaris", "Martes martes")

species <- Mammalia_GB_citizenscience_21 %>%
  count(year, sort = TRUE)
species$species <- "AllMammalia"
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
  ylab("log(n)")+
  ggtitle("Number of citizenscience observations over the years")
dev.off()

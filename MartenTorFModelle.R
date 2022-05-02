
#models wit pine marten T or F not as Proportions
dfwithmartenTorF <- df_counted_Mammalia%>%
  mutate(MartenTorF = case_when(
    Proportion_marten > 0 ~ "T",
    Proportion_marten == 0  ~ "F"
  ))

dfwithmartenTorF %>%
  mutate(across(c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris, Proportion_marten), .fns = scale, .names = "{.col}_z")) %>% ## turn a bunch of variable into their scaled form and add _z to their names
  mutate(AllMammalia_log = log(AllMammalia),
         year_from_2000 = year - 2000,
         long = lon / 1e5, lat = lat/1e5) %>% ## that seems to be important, since I guess your weird coordinate yield huge values in matrix computations
  dplyr::select(-c(Grey_urban:Waterbodies, Proportion_carolinensis, Proportion_vulgaris, Proportion_marten, AllMammalia, year, CELLCODE)) -> datamarten

mesh_marten <- INLA::inla.mesh.2d(loc = datamarten[, c("long", "lat")], max.n = 100, max.edge = c(3, 20)) 
corrfamily_marten <- MaternIMRFa(mesh=mesh_marten)

nur_rote <- df_counted_Mammalia%>%
  filter(Proportion_vulgaris == 1)

formula_vulgaris_martentorf <- list(
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z  + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:MartenTorF +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            corrFamily(1|long+lat)))
fits_vulgaris_martentorf <- mclapply((formula_vulgaris_martentorf), function(i){
  fitme(i,covStruct=list(corrFamily=corrfamily_marten),
        family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(alpha=1.25,kappa=0.74)),NB_shape=2.99, lambda=10),
        verbose = c(TRACE = TRUE), method="PQL/L",
        control.HLfit=list(LevenbergM=TRUE),
        data = datamarten)
},mc.cores=15)


pValues_vulgaris_martenTorF <- lapply((2:17), function(i){
  anova(fits_vulgaris_martentorf[[1]], fits_vulgaris_martentorf[[i]])
})

p_values_for_vulgaris_martenTorF_2 <- lapply(pValues_vulgaris_martenTorF, "[[", "basicLRT")%>%
  do.call(rbind, .)
Vulgarismartentorf_table <- as.data.frame(spaMM:::.make_beta_table(fits_vulgaris_martentorf[[1]]))

p_values_for_vulgarismartentorf <- p_values_for_vulgaris_martenTorF_2 %>% add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)
Model_table_vulgarismartentorf <- cbind(Vulgarismartentorf_table, p_values_for_vulgarismartentorf )
Model_table_vulgarismartentorf_1.1 <- round(Model_table_vulgarismartentorf, digits = 3)
Model_table_vulgarismartentorf_2 <-Model_table_vulgarismartentorf_1.1%>%  mutate(translation = format(Model_table_vulgarismartentorf_1.1$p_value, scientific = FALSE, big.mark = ","))
colnames(Model_table_vulgarismartentorf_2) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_vulgarismartentorf_2<- tibble::rownames_to_column(Model_table_vulgarismartentorf_2, "Predictor")
Model_table_vulgarismartentorf_3 <- Model_table_vulgarismartentorf_2%>%
  dplyr::select(-c(p_valuescientific))


#S.carolinensis TorF

formula_carolinensis_martentorf <- list(
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z  + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            corrFamily(1|long+lat)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + MartenTorF+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:MartenTorF +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            corrFamily(1|long+lat)))
fits_carolinensis_martentorf <- mclapply((formula_carolinensis_martentorf), function(i){
  fitme(i,covStruct=list(corrFamily=corrfamily_marten),
        family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(alpha=1.25,kappa=0.74)),NB_shape=2.99, lambda=10),
        verbose = c(TRACE = TRUE), method="PQL/L",
        control.HLfit=list(LevenbergM=TRUE),
        data = datamarten)
},mc.cores=15)


pValues_carolinensis_martenTorF <- lapply((2:17), function(i){
  anova(fits_carolinensis_martentorf[[1]], fits_carolinensis_martentorf[[i]])
})

p_values_for_carolinensis_martenTorF_2 <- lapply(pValues_carolinensis_martenTorF, "[[", "basicLRT")%>%
  do.call(rbind, .)
Carolinensismartentorf_table <- as.data.frame(spaMM:::.make_beta_table(fits_carolinensis_martentorf[[1]]))

p_values_for_carolinensismartentorf <- p_values_for_carolinensis_martenTorF_2 %>% add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)
Model_table_carolinensismartentorf <- cbind(Carolinensismartentorf_table, p_values_for_carolinensismartentorf )
Model_table_carolinensismartentorf_1.1 <- round(Model_table_carolinensismartentorf, digits = 3)
Model_table_carolinensismartentorf_2 <-Model_table_carolinensismartentorf_1.1%>%  mutate(translation = format(Model_table_carolinensismartentorf_1.1$p_value, scientific = FALSE, big.mark = ","))
colnames(Model_table_carolinensismartentorf_2) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_carolinensismartentorf_2<- tibble::rownames_to_column(Model_table_carolinensismartentorf_2, "Predictor")
Model_table_carolinensismartentorf_3 <- Model_table_carolinensismartentorf_2%>%
  dplyr::select(-c(p_valuescientific))



#Pseudodata MartenTorF


#Values_for_marten_TorF <- as.data.frame(rep(c(T), times = 11))
Values_for_marten_TorF <- as.data.frame(c(T,T,T,T,T,T,T,T,T,T,F,F,F,F,F,F,F,F,F,F,F))
Values_for_caro_try <- as.data.frame(c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))


Predictiondf_MartenTor_F<- cbind(Values_for_caro_try,Values_for_marten_TorF)

#setnames(Predictiondf_MartenTor_F, old = c("unlist(Values_for_caro_MartenTortF)", "rep(c(T, F), times = 11)"), new = c("Proportion_carolinensis", "MartenTorF"), skip_absent = T)
setnames(Predictiondf_MartenTor_F, old = c("c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)", "c(T, T, T, T, T, T, T, T, T, T, F, F, F, F, F, F, F, F, F, F, F)"), new = c("Proportion_carolinensis", "MartenTorF"), skip_absent = T)
#remove row 1.0 and T because if 100% are S.carolinensis there not can be marten


#add continous year + average grid
Predictiondf_MartenTor_F_2 <- Predictiondf_MartenTor_F%>%
  mutate(Year = 12)%>%
  mutate(Grey_urban = 0.05)%>%
  mutate(green_urban = 0.01)%>%
  mutate(Agrar = 0.471)%>%
  mutate(Broadleafed_Forest = 0.016)%>%
  mutate(Coniferous_Forest = 0.039)%>%
  mutate(Mixed_Forest = 0.01)%>%
  mutate(Other_seminatural = 0.201)%>%
  mutate(Waterbodies = 0.201)

#scale the values

scale_Proportion_caro_MartenTorF <- scale(Predictiondf_MartenTor_F_2$Proportion_carolinensis, center=mean(df_counted_Mammalia$Proportion_carolinensis), scale=sd(df_counted_Mammalia$Proportion_carolinensis))
#scale_Proportion_Marten <- scale(Predictiondf_1.2$Proportion_marten, center=mean(df_counted_Mammalia$Proportion_marten), scale=sd(df_counted_Mammalia$Proportion_marten))

Values_for_marten_TorF_try_2 <- as.data.frame(c("T","T","T","T","T","T","T","T","T","T","F","F","F","F","F","F","F","F","F","F","F"))
colnames(Values_for_marten_TorF_try_2) <- "MartenTorF"


scale_Proportion_Grey_urban_martenTorF <- scale(Predictiondf_MartenTor_F_2$Grey_urban, center=mean(df_counted_Mammalia$Grey_urban), scale=sd(df_counted_Mammalia$Grey_urban))  

scale_Proportion_Gree_urban_martenTorF <- scale(Predictiondf_MartenTor_F_2$green_urban, center=mean(df_counted_Mammalia$green_urban), scale=sd(df_counted_Mammalia$green_urban))

scale_Proportion_Agrar_martenTorF <- scale(Predictiondf_MartenTor_F_2$Agrar, center=mean(df_counted_Mammalia$Agrar), scale=sd(df_counted_Mammalia$Agrar))

scale_Proportion_Broadleafed_Forest_martenTorF <- scale(Predictiondf_MartenTor_F_2$Broadleafed_Forest, center=mean(df_counted_Mammalia$Broadleafed_Forest), scale=sd(df_counted_Mammalia$Broadleafed_Forest))
scale_Proportion_Coniferous_Forest_martenTorF <- scale(Predictiondf_MartenTor_F_2$Coniferous_Forest, center=mean(df_counted_Mammalia$Coniferous_Forest), scale=sd(df_counted_Mammalia$Coniferous_Forest))

scale_Proportion_Mixed_Forest_martenTorF <- scale(Predictiondf_MartenTor_F_2$Mixed_Forest, center=mean(df_counted_Mammalia$Mixed_Forest), scale=sd(df_counted_Mammalia$Mixed_Forest))
scale_Proportion_Other_seminatural_martenTorF <- scale(Predictiondf_MartenTor_F_2$Other_seminatural, center=mean(df_counted_Mammalia$Other_seminatural), scale=sd(df_counted_Mammalia$Other_seminatural))

scale_Proportion_Waterbodies_martenTorF <- scale(Predictiondf_MartenTor_F_2$Waterbodies, center=mean(df_counted_Mammalia$Waterbodies), scale=sd(df_counted_Mammalia$Waterbodies))


Pseudodata_MartenTorF <- as.data.frame(cbind(scale_Proportion_caro_MartenTorF, Values_for_marten_TorF_try_2, scale_Proportion_Grey_urban_martenTorF, scale_Proportion_Gree_urban_martenTorF,
                                             scale_Proportion_Agrar_martenTorF, scale_Proportion_Broadleafed_Forest_martenTorF, scale_Proportion_Coniferous_Forest_martenTorF,
                                             scale_Proportion_Mixed_Forest_martenTorF, scale_Proportion_Other_seminatural_martenTorF, scale_Proportion_Waterbodies_martenTorF))
setnames(Pseudodata_MartenTorF, old = c("scale_Proportion_caro_MartenTorF", "MartenTorF", "scale_Proportion_Grey_urban_martenTorF", "scale_Proportion_Gree_urban_martenTorF", "scale_Proportion_Agrar_martenTorF", "scale_Proportion_Broadleafed_Forest_martenTorF", "scale_Proportion_Coniferous_Forest_martenTorF", "scale_Proportion_Mixed_Forest_martenTorF", "scale_Proportion_Other_seminatural_martenTorF", "scale_Proportion_Waterbodies_martenTorF"),
         new = c("Proportion_carolinensis_z", "MartenTorF", "Grey_urban_z", "green_urban_z", "Agrar_z",
                 "Broadleafed_Forest_z", "Coniferous_Forest_z", "Mixed_Forest_z","Other_seminatural_z", "Waterbodies_z"))
#setnames(Pseudodata_MartenTorF, old = c("V1", "c(T, T, T, T, T, T, T, T, T, T, F, F, F, F, F, F, F, F, F, F, F)", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10"),
# new = c("Proportion_carolinensis_z", "MartenTorF", "Grey_urban_z", "green_urban_z", "Agrar_z",
# "Broadleafed_Forest_z", "Coniferous_Forest_z", "Mixed_Forest_z","Other_seminatural_z", "Waterbodies_z"))


Pseudodata_MartenTorF <- Pseudodata_MartenTorF%>%
  mutate(year_from_2000 = 12)%>%
  mutate(lat = 33.75)%>%
  mutate(long = 29.25) %>%
  mutate(AllMammalia_log = log(100))



#Predictions Marten T or F
Pseudodata_MartenTorF_new <- Pseudodata_MartenTorF
Pseudodata_MartenTorF_new$MartenTorF2 <- factor(Pseudodata_MartenTorF_new$MartenTorF, labels = c("Absence of pine marten", "Presence of pine marten"))

png(filename= "PredictiondifferentcarolinensismartenTorF.png")


plot_PseudodataTorF_caro <- Pseudodata_MartenTorF_new %>% transform(predictions=predict(fits_vulgaris_martentorf[[1]], newdata=Pseudodata_MartenTorF_new)) %>%
  ggplot(aes(Proportion_carolinensis_z, predictions)) +
  geom_point() +
  scale_x_continuous(breaks = c(-0.4940462 ,0.7418800, 1.9778062, 3.2137323, 4.4496585, 5.6855847), labels = c("0", "20", "40", "60","80", "100"))+
  xlab("Number of S.carolinensis")+
  ylab("Predicted proportion of S.vulgaris")
plot_PseudodataTorF_caro  +  facet_wrap(. ~ MartenTorF2)
  #facet_wrap(~MartenTorF)


dev.off()

Pseudodataforcarolinensis <- Pseudodata_MartenTorF%>%
  dplyr::select(-c(Proportion_carolinensis_z))

Vulgaris_proportion <- setnames(Values_for_caro_try, old = c("c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)"), new = c("Proportion_vulgaris_z"), skip_absent = T)

scale_Proportion_vulgaris_MartenTorF <- scale(Vulgaris_proportion$Proportion_vulgaris_z, center=mean(df_counted_Mammalia$Proportion_vulgaris), scale=sd(df_counted_Mammalia$Proportion_vulgaris))
colnames(scale_Proportion_vulgaris_MartenTorF)[1] <- "Proportion_vulgaris_z"
Pseudodataforpredictionscarolinensis <- cbind( scale_Proportion_vulgaris_MartenTorF,Pseudodataforcarolinensis)

Pseudodataforpredictionscarolinensis_new <- Pseudodataforpredictionscarolinensis
Pseudodataforpredictionscarolinensis_new$MartenTorF2 <- factor(Pseudodataforpredictionscarolinensis$MartenTorF, labels = c("Absence of pine marten", "Presence of pine marten"))
png(filename= "PredictiondifferentvulgarismartenTorF.png")


plot_PseudodataTorF <- Pseudodataforpredictionscarolinensis_new%>% transform(predictions=predict(fits_carolinensis_martentorf[[1]], newdata=Pseudodataforpredictionscarolinensis_new)) %>%
  ggplot(aes(Proportion_vulgaris_z, predictions)) +
  geom_point() +
  #facet_wrap(~MartenTorF)+
  scale_x_continuous(breaks = c(-0.2433489 , 1.0924730 , 2.4282950, 3.7641170 , 5.0999389  ), labels = c("0", "20", "40", "60","80"))+
  xlab("Number of S.vulgaris")+
  ylab("Predicted proportion of S.carolinensis")
plot_PseudodataTorF +  facet_wrap(. ~ MartenTorF2)

dev.off()

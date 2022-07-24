library(spaMM)

mesh <- INLA::inla.mesh.2d(loc = d[, c("lon", "lat")], max.n = 100, max.edge = c(3, 20)) 
fit1.25_new <- fitme(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
                       Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
                       Proportion_marten_z+
                       Proportion_carolinensis_z +
                       Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
                       Proportion_carolinensis_z:Proportion_marten_z +
                       Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
                       Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
                       Proportion_carolinensis_z:Coniferous_Forest_z +
                       MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25)),
                     family = negbin(link = "log"),
                     init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
                     # control.HLfit=list(LevenbergM=TRUE), # maybe
                     verbose = c(TRACE = TRUE), method="PQL/L",
                     data = d)

fit1.25_new_ohnecaro <- fitme(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
                                Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
                                Proportion_marten_z+
                                Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
                                MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25)),
                              family = negbin(link = "log"),
                              init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
                              # control.HLfit=list(LevenbergM=TRUE), # maybe
                              verbose = c(TRACE = TRUE), method="PQL/L",
                              data = d)

diff_glmm_formula_1.25 <- list(
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z  + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))))

fits_vulgaris_1.25 <- lapply((diff_glmm_formula_1.25), function(i){
  fitme(i,family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
        # control.HLfit=list(LevenbergM=TRUE), # maybe
        verbose = c(TRACE = TRUE), method="PQL/L",
        data = d)
})
saveRDS(fits_vulgaris_1.25,"fits_vulgaris_1.25")
fits_vulgaris_1.25 <- readRDS("fits_vulgaris_1.25")

pValues_vulgaris_1.25 <- lapply((2:17), function(i){
  anova(fits_vulgaris_1.25[[1]], fits_vulgaris_1.25[[i]])
})
pValues_vulgaris_1.25_2 <- lapply(pValues_vulgaris_1.25, "[[", "basicLRT")%>%
  do.call(rbind, .)
Vulgaris_table1.25 <- as.data.frame(spaMM:::.make_beta_table(fits_vulgaris_1.25[[1]]))

p_values_for_vulgaris1.25<- pValues_vulgaris_1.25_2 %>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)

Model_table_vulgaris_1.25 <- cbind(Vulgaris_table1.25, p_values_for_vulgaris1.25)
Model_table_vulgaris_1.25_1.1 <- round(Model_table_vulgaris_1.25, digits = 3)
Model_table_vulgaris_1.25_2 <- Model_table_vulgaris_1.25_1.1%>%  
  mutate(translation = format(Model_table_vulgaris_1.25_1.1$p_value, scientific = FALSE, big.mark = ","))

colnames(Model_table_vulgaris_1.25_2) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_vulgaris_1.25_2<- tibble::rownames_to_column(Model_table_vulgaris_1.25_2, "Predictor")
Model_table_vulgaris_1.25_3 <- Model_table_vulgaris_1.25_2%>%
  dplyr::select(-c(p_valuescientific))

saveRDS(Model_table_vulgaris_1.25_3,"Tabellevulgariscitizen.rds")


inits_alle_fits <- lapply((fits_vulgaris_1.25), function(i){
  get_inits_from_fit(i)$init
})



try_fit <- fitme(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
                   Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
                   Proportion_marten_z+
                   Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
                   MaternIMRFa(1|lon+lat, mesh=mesh),
                 family = negbin(link = "log"),
                 init=list(corrPars=list("1"=c(alpha=1.25,kappa=inits_alle_fits[[8]]$corrPars[[1]][["kappa"]])),NB_shape=inits_alle_fits[[8]]$NB_shape, lambda=inits_alle_fits[[8]]$lambda),
                 verbose = c(TRACE = TRUE), method="PQL/L",
                 control.HLfit=list(LevenbergM=TRUE),
                 data = d)

diff_glmm_formula_caro_1.25 <- list(
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + 
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + 
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z +
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + 
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z  +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))))


fits_carolinensis_1.25 <- lapply((diff_glmm_formula_caro_1.25), function(i){
  fitme(i,family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
        # control.HLfit=list(LevenbergM=TRUE), # maybe
        verbose = c(TRACE = TRUE), method="PQL/L",
        data = d)
})
saveRDS(fits_carolinensis_1.25,"fits_carolinensis_1.25")
fits_carolinensis_1.25 <- readRDS("fits_carolinensis_1.25")

pValues_carolinensis_1.25 <- lapply((2:17), function(i){
  anova(fits_carolinensis_1.25[[1]], fits_carolinensis_1.25[[i]])
})
Carolinensis_table_1.25 <- as.data.frame(spaMM:::.make_beta_table(fits_carolinensis_1.25[[1]]))

pValues_carolinensis_1.25_2 <- lapply(pValues_carolinensis_1.25, "[[", "basicLRT")%>%
  do.call(rbind, .)


p_values_for_carolinensis1.25 <-pValues_carolinensis_1.25_2%>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)
Model_table_carolinensis1.25 <- cbind(Carolinensis_table_1.25, p_values_for_carolinensis1.25)
Model_table_carolinensis1.25_1.1 <- round(Model_table_carolinensis1.25, digits = 3)
Model_table_carolinensis1.25_2 <-Model_table_carolinensis1.25_1.1%>% 
  mutate(translation = format(Model_table_carolinensis1.25_1.1$p_value, scientific = FALSE, big.mark = ","))
colnames(Model_table_carolinensis1.25_2) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_carolinensis1.25_2<- tibble::rownames_to_column(Model_table_carolinensis1.25_2, "Predictor")

Model_table_carolinensis1.25_3 <- Model_table_carolinensis1.25_2%>%
  dplyr::select(-c(p_valuescientific))
saveRDS(Model_table_carolinensis1.25_3,"Tabellecarocitizenscience.rds")

#Alle  Pub
d_10kmmixed <- readRDS("Data10kmallePub.rds")
mesh_10km_mixed <- INLA::inla.mesh.2d(loc = d_10kmmixed[, c("lon", "lat")], max.n = 100, max.edge = c(3, 20))


diff_glmm_formula_mixed <- list(
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z  + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))))

fits_vulgaris_mixed <- lapply((diff_glmm_formula_mixed), function(i){
  fitme(i,family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
        # control.HLfit=list(LevenbergM=TRUE), # maybe
        verbose = c(TRACE = TRUE), method="PQL/L",
        data = d_10kmmixed)
})
saveRDS(fits_vulgaris_mixed,"fits_vulgaris_mixed.rds")
fits_vulgaris_mixed <- readRDS("fits_vulgaris_mixed.rds")

pValues_vulgaris_mixed <- lapply((2:17), function(i){
  anova(fits_vulgaris_mixed[[1]], fits_vulgaris_mixed[[i]])
})
p_values_for_vulgaris_mixed <- lapply(pValues_vulgaris_mixed, "[[", "basicLRT")%>%
  do.call(rbind, .)
Vulgaris_table_mixed <- as.data.frame(spaMM:::.make_beta_table(fits_vulgaris_mixed[[1]]))

p_values_for_vulgaris_mixed<- p_values_for_vulgaris_mixed  %>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)

Model_table_vulgaris_mixed <- cbind(Vulgaris_table_mixed , p_values_for_vulgaris_mixed)
Model_table_vulgaris_1.1_mixed <- round(Model_table_vulgaris_mixed, digits = 3)
Model_table_vulgaris_2_mixed <-Model_table_vulgaris_1.1_mixed%>%  
  mutate(translation = format(Model_table_vulgaris_1.1_mixed$p_value, scientific = FALSE, big.mark = ","))

colnames(Model_table_vulgaris_2_mixed) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_vulgaris_2_mixed<- tibble::rownames_to_column(Model_table_vulgaris_2_mixed, "Predictor")
Model_table_vulgaris_3_mixed <- Model_table_vulgaris_2_mixed%>%
  dplyr::select(-c(p_valuescientific))
saveRDS(Model_table_vulgaris_3_mixed,"AllePub1.25Tabelle.rds")



diff_glmm_formula_caro_mixed <- list(
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + 
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + 
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z +
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + 
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z  +
            MaternIMRFa(1|lon+lat, mesh=mesh_10km_mixed, fixed=c(alpha=1.25))))
fits_carolinensis_mixed <- lapply((diff_glmm_formula_caro_mixed), function(i){
  fitme(i,family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
        # control.HLfit=list(LevenbergM=TRUE), # maybe
        verbose = c(TRACE = TRUE), method="PQL/L",
        data = d_10kmmixed)
})
saveRDS(fits_carolinensis_mixed,"fits_carolinensis_mixed.rds")
pValues_carolinensis_mixed <- lapply((2:17), function(i){
  anova(fits_carolinensis_mixed[[1]], fits_carolinensis_mixed[[i]])
})
Carolinensis_table_mixed <- as.data.frame(spaMM:::.make_beta_table(fits_carolinensis_mixed[[1]]))

p_values_for_carolinensis_2_mixed <- lapply(pValues_carolinensis_mixed, "[[", "basicLRT")%>%
  do.call(rbind, .)


p_values_for_carolinensis_mixed <- p_values_for_carolinensis_2_mixed %>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)
Model_table_carolinensis_mixed <- cbind(Carolinensis_table_mixed, p_values_for_carolinensis_mixed)

Model_table_carolinensis_1.1_mixed <- round(Model_table_carolinensis_mixed, digits = 3)
Model_table_carolinensis_2_mixed <-Model_table_carolinensis_1.1_mixed%>% 
  mutate(translation = format(Model_table_carolinensis_1.1_mixed$p_value, scientific = FALSE, big.mark = ","))
colnames(Model_table_carolinensis_2_mixed) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_carolinensis_2_mixed<- tibble::rownames_to_column(Model_table_carolinensis_2_mixed, "Predictor")

Model_table_carolinensis_3_mixed <- Model_table_carolinensis_2_mixed%>%
  dplyr::select(-c(p_valuescientific))

saveRDS(Model_table_carolinensis_3_mixed,"Carolinensisalle1.25.rds")

#cit
diff_glmm_formula_vertebrata <- list(
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z  + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))))

fits_vulgaris_vertebrata <- lapply((diff_glmm_formula_vertebrata), function(i){
  fitme(i,family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
        # control.HLfit=list(LevenbergM=TRUE), # maybe
        verbose = c(TRACE = TRUE), method="PQL/L",
        data = d_moreverte)
})

#saveRDS(fits_vulgaris_vertebrata,"fits_vulgaris_vertebratacit.rds")
saveRDS(fits_vulgaris_vertebrata,"fits_vulgaris_vertebrataallepub.rds")

fits_vulgaris_vertebrata_cit <-readRDS("fits_vulgaris_vertebratacit.rds")

pValues_vulgaris_moreverte_alle <- lapply((2:17), function(i){
  anova(fits_vulgaris_vertebrata[[1]], fits_vulgaris_vertebrata[[i]])
})
p_values_for_vulgaris_vertebrata2 <- lapply(pValues_vulgaris_moreverte_alle, "[[", "basicLRT")%>%
  do.call(rbind, .)
Vulgaris_table_verte <- as.data.frame(spaMM:::.make_beta_table(fits_vulgaris_vertebrata[[1]]))

p_values_for_vulgaris_verte<- p_values_for_vulgaris_vertebrata2 %>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)

Model_table_vulgaris_verte <- cbind(Vulgaris_table_verte, p_values_for_vulgaris_verte)
Model_table_vulgaris_1.1_verte <- round(Model_table_vulgaris_verte, digits = 3)
Model_table_vulgaris_2_verte <-Model_table_vulgaris_1.1_verte%>%  
  mutate(translation = format(Model_table_vulgaris_1.1_verte$p_value, scientific = FALSE, big.mark = ","))

colnames(Model_table_vulgaris_2_verte) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_vulgaris_2_verte<- tibble::rownames_to_column(Model_table_vulgaris_2_verte, "Predictor")
Model_table_vulgaris_3_verte <- Model_table_vulgaris_2_verte%>%
  dplyr::select(-c(p_valuescientific))
saveRDS(Model_table_vulgaris_3_verte,"Vertebratavulgaristabellealle.rds")

#saveRDS(Model_table_vulgaris_3_verte,"Vertebratavulgaristabellecit.rds")


diff_glmm_formula_caro_moreverte <- list(
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + 
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + 
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z +
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + 
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z  +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte, fixed=c(alpha=1.25))))


fits_carolinensis_moreverte <- lapply((diff_glmm_formula_caro_moreverte), function(i){
  fitme(i,family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
        # control.HLfit=list(LevenbergM=TRUE), # maybe
        verbose = c(TRACE = TRUE), method="PQL/L",
        data = d_moreverte)
})
saveRDS(fits_carolinensis_moreverte,"fits_carolinensis_moreverteallepub.rds")
#saveRDS(fits_carolinensis_moreverte,"fits_carolinensis_morevertecit.rds")
fits_carolinensis_moreverteallepub <- readRDS("fits_carolinensis_moreverteallepub.rds")
pValues_caro_moreverte <- lapply((2:17), function(i){
  anova(fits_carolinensis_moreverte[[1]], fits_carolinensis_moreverte[[i]])
})

Carolinensis_table_moreverte <- as.data.frame(spaMM:::.make_beta_table(fits_carolinensis_moreverte[[1]]))

p_values_for_carolinensis_2_moreverte <- lapply(pValues_caro_moreverte, "[[", "basicLRT")%>%
  do.call(rbind, .)


p_values_for_carolinensis_moreverte <- p_values_for_carolinensis_2_moreverte %>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)
Model_table_carolinensis_moreverte <- cbind(Carolinensis_table_moreverte, p_values_for_carolinensis_moreverte)

Model_table_carolinensis_1.1_moreverte <- round(Model_table_carolinensis_moreverte, digits = 3)
Model_table_carolinensis_2_moreverte <-Model_table_carolinensis_1.1_moreverte%>% 
  mutate(translation = format(Model_table_carolinensis_1.1_moreverte$p_value, scientific = FALSE, big.mark = ","))

colnames(Model_table_carolinensis_2_moreverte) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_carolinensis_2_moreverte<- tibble::rownames_to_column(Model_table_carolinensis_2_moreverte, "Predictor")

Model_table_carolinensis_3_moreverte <- Model_table_carolinensis_2_moreverte%>%
  dplyr::select(-c(p_valuescientific))
saveRDS(Model_table_carolinensis_3_moreverte,"Carolinensismorevertetabellealle.rds")
#saveRDS(Model_table_carolinensis_3_moreverte,"Carolinensismorevertetabellecit.rds")


#alle pub more verte
diff_glmm_formula_vertebrata_all <- list(
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z  + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))))

fits_vulgaris_vertebrata <- lapply((diff_glmm_formula_vertebrata), function(i){
  fitme(i,family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
        # control.HLfit=list(LevenbergM=TRUE), # maybe
        verbose = c(TRACE = TRUE), method="PQL/L",
        data = d_moreverte)
})

#saveRDS(fits_vulgaris_vertebrata,"fits_vulgaris_vertebratacit.rds")
saveRDS(fits_vulgaris_vertebrata,"fits_vulgaris_vertebrataallepub.rds")

fits_vulgaris_vertebrata_cit <-readRDS("fits_vulgaris_vertebratacit.rds")

pValues_vulgaris_moreverte_alle <- lapply((2:17), function(i){
  anova(fits_vulgaris_vertebrata[[1]], fits_vulgaris_vertebrata[[i]])
})
p_values_for_vulgaris_vertebrata2 <- lapply(pValues_vulgaris_moreverte_alle, "[[", "basicLRT")%>%
  do.call(rbind, .)
Vulgaris_table_verte <- as.data.frame(spaMM:::.make_beta_table(fits_vulgaris_vertebrata[[1]]))

p_values_for_vulgaris_verte<- p_values_for_vulgaris_vertebrata2 %>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)

Model_table_vulgaris_verte <- cbind(Vulgaris_table_verte, p_values_for_vulgaris_verte)
Model_table_vulgaris_1.1_verte <- round(Model_table_vulgaris_verte, digits = 3)
Model_table_vulgaris_2_verte <-Model_table_vulgaris_1.1_verte%>%  
  mutate(translation = format(Model_table_vulgaris_1.1_verte$p_value, scientific = FALSE, big.mark = ","))

colnames(Model_table_vulgaris_2_verte) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_vulgaris_2_verte<- tibble::rownames_to_column(Model_table_vulgaris_2_verte, "Predictor")
Model_table_vulgaris_3_verte <- Model_table_vulgaris_2_verte%>%
  dplyr::select(-c(p_valuescientific))
saveRDS(Model_table_vulgaris_3_verte,"Vertebratavulgaristabellealle.rds")

#saveRDS(Model_table_vulgaris_3_verte,"Vertebratavulgaristabellecit.rds")


diff_glmm_formula_caro_moreverte_all <- list(
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + 
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + 
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z +
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + 
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z  +
            MaternIMRFa(1|lon+lat, mesh=mesh_moreverte_mixed, fixed=c(alpha=1.25))))


fits_carolinensis_moreverte_all <- lapply((diff_glmm_formula_caro_moreverte_all), function(i){
  fitme(i,family = negbin(link = "log"),
        init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
        # control.HLfit=list(LevenbergM=TRUE), # maybe
        verbose = c(TRACE = TRUE), method="PQL/L",
        data = d_moreverte)
})
saveRDS(fits_carolinensis_moreverte,"fits_carolinensis_moreverteallepub.rds")


pValues_caro_moreverte_all <- lapply((2:17), function(i){
  anova(fits_carolinensis_moreverte_all[[1]], fits_carolinensis_moreverte_all[[i]])
})

Carolinensis_table_moreverte_all <- as.data.frame(spaMM:::.make_beta_table(fits_carolinensis_moreverte_all[[1]]))

p_values_for_carolinensis_2_moreverte_all <- lapply(pValues_caro_moreverte_all, "[[", "basicLRT")%>%
  do.call(rbind, .)


p_values_for_carolinensis_moreverte_all <- p_values_for_carolinensis_2_moreverte_all %>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)
Model_table_carolinensis_moreverte_all <- cbind(Carolinensis_table_moreverte_all, p_values_for_carolinensis_moreverte_all)

Model_table_carolinensis_1.1_moreverte_all <- round(Model_table_carolinensis_moreverte_all, digits = 3)
Model_table_carolinensis_2_moreverte <-Model_table_carolinensis_1.1_moreverte_all%>% 
  mutate(translation = format(Model_table_carolinensis_1.1_moreverte_all$p_value, scientific = FALSE, big.mark = ","))

colnames(Model_table_carolinensis_2_moreverte) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_carolinensis_2_moreverte<- tibble::rownames_to_column(Model_table_carolinensis_2_moreverte, "Predictor")

Model_table_carolinensis_3_moreverte <- Model_table_carolinensis_2_moreverte%>%
  dplyr::select(-c(p_valuescientific))
saveRDS(Model_table_carolinensis_3_moreverte,"Carolinensismorevertetabellealle.rds")



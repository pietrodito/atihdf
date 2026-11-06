groupe_volume_valorisation_cas_cmd_hc_hp <- function(df_FIXE) {
  (
    df_FIXE
    %>% nettoie_séjours()
    %>% caractérise_hospitalisation()
    %>% caractérise_séjour()
    %>% caractérise_GHS()
    %>% caractérise_région_ES()
    %>% caractérise_ES()
    %>% transforme_âge_en_classe()
    %>% dplyr::mutate(cas = case_when(
      cas == "S"                                     ~ "Séances"    ,
      cas == "O15"                                   ~ "Nouveau né" ,
      cas == "O14"                                   ~ "Obstétrique",
      cas == "C"                                     ~ "Chirurgie"  ,
      cas == "PI"                                    ~ "Peu invasif",
      str_detect(type_GHS, "GHS") & annee >= 2020    ~ "Gradation"  ,
      .default                                       = "Médecine"   ))
    %>% dplyr::mutate(cmd = str_sub(ghm2, end = 2L))
    %>% atihdf::préfixe_les_colonnes(
      sexe, cl_age, TOP_PEDIATRIE,
      cas, cmd, type_hospitalisation, région_ES,
      TOP_DIALYSE, type_ES)
    %>%  dplyr::select(ident, finess, ghs, mois, duree, nbseance,
                       dplyr::starts_with("groupage_"))
  )
}

## (FR_séjours_journées_cas_cmd_hc_hp_2016_2025 <-
##     compte_les_séjours_séances_et_journées(
##       groupe_volume_valorisation_cas_cmd_hc_hp,
##       2016, 2025))
##
## ((
##   FR_séjours_journées_cas_cmd_hc_hp_2016_2025
##   %>% dplyr::filter(région_ES == "HDF")
## ) -> HDF_séjours_journées_cas_cmd_hc_hp_2016_2025)
##
##
## ((
##   FR_séjours_journées_cas_cmd_hc_hp_2016_2025
##   %>% dplyr::group_by(année, région_ES, type_ES, TOP_DIALYSE,
##                       type_hospitalisation, cas, cmd,
##                       sexe, TOP_PEDIATRIE, cl_age)
##   %>% dplyr::summarise(dplyr::across(c(séjours, journées, séances), sum))
## ) -> Par_régions_séjours_journées_cas_cmd_hc_hp_2016_2025)
##
## readr::write_rds(HDF_séjours_journées_cas_cmd_hc_hp_2016_2025,
##                  paste0(
##                    "tests/Données_valorisation/",
##                    "HDF_séjours_journées_cas_cmd_hc_hp_2016_2025.rds"))
##
## readr::write_rds(Par_régions_séjours_journées_cas_cmd_hc_hp_2016_2025,
##                  paste0(
##                    "tests/Données_valorisation/",
##                    "Par_régions_séjours_journées_cas_cmd_hc_hp_2016_2025.rds"))




valo_base(groupe_volume_valorisation_cas_cmd_hc_hp, 2025, 2025)

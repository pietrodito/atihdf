#'@export
exclusion_des_séjours_trop_longs <- function(FIXE_df) {

  ((
    FIXE_df
    |> caractérise_hospitalisation()
    |> dplyr::filter(type_hospitalisation != "Séances")
  ) -> FIXE_common_setup)

  ((
    FIXE_common_setup
    |> dplyr::mutate(LN_DS = dplyr::case_when(
      duree == 0 ~ 0,
      .default = ln(duree)))
    |> dplyr::select(ghm2, duree, LN_DS)
    |> dplyr::group_by(ghm2)
    |> dplyr::summarise(N = n(),
                 median_LN_DS = median(LN_DS),
                 mean_LN_DS = mean(LN_DS),
                 sd_LN_DS = sd(LN_DS),
                 p5_LN_DS = quantile(LN_DS, .05),
                 p95_LN_DS = quantile(LN_DS, .95))
    |> dplyr::ungroup()
    |> dplyr::mutate(BINF = mean_LN_DS - sd_LN_DS + p5_LN_DS  - median_LN_DS)
    |> dplyr::mutate(BSUP = mean_LN_DS + sd_LN_DS + p95_LN_DS - median_LN_DS)
    |> dplyr::mutate(BINF = exp(BINF))
    |> dplyr::mutate(BSUP = exp(BSUP))
    |> tidyr::replace_na(list(BINF = 0, BSUP = 1000))
    |> dplyr::select(ghm2, BINF, BSUP)
  ) -> limits)

  ## --
  (
    FIXE_common_setup
    |> dplyr::left_join(limits)
    |> dplyr::filter(duree <= BSUP)
    |> dplyr::select(-BINF, -BSUP)
  )
}


#'@export
jointure_dms_théoriques <- function(FIXE_df, année = NULL) {
  if(is.null(année)) {
    année <- FIXE_df |> head(1) |> dplyr::pull(annee)
  }
 (
   FIXE_df
   |> dplyr::inner_join(get(glue::glue("dms_{année}")), copy = TRUE)
 )
}

#'@export
ip_dms_setup <- function(FIXE_df,
                         colonnes = "aso"
                         ) {
  (
    FIXE_df
    |> dplyr::group_by(dplyr::across(dplyr::all_of(colonnes)))
  )
}

#'@export
ip_dms <- function(FIXE_df) {
  (
    FIXE_df
    |> dplyr::summarise(théorique = sum(dms),
                 effective = sum(duree))
    |> dplyr::mutate(ip_dms = effective / théorique)
  )
}

#'@export
dms_par_aso <- function(finess, année) {
  le_finess <- finess
  (
    ct(MCO, FIXE, année = year, verbose = FALSE)
    |> dplyr::select(finess, annee, ghm2, ghs, duree, mois, typ_sej)
    |> dplyr::filter(finess == le_finess)
    |> nettoyage_séjours()
    |> caractérise_ES(regroupe_les_ex_DGF = T)
    |> caractérise_séjour()
    |> exclusion_des_séjours_trop_longs()
    |> jointure_dms_théoriques()
    |> ip_dms_setup()
    |> ip_dms()
    |> dplyr::collect()
  )
}

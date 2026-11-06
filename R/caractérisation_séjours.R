#' Caractérise le type du GHS d'un séjour
#'
#' @description
#'
#' Ajoute une colonne type_GHS pouvant valoir :
#'  - Mono-UHCD
#'  - GHS intermédiaire
#'  - GHS pein
#'  - Normal
#'
#'  La table en entrée issue de `FIXE` doit posséder les colonnes `ident` et
#'  `ghs`
#'
#' @export
caractérise_GHS <- function(FIXE_df) {

  année_table_fixe <- attr(FIXE_df, "année")
  ct(MCO,  UM,  année = année_table_fixe, verbose = FALSE)

  uhcd <- c("07A", "07B")

  ((
    UM
    |> dplyr::filter(type_rum_1 %in% uhcd)
    |> dplyr::select(ident)
    |> dplyr::distinct()
  ) -> ident_uhcd)

  ((
    UM
    |> dplyr::filter(! type_rum_1 %in% uhcd)
    |> dplyr::select(ident)
    |> dplyr::distinct()
  ) -> ident_non_uhcd)

  ((
    ident_uhcd
    |> dplyr::anti_join(ident_non_uhcd)
    |> dplyr::mutate(type_GHS = "Mono-UHCD")
  ) -> ident_mono_UHCD)

  (
    FIXE_df
    |> dplyr::left_join(ident_mono_UHCD)
    |> dplyr::mutate(type_GHS = dplyr::case_when(
      type_GHS == "Mono-UHCD" ~ "Mono-UHCD",
      ghs %in% ghs_intermediaires_codes ~ "GHS intermédiaire",
      ghs %in% ghs_pleins_codes & duree == 0 ~ "GHS plein",
      .default = "Normal"))
  )
}


#' Détermine le type d'hospitalisation d'un séjour
#'
#' @description
#' Cette fonction ajoute  une colonne `type_hospitalisation` qui peut
#' prendre les valeurs : `HC`, `HDJ` ou `Séances`.
#'
#' Elle nécessite en entrée une table issue de `FIXE` contenant au minimum
#' les colonnes `ghm2` et `durée`.
#'
#' NB : Ne fonctione que pour le champ MCO
#'
#' @param FIXE_df Un data.frame issu de la table FIXE, contenant
#'   notamment les colonnes `ghm2` et `durée`.
#'
#' @param champ ne fonctionne que pour `mco` (valeur par défaut)
#'
#' @return Un data.frame identique à `FIXE_df` avec une colonne
#'   supplémentaire `type_hospitalisation` (`factor` ordonné).
#'
#'@export
caractérise_hospitalisation <- function(FIXE_df, champ = "mco") {
  (
    FIXE_df
    |> dplyr::mutate(
      type_hospitalisation =
        dplyr::case_when(
          stringr::str_sub(ghm2, 1, 2) == "28" ~ "Séances",
          duree == 0 ~ "HDJ",
          duree > 0  ~ "HC",
          .default = NA))
  )
}






mco_rgp_pour_join <- function(année_en_cours) {

  (
    paste0("v", (année_en_cours - 1):année_en_cours)
  ) -> versions_concernées


  ct(NOMPMSI, mco_rgp_ghm, verbose = FALSE)

  (
    mco_rgp_ghm
    %>% dplyr::rename(v2015 = v11g)
    %>% dplyr::select(ghm, aso, cas, gp, da, dplyr::all_of(versions_concernées))
    %>% tidyr::pivot_longer(all_of(versions_concernées),
                            names_to = "annee",
                            values_to = "Existe")
    %>% dplyr::filter(Existe == "1")
    %>% dplyr::select(-Existe)
    %>% dplyr::mutate(annee = stringr::str_sub(annee, start = 2L))
    %>% dplyr::rename(ghm2 = ghm)
  )

}


mco_rgp_lib_pour_join <- function(année) {


  (
    paste0("v", (année - 1):année)
  ) -> versions_concernées

  ct(NOMPMSI, mco_rgp_lib, verbose = FALSE)

  (
    mco_rgp_lib
    |> dplyr::rename(v2015 = v11g,
                     v2014 = v11f)
    |> dplyr::select(code, type_code, libelle, all_of(versions_concernées))
    %>% dplyr::mutate(libelle = dplyr::case_when(
      libelle == "Cathétérismes vasculaires diagnostiques et interventionnels" ~ "Cathétérismes vasculaires",
      libelle == "Chimiothérapie, radiothérapie, hors séances" ~ "Chimiothérapie, radiothérapie",
      libelle == "Cardio-vasculaire (hors cathétérismes vasculaires diagnostiques et interventionnels)" ~ "Cardio-vasculaire",
      libelle == "Système nerveux (hors cathétérismes vasculaires diagnostiques et interventionnels)" ~ "Système nerveux",
      libelle == "Activités inter spécialités, suivi thérapeutique d'affections connues" ~ "Activités inter spécialités",
      libelle == "Traumatismes multiples ou complexes graves" ~ "Traumatismes multiples",
      # libelle == "" ~ "",
      .default = libelle
    ))
    %>% tidyr::pivot_longer(all_of(versions_concernées),
                            names_to = "annee",
                            values_to = "Existe")
    %>% dplyr::filter(Existe == "1")
    %>% dplyr::select(-Existe)
    %>% dplyr::mutate(annee = stringr::str_sub(annee, start = 2L))
  )
}



#' Caractérise le type d'un séjour MCO
#'
#' @description
#' Ajoute les colonnes `aso`, `cas`, `da` et `gp`
#'
#' La table en entrée issue de `FIXE` doit posséder la colonne `ghm2`
#'@export
caractérise_séjour <- function(FIXE_df) {
  année <- attr(FIXE_df, "année")

  (
    FIXE_df
    |> dplyr::mutate(annee = ifelse(mois < 3, annee - 1L, annee))
    |> dplyr::left_join(mco_rgp_pour_join(année))
    |> dplyr::mutate(annee = ifelse(mois < 3, annee + 1L, annee))
  )
}



#'@export
type_séjour_ajoute_lib <- function(FIXE_df, lib) {
  lib_col_name <- paste0("lib_", lib)
  (
    FIXE_df
    |> dplyr::mutate(annee = ifelse(mois < 3, annee - 1L, annee))
    |> dplyr::left_join(
      mco_rgp_lib_pour_join(attr(FIXE_df, "année"))
      |> dplyr::filter(type_code == lib)
      |> dplyr::rename({{lib}} := code)
      |> dplyr::rename({{lib_col_name}} := libelle)
      |> dplyr::select(dplyr::all_of(c(lib, "annee", lib_col_name)))
    )
    |> dplyr::mutate(annee = ifelse(mois < 3, annee + 1L, annee))
  )
}


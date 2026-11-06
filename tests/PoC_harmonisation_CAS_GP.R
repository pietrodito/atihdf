teradata()

library(tidyverse)


## Liste des GHM qui existent entre :
# Année en cours
# Version la + ancienne concernée

mco_rgp_pour_join <- function(année_en_cours) {
  année_de_la_plus_ancienne_version_concernée <-
    paste(année_en_cours - 11)

  versions_concernées <-
    paste0("v",
           année_de_la_plus_ancienne_version_concernée:année_en_cours)

  sum_expr <- purrr::reduce(versions_concernées[-1],
                            ~rlang::expr(!!.x + !!sym(.y)),
                            .init = rlang::sym(versions_concernées[1]))


  ct(NOMPMSI, mco_rgp_ghm)
  (
    mco_rgp_ghm
    |> dplyr::rename(v2014 = v11f, v2015 =  v11g)
    |> dplyr::mutate(existe = !!sum_expr)
    |> dplyr::filter(existe > 0)
    |> dplyr::select(ghm)
    |> dplyr::distinct()
    |> dplyr::inner_join(mco_rgp_ghm)
    |> dplyr::rename(v2014 = v11f, v2015 =  v11g)
    |> dplyr::group_by(ghm)
    |> dplyr::mutate(dplyr::across(
      dplyr::all_of(versions_concernées),
      ~ dplyr::case_when(
        sum(.) > 0 ~ 1,
        .default = 0
      )))
    |> dplyr::select(ghm, dplyr::all_of(versions_concernées))
    |> tidyr::pivot_longer(versions_concernées,
                           names_to =  "année",
                           values_to = "existe")
    |> dplyr::mutate(année =
                       stringr::str_sub(année, start = 2L) |> as.integer())
    |> dplyr::filter(existe == 1)
    |> dplyr::group_by(ghm, existe)
    |> dplyr::summarise(année = max(année))
    |> dplyr::ungroup()
    |> dplyr::mutate(année = paste0("w", année |> as.character()))
    |> tidyr::pivot_wider(names_from = année, values_from = existe)
    |> dplyr::mutate(top_keep = "1")
  ) -> temp

  nom_colonnes <- colnames(temp)
  acc <- mco_rgp_ghm |> dplyr::rename(v2014 = v11f, v2015 =  v11g)
  for(step in versions_concernées) {

    w_name <- stringr::str_replace(step, pattern = "v", replacement = "w")

    join_vector <- (
      c("ghm", w_name)
      |> rlang::set_names( "ghm", step)
    )

    if(w_name %in% nom_colonnes) {
      acc <- dplyr::left_join(
        acc,
        temp |> dplyr::select(dplyr::all_of(c("ghm", w_name, "top_keep"))),
        by = join_vector)
    }
  }
  acc_colnames <- colnames(acc)
  tops <- acc_colnames[stringr::str_detect(acc_colnames, "^top_keep")]

  sum_expr <- purrr::reduce(tops[-1],
                            ~rlang::expr(!!.x + !!sym(.y)),
                            .init = rlang::sym(tops[1]))

  (
    acc
    |> dplyr::mutate(dplyr::across(dplyr::everything(),
                                   ~dplyr::coalesce(., 0)))
    |> dplyr::mutate(keep = !!sum_expr)
    |> dplyr::filter(keep > 0)
    |> dplyr::select(ghm, aso, cas, gp)
  )
}

(
  ghms_existants(2025)
  |> count(ghm)
  |> arrange(n |> desc())
)

(
  # ghms_existants(2025)
  mco_rgp_ghm
  # |> count(ghm)
  # |> arrange(desc(n))
  |> filter(ghm == "26M024")
)

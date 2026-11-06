#'@export
préfixe_les_colonnes <- function(df, ..., .prefixe = "groupage_") {

  stopifnot(is.character(.prefixe), length(.prefixe) == 1L)

  if (rlang::dots_n(...) == 0L) return(df)

  idx <- tidyselect::eval_select(rlang::expr(c(...)), data = df)
  nms <- names(idx)
  if (length(nms) == 0L) return(df)

  dplyr::rename_with(
    df,
    .fn = function(x) ifelse(startsWith(x, .prefixe), x, paste0(.prefixe, x)),
    .cols = tidyselect::all_of(nms)
  )
}


#'@export
compte_les_séjours_séances_et_journées <-
  function(fn_groupes_volume_valo,
           première_année,
           dernière_année,
           mois_de_sortie_dernière_année = 12) {

    years <- première_année:dernière_année

    nb_sej <- function(year) {

      ct(MCO, FIXE, année = year)

        (
          FIXE
          |> fn_groupes_volume_valo()
          %>% {
            if(year == dernière_année) {
              dplyr::filter(., mois <= mois_de_sortie_dernière_année)
            } else {
              .
            }
          }
          |> dplyr::group_by(dplyr::across(
            c(finess, dplyr::starts_with("groupage_"))))
          %>% dplyr::summarise(séjours = n(),
                               séances = sum(nbseance),
                               journées = sum(duree))
          %>% dplyr::ungroup()
          %>% dplyr::mutate(séances = if_else(groupage_cas != "Séances",
                                              0, séances))
          %>% dplyr::mutate(séjours = if_else(groupage_cas == "Séances",
                                              0, séjours))
          %>% dplyr::mutate(journées = if_else(groupage_cas == "Séances",
                                              0, journées))
          %>% {
            if(year == dernière_année) {
              (
                dplyr::mutate(.,
                              dplyr::across(
                                c(séjours, séances, journées),
                                ~ . * 12 / !! mois_de_sortie_dernière_année)))
            } else {
              .
            }
          }
          |> dplyr::mutate(année = year)
          %>% dplyr::collect()
        ) -> out

      total_séjours <- sum(out$séjours)
      total_séances <- sum(out$séances)

      cli::cli_alert_success(
        glue::glue(
          "Année {year} : ",
          "{total_séjours |> scales::number_format(accuracy = 1)()} séjours",
          " et ",
          "{total_séances |> scales::number_format(accuracy = 1)()} séances.",
          ))

      out
    }

    ((
      years
      |> purrr::map(nb_sej)
      %>% dplyr::bind_rows()
      %>% dplyr::rename_with(~ stringr::str_replace(., "groupage_", ""))
    ))
  }

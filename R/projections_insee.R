#'@export
projections_insee <- function(df, année_observée, année_projetée) {

  ## df doit contenir les colonnes département, cl_age et sexe
  ## ainsi que des colonnes préfixées observation_
  ## qui seront extrapolées en projection_
  ## en fonction des prédictions de l'évolution de la pyramide des âges

  col_pop_année_observée <- paste0("POP_", année_observée)
  col_pop_année_projetée <- paste0("POP_", année_projetée)


  ((
    df
    %>% dplyr::collect()
    %>% dplyr::inner_join(projections_insee_dpt_hdf)
    %>% dplyr::select(
      1:(length(colnames(df))),
      dplyr::all_of(c(col_pop_année_observée, col_pop_année_projetée)))
  ) -> prêt_pour_projection)

  (
    prêt_pour_projection
    |> dplyr::mutate(dplyr::across(
      dplyr::starts_with("observation_"),
      ~ (1.0 * .x /  .data[[col_pop_année_observée]]) *
        .data[[col_pop_année_projetée]],
      .names = "projection_{.col}")
    )
    |> dplyr::rename_with(
      ~ stringr::str_replace(.x, "^projection_observation_", "projection_"),
      dplyr::starts_with("projection_observation_") )
    %>% dplyr::group_by(lib_département)
    %>% dplyr::summarise(dplyr::across(
      c(dplyr::starts_with("observation_"), dplyr::starts_with("projection_")),
      sum))
    %>% dplyr::rename_with(
      ~ stringr::str_replace(.x, "observation_",
                             paste0("observation_", année_observée, "_")),
                             dplyr::starts_with("observation_"))
    %>% dplyr::rename_with(
      ~ stringr::str_replace(.x, "projection_",
                             paste0("projection_", année_projetée, "_")),
                             dplyr::starts_with("projection_"))
  )
}

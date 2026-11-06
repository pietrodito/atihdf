#' Filtre les lignes des HDF
#'
#' @description
#' Le filtre est basé sur une colone dont les deux premiers caractères doivent
#' correspondre au code du département.
#'
#' Par défaut, la colonne utilisée est `finess`.
#'
#' @export
filtre_HDF <- function(df, col = finess) {
  (
    df
    |> dplyr::filter(
      stringr::str_sub({{col}}, 1, 2) %in% codes_départements_hdf)
  )
}


#' @export
conso_prod_HDF <- function(df, col_conso = codegeo, col_ES = finess) {
  (
    df
    |> dplyr::filter(
      stringr::str_sub({{col_conso}}, 1, 2) %in% codes_départements_hdf
      | # OU LOGIQUE
      stringr::str_sub({{col_ES}}   , 1, 2) %in% codes_départements_hdf
    )
  )
}

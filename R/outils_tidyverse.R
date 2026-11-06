#' @export
remplace_tous_les_NA <- function(df_avec_NA) {
  df_avec_NA |>
    dplyr::mutate(
      # Pour les colonnes numériques : remplace NA, NaN, Inf et -Inf par 0
      dplyr::across(
        dplyr::where(is.numeric),
        ~ replace(., !is.finite(.), 0)
      ),
      # Pour les colonnes textuelles : remplace NA par ""
      dplyr::across(
        dplyr::where(is.character),
        ~ tidyr::replace_na(., "")
      )
    )
}

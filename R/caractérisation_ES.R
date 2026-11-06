#'@export
filtre_ES_métropole <- function(df, col = finess) {
  (
    df
    |> dplyr::filter(stringr::str_sub({{col}}, 1, 2) <= "95")
  )
}

helper_dpt <- function() {

  ct(NOMGEN, FINESS, année = NULL, verbose = FALSE)
  (
    FINESS
    |> dplyr::filter(! is.na(code_com))
    |> dplyr::mutate(
      code_département =
        dplyr::case_when(
          substr(code_com, 1, 2) == "97" ~ substr(code_com, 1, 3),
          .default                       = substr(code_com, 1, 2)))
    |> dplyr::select(finess, code_département)
    |> dplyr::distinct()
    |> dplyr::left_join(
      régions_départements
      |> dplyr::select(code_département, code_région, lib_département),
      copy = TRUE
    )
    |> dplyr::rename(région_ES = code_région,
                     département_ES = code_département,
                     lib_département_ES = lib_département)
  )
}

#' Donne la région et le département des ES
#'
#' @description
#' Basée sur la colonne `finess`
#'
#' La fonction ajoute les colonnes `région_ES` (ARA, BFC...), la colonne
#' `département_ES` (01, 02, ...) et la colonne `lib_département_ES`.
#'
#' @param col colonne qui doit contenir, à l'instar du finess,  le code
#' département dans ses 2 premiers caractères.
#'
#'@export
caractérise_région_ES <- function(df) {
  (
    df
    |> dplyr::left_join(helper_dpt())
  )
}


helper_ES_type <- function(champ, rs = FALSE) {

  if(champ == "smr") champ <- "ssr"
  colSym <- rlang::sym(champ)

  ct(NOMGEN, FINESS, verbose = FALSE)
  (
    FINESS
    |> dplyr::filter(!! colSym == 1)

    |> dplyr::mutate(type_ES = dplyr::case_when(
      categ == "CH" ~ "CH",
      categ == "CHR/U" ~ "CHU",
      categ == "Privé" ~ "Privé",
      .default = "EBNL"))
    |> dplyr::mutate(`TOP_DIALYSE` =
                       dplyr::if_else(! is.na(dialyse), "1", "0"))
  ) -> temp
  cols_to_select <- c("finess", "annee", "type_ES", "TOP_DIALYSE",
                      if(rs) "rs" else character(0))
  dplyr::select(temp, dplyr::all_of(cols_to_select))
}

#' Caractérise le type des ES
#'
#' @description
#'
#' Ajoute la colonne `type_ES` qui peut prendre les valeurs :
#' - `CHU`, `CH`, `EBNL`, `Privé`
#'
#' Ajoute une colonne TOP_DIALYSE : 1 <=> ES ne faisant que de la dialyse.
#'
#' L'option `regroupe_les_ex_DGF` est `FALSE` par défaut.
#'
#' L'option `rs` est `FALSE` par défaut
#'
#' L'option `champ` est à "mco" par défaut
#'
#'@export
caractérise_ES <- function(df_FIXE, regroupe_les_ex_DGF = FALSE,
                           rs = FALSE, champ = "mco") {
  ((
    df_FIXE
    |> dplyr::left_join(helper_ES_type(champ, rs), by = c("finess", "annee"))
  ) -> out)

  if(regroupe_les_ex_DGF) {
    ((
      out
      |> dplyr::mutate(type_ES = dplyr::case_when(
        type_ES == "Privé" ~ "ex-OQN",
        .default = "ex-DGF"))
    ) -> out)
  }
  out
}



age_en_classe <- function()  {
  age <- 0:140
  q <- age %/% 5
  q <- ifelse(q > 19, 19, q)
  left_i <- paste0("[", q * 5, "-")
  right_i <- paste0((q + 1) * 5, "[")
  right_i <- ifelse(right_i == "100[" , "99+[", right_i)
  classe_age <- paste0(left_i, right_i)
  (classe_lettre_insee <- sort(c(rep(LETTERS[1:19], 5), rep("T", 46))))
  dplyr::tibble(age, classe_age, cl_age = classe_lettre_insee)
}

#' Transorme l'âge en classe
#'
#' @description
#' La colonne `age` prend la forme `[0-5[`, `[5-10[`, etc...
#' Une seconde colonne est créée `cl_age` avec lettres de A à T correspondant
#' à la table INSEE de `NOMGEN`
#'
#'@export
transforme_âge_en_classe <- function(df_FIXE, col = "age")  {
  (
    df_FIXE
    %>% dplyr::mutate(TOP_PEDIATRIE = if_else(age < 18, "1", "0"))
    |> dplyr::rename(age_temp = {{col}})
    |> dplyr::left_join(age_en_classe(), by = c("age_temp" = "age"), copy = T)
    |> dplyr::select(-age_temp)
    |> dplyr::rename({{col}} := classe_age)
  )
}



#' @export
caractérise_région_patient <- function(df_FIXE, col = codegeo) {
  ((
    df_FIXE
    %>% dplyr::inner_join(
      territoires_patient(attr(df_FIXE, "année"))
      %>% dplyr::select(codegeo, code_zone_région)
      %>% dplyr::inner_join(régions_départements
                            %>% dplyr::distinct(
                              code_région,
                              code_zone_région = code_chiffré_région),
                            copy = TRUE))
  ))
}

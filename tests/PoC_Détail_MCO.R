ct(MCO, FIXE, 2025)

((
  FIXE
  |> nettoie_séjours()
  |> transforme_âge_en_classe()
  |> caractérise_hospitalisation()
  |> caractérise_GHS()
  |> caractérise_séjour()
  |> caractérise_région_ES()
  |> caractérise_ES()
  |> dplyr::select(
    ident, finess,
    type_ES, TOP_DIALYSE,
    ghm2, ghs, type_GHS,
    mois, annee,
    age, cl_age,
    type_hospitalisation,
    aso, cas, gp,
    région_ES, département_ES, lib_département_ES,
  )
  |> type_séjour_ajoute_lib("gp")
  |> table_temporaire(FIXE_DÉTAILLÉE)
))

## Vérfie pas de doublon séjour dans FIXE_DÉTAILLÉE
(nb_lignes <- FIXE_DÉTAILLÉE %>% dplyr::count() %>% dplyr::pull())
(nb_séjours <- FIXE_DÉTAILLÉE %>% dplyr::distinct(ident) %>% dplyr::count()
  %>% dplyr::pull())
stopifnot( nb_lignes == nb_séjours )

((
  FIXE_DÉTAILLÉE |> dplyr::count(type_GHS)
  %>% dplyr::collect()
) -> types_ghs)

stopifnot(sum(types_ghs$n) == nb_lignes)


library(atihdf)

teradata()

ct(SMR, FIXE, 2024)

((
  FIXE
  |> transforme_age_en_classe()
  |> caractérise_région_ES()
  |> dplyr::mutate(annee = 2024L)
  |> caractérise_ES(champ = "smr")
) -> df)


library(tidyverse)


library(atihdf)
library(tidyverse)

((
  ct(NOMGEN, pop_com_sex_ag)
  |> filter(annee == 2019)
))

((
  ct(NOMGEN, pop_com_sex_ag)
  |> filter(annee %in% 2015:2024)
  |> mutate(dpt = substr(code_com, 1, 2))
  |> filter(dpt != "97")
  |> group_by(annee, age, sexe)
  |> summarise(pop = sum(pop))
  %>% ungroup()
  |> collect()
  |> mutate(Territoire = "France")
) -> insee_métro)

((
  ct(NOMGEN, pop_com_sex_ag)
  |> filter(annee %in% 2015:2024)
  |> mutate(dpt = substr(code_com, 1, 2))
  |> filter(dpt %in% codes_départements_hdf )
  |> group_by(annee, age, sexe)
  |> summarise(pop = sum(pop))
  %>% ungroup()
  |> collect()
  |> mutate(Territoire = "HDF")
) -> insee_hdf)


usethis::use_data(insee_métro, overwrite = TRUE)
usethis::use_data(insee_hdf, overwrite = TRUE)


## TODO en friche
# - les tables pop_com_sex_ag et celles dwnld de l'INSEE et le site INSEE sont
# légèrement discordants
# - revoir quelle est la meilleure approche lorsque l'on voudra standardiser

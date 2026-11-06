library(tidyverse)

assure_ghs_sur_4_char <- function(string) { str_pad(string, 4, "left", "0") }

((
  "data_building_scripts/json/ghm_intermediaire.json"
  |> jsonlite::fromJSON()
  |> as_tibble()
  |> select(ghm_intermediaire, ghs_plein, ghs_intermediaire)
  |> distinct()
  |> mutate(across(ghs_plein:ghs_intermediaire, assure_ghs_sur_4_char))
) -> ghs_intermediaires)

ghs_intermediaires_codes <- ghs_intermediaires$ghs_intermediaire
ghs_pleins_codes <- ghs_intermediaires$ghs_plein

usethis::use_data(ghs_intermediaires,       overwrite = TRUE)
usethis::use_data(ghs_pleins_codes, overwrite = TRUE)
usethis::use_data(ghs_intermediaires_codes, overwrite = TRUE)

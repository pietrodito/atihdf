library(tidyverse)

## TODO pivoter soit insee_region soit projection pour coller 2015, 2018, 2019, etcc

(rs_etb <- readr::read_csv2("~/commun/_IMT2A/NOMENCLATURES/LISTE DES ETB.csv"))

((
  rs_etb
  |> select(finess, Statut, RS = `RS Court corrigé`)
) -> rs_courts)

usethis::use_data(rs_courts, overwrite = TRUE)


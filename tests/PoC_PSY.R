library(tidyverse)
devtools::load_all()
teradata()

(
  ct(PSY, FIXE, 2024)
  |> caractérise_ES()
  |> filter(is.na(type_ES))
  |> select(finess, annee, type_ES)
)


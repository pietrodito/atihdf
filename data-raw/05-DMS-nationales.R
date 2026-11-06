##   .
##  /!\  Il faut déjà avoir un package compilé pour exécuter ce script
##  '''

## Calcul des dms nationales pour une année donnée par secteur
library(atihdf)
library(tidyverse)
dms_nationales <- function(année) {
  ct(MCO, FIXE, année = année)

  ln <- log

  ((
    FIXE
    |> nettoie_séjours()
    |> caractérise_hospitalisation()
    |> filter(type_hospitalisation != "Séances")
    |> caractérise_ES()
    |> mutate(type_ES = case_when(
      type_ES == "Privé" ~ "ex-OQN",
      .default = "ex-DGF"))
  ) -> FIXE_common_setup)
  ## --
  ((
    FIXE_common_setup
    |> mutate(LN_DS = case_when(
      duree == 0 ~ 0,
      .default = ln(duree)))
    |> select(type_ES, ghm2, duree, LN_DS)
    |> group_by(type_ES, ghm2)
    |> summarise(N = n(),
                 median_LN_DS = median(LN_DS),
                 mean_LN_DS = mean(LN_DS),
                 sd_LN_DS = sd(LN_DS),
                 p5_LN_DS = quantile(LN_DS, .05),
                 p95_LN_DS = quantile(LN_DS, .95))
    |> ungroup()
    |> mutate(BINF = mean_LN_DS - sd_LN_DS + p5_LN_DS  - median_LN_DS)
    |> mutate(BSUP = mean_LN_DS + sd_LN_DS + p95_LN_DS - median_LN_DS)
    |> mutate(BINF = exp(BINF))
    |> mutate(BSUP = exp(BSUP))
    |> replace_na(list(BINF = 0, BSUP = 1000))
    |> select(type_ES, ghm2, BINF, BSUP)
    |> arrange(type_ES, ghm2)
  ) -> limits)

  ## --
  ((
    FIXE_common_setup
    |> select(type_ES, ghm2, duree)
    |> left_join(limits)
    |> filter(duree <= BSUP)
    |> group_by(type_ES, ghm2)
    |> summarise(dms = mean(duree) |> round(digits = 3))
    |> collect()
    |> arrange(type_ES, ghm2)
    |> ungroup()
  ) -> out)

  var_name <- glue::glue("dms_{année}")
  assign(var_name, out)
  do.call(usethis::use_data,
          list(as.name(var_name), overwrite = TRUE))
}

purrr::walk(2016:2025, dms_nationales)

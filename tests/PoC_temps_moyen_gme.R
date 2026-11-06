library(atihdf)
library(tidyverse)

ct(SMR, FIXE, 2025)
ct(NOMGEN, FINESS_CODEGEO_DISTANCE_TPS)
ct(NOMPMSI, ALL_CLASSIF_PMSI)

((
  FINESS_CODEGEO_DISTANCE_TPS
  %>% select(finessgeo = finess, codegeo, tps_trajet)
  %>% mutate(tps_trajet = sql("
        EXTRACT(HOUR FROM tps_trajet) * 3600 +
        EXTRACT(MINUTE FROM tps_trajet) * 60 +
        EXTRACT(SECOND FROM tps_trajet)"))
) -> TRAJETS_CODEGEO_FINESSGEO)

((
  FIXE
  %>% filter(age < 18)
  %>% select(gme, finessgeo, codegeo)
  %>% inner_join(TRAJETS_CODEGEO_FINESSGEO)
  %>% group_by(gme)
  %>% summarise(tps_trajet = mean(tps_trajet))
  %>% inner_join(
    (
      ALL_CLASSIF_PMSI
      %>% filter(type_code == "gme")
      %>% filter(version == "v2025")
      %>% select(gme = code, lib)
    ))
) -> gme_tps_trajet)

(
  FIXE
  %>% filter(age < 18)
  %>% mutate(dpt_patient = str_sub(codegeo, end = 2L))
  %>% filter(dpt_patient == "59")
  %>% select(gme, codegeo, finessgeo)
  %>% inner_join(TRAJETS_CODEGEO_FINESSGEO)
  %>% inner_join(gme_tps_trajet %>% select(gme, tps_moyen = tps_trajet))
  %>% group_by(gme, tps_moyen)
  %>% summarise(across(tps_trajet, mean))
  %>% collect()
  %>% write_csv("gme_tmp.csv")
)


## Terrioires ZAS, DPT et FRANCE METRO / FRANCE ENTIERE

library(tidyverse)

actes_chir_bariatriques <- c(
  "HFMC007", "HFMA009", "HFKC001", "HFKA002", "HFMC008", "HFMA011", "HFCC003",
  "HFCA001", "HFFC018", "HFFA011", "HFMC006", "HFMA010", "HGCC027", "HGCA009",
  "HFFC004", "HFFA001")

année <- 2024

ct(MCO, ACTE, FIXE, année = année)

((
  ACTE
  %>% filter(str_sub(acte, end =7L) %in% actes_chir_bariatriques)
  %>% distinct(ident)
) -> IDENT_CHIR_BARIATRIQUES)


((
  FIXE
  %>% inner_join(IDENT_CHIR_BARIATRIQUES)
  %>% select(codegeo, sexe, age)
  %>% transforme_âge_en_classe()
  %>% group_by(codegeo, sexe, cl_age)
  %>% summarise(recours_chir_bariatrique = n())
  %>% ungroup()
) -> NB_CHIR_BARIATRIQUES_PAR_CODEGEO)


((
  population_toutes_zones(année)
  %>% left_join(NB_CHIR_BARIATRIQUES_PAR_CODEGEO)
  %>% standardisation_focus_HDF()
  %>% collect()
  %>% arrange(zone_var, zone)
) -> x) %>% pinf()



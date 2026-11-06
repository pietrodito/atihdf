library(tidyverse)

((
  tibble(age = 0:19 * 5)
  %>% transforme_age_en_classe()
  %>% mutate(age = age %>% str_replace("-", ";"))
  %>% mutate(age = age %>% str_replace("\\+\\[", "\\+\\]"))
  %>% collect()
  %>% rename(TRAGE = age)
) -> map_TRAG_to_letter)

((
  "data-raw/xlsx/donnees_det_Central.xlsx"
  %>% readxl::read_excel(sheet = "Population", skip = 5)
  %>% filter(ZONE %in% départements_hdf$lib_département)
  %>% inner_join(map_TRAG_to_letter)
  %>% select(-TRAGE)
  %>% rename(sexe = SEXE)
  %>% mutate(sexe = as.character(sexe))
  %>% rename(lib_département = ZONE)
  %>% relocate(cl_age, .after = sexe)
) -> projections_insee_dpt_hdf)

usethis::use_data(projections_insee_dpt_hdf, overwrite = TRUE)


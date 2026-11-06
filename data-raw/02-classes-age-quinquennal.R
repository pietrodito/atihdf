library(tidyverse)

((
  tibble(classe = 1:20)
  %>% mutate(borne_basse = (classe - 1) * 5)
  %>% mutate(borne_haute = (classe) * 5 - 1)
  %>% mutate(borne_haute = ifelse(borne_haute == 99, 200, borne_haute))
  %>% mutate(intervalle = case_when(
    borne_haute == 200 ~ "[95;99+[",
    .default = glue::glue("[{borne_basse};{borne_haute}[")
  ))
  %>% mutate(cl_age = LETTERS[1:20])
) -> classes_age_quinquennal)

usethis::use_data(classes_age_quinquennal, overwrite = TRUE)

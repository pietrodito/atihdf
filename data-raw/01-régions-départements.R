library(tidyverse)
library(readr)

((
  "data-raw/csv/correspondance_regions_departements.csv"
  |> read_csv()
  |> rename(code_région      = 1,
            lib_région       = 2,
            code_département = 3,
            lib_département  = 4)
  |> mutate(across(starts_with("code"), ~ str_remove(., "FR-")))
  |> bind_rows(tibble(
    code_région = c("SPE", "STM", "STB"),
    lib_région  = c("Saint-Pierre-et-Miquelon", "Saint-Martin", "Saint-Barthélémy"),
    code_département = c("975", "978", "977"),
    lib_département = c("Saint-Pierre-et-Miquelon", "Saint-Martin", "Saint-Barthélémy")))
  %>% inner_join(
    tribble(
    ~code_chiffré_région,  ~ code_région,
                "44"    ,          "GES",
                "76"    ,          "OCC",
                "32"    ,          "HDF",
                "84"    ,          "ARA",
                "02"    ,          "MTQ",
                "04"    ,          "REU",
                "52"    ,          "PDL",
                "24"    ,          "CVL",
                "27"    ,          "BFC",
                "03"    ,          "GUF",
                "75"    ,          "NAQ",
                "94"    ,          "COR",
                "28"    ,          "NOR",
                "53"    ,          "BRE",
                "06"    ,          "MYT",
                "93"    ,          "PAC",
                "01"    ,          "GLP",
                "11"    ,          "IDF"
    )
  )
) -> régions_départements)

((
  régions_départements
  |> filter(code_région == "HDF")
  |> select(code_département, lib_département)
) -> départements_hdf)

(codes_départements_hdf <- départements_hdf$code_département)

((
  régions_départements
  |> filter(code_département <= "95")
  |> pull(code_région)
  |> unique()
) -> régions_métropolitaines)

usethis::use_data(départements_hdf       , overwrite = TRUE)
usethis::use_data(codes_départements_hdf , overwrite = TRUE)
usethis::use_data(régions_départements   , overwrite = TRUE)
usethis::use_data(régions_métropolitaines, overwrite = TRUE)


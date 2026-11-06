plus_ancienne_année_PMSI <- 2015

local_create_connection <- function() {
  suppressMessages(library(tidyverse))
  suppressMessages(library(glue))
  suppressMessages(teradata())
  finess_code_dpt <<- finess_code_dpt()
  ((
    finess_code_dpt
    |> filter(is.na(code_département))
    |> distinct()
    |> pull(finess)
  ) ->> ES_sans_dpt)
}

local_create_connection()


# loop over 10 tests: Aucun finess sans dpt dans une table FIXE
{
  walk(plus_ancienne_année_PMSI:(plus_ancienne_année_PMSI + 9),
       function(année) {
         ((
           ct(MCO, FIXE, année)
           |> filter(finess %in% ES_sans_dpt)
           |> collect()
         ) -> df)
         test_that(glue(
           "Aucun finess sans département n'apparait dans la table FIXE {année}"), {
             expect_equal(nrow(df), 0) }) }) }

test_that("aucun ES n'a plusieurs codes départements", {
  ((
    finess_code_dpt
    |> filter(! is.na(code_département))
    |> select(finess, code_département)
    |> distinct()
    |> count(finess)
    |> filter(n > 1)
    |> pull(finess)
  ) -> ES_multiple_code_dpt)
  expect_equal(ES_multiple_code_dpt, character(0)) })

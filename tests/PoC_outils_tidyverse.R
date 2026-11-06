
(
  tibble::tibble(a = c("qwer", NA, "asdf"),
                 b = c(1, 2, NA),
                 c = c(NA, 1L, 2L)
                 )
  |> remplace_tous_les_NA()
)


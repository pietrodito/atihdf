#'@export
pinf <- function(...) { print(..., n = Inf) |> invisible() }

#'@export
cp <- function(...) { dplyr::collect(...) |> pinf() }

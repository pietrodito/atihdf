#' @export
clean_linux_processes <- function() {
  (
    system('ps -ux --sort start_time | grep rstudio-server | cut -c10-16 ',
           intern = T)
    |> head(-3)
    |> purrr::walk(~ system(glue::glue("kill -9 {.}")))
  )
}


#' @export
list_linux_processes <- function() {
    system('ps -ux --sort start_time | grep rstudio-server', intern = T)
}

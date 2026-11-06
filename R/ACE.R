#' Activité externe des ES DGF
#'
#' `ACE` renvoie le total des remboursements Assurance Maladie pour tous les
#' établissements français d'une année donnée en précisant leur région et leur
#' type CHU, CH, EBNL.
#'
#' @export
#' @examples
#' ACE(2019)
ACE <- function(année, détail = FALSE) {
  table <- raccourci_schéma("MCO", année)
  dgf <- c("CHU", "CH", "EBNL")
  ((
    table(FIXE)
    |> dplyr::select(finess, annee)
    |> dplyr::distinct()
    |> caractérise_ES_région()
    |> caractérise_ES()
    |> dplyr::filter(type_ES %in% dgf)
    |> dplyr::select(finess, région_ES, type_ES)
  ) -> ES_dgf_par_région)
  suffixes_détail <- c("atu", "ccam", "dia", "dm_ext", "ffm", "ftn", "ngap",
                        "se")
  fu      <- if (année >= 2022) "fu"      else character(0)
  inno    <- if (année >= 2021) "inno"    else character(0)
  med_ext <- if (année >= 2017) "med_ext" else character(0)
  if(détail) {
    variables_détail <- c("mnt_remb",
                          paste0("mnt_remb_",
                                 c(suffixes_détail, fu, inno, med_ext)))
  } else {
    variables_détail <- "mnt_remb"
  }
  (
    table(VALO_EXTERNE_DGF)
    |> dplyr::inner_join(table(ID_EXTERNE))
    |> dplyr::inner_join(ES_dgf_par_région, copy = T)
    |> dplyr::select(finess, ident_ext, mnt_remb,
                     région_ES, type_ES, variables_détail)
    |> dplyr::group_by(région_ES, type_ES, finess)
    |> dplyr::summarise(across(all_of(variables_détail), sum),
                        SEJ = count(ident_ext))
    |> dplyr::ungroup()
    |> dplyr::mutate(type = "ACE")
    |> dplyr::mutate(Année = année)
    |> dplyr::collect()
  )
}

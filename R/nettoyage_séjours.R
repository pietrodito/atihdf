#' Nettoie les séjours MCO de la table FIXE
#'
#' @description
#' Supprime :
#'  - les doublons liés aux finess géographiques AP‑HP, AP‑HM et HCL, avant 2017
#'  - les `ghm2` en `90%`
#'  - éventuellement les GHS en 9999
#'  - éventuellement les PIE A ou B
#'
#'
#'@export
nettoie_séjours <- function(FIXE_df,
                              garde_les_ghs_9999 = TRUE,
                              PIE_à_garder = c("A", "B")) {

  finess_geo_grands_CHU_exclus <- c(
    '130780521', '130783236', '130783293', '130784234',
    '130804297', '600100101', '750041543', '750100018',
    '750100042', '750100075', '750100083', '750100091',
    '750100109', '750100125', '750100166', '750100208',
    '750100216', '750100232', '750100273', '750100299',
    '750801441', '750803447', '750803454', '910100015',
    '910100023', '920100013', '920100021', '920100039',
    '920100047', '920100054', '920100062', '930100011',
    '930100037', '930100045', '940100027', '940100035',
    '940100043', '940100050', '940100068', '950100016',
    '690783154', '690784137', '690784152', '690784178',
    '690787478', '830100558')

  ((
    FIXE_df
    |> dplyr::filter(
      ! finess %in% finess_geo_grands_CHU_exclus,
      ! ghm2 %LIKE% '90%',
      is.na(typ_sej) | typ_sej %in% PIE_à_garder)
  ) -> tmp)

  if(garde_les_ghs_9999) {
    tmp
  } else {
    dplyr::filter(tmp, ghs != '9999')
  }
}

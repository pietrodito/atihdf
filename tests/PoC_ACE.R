library(tidyverse)
library(atihdf)

#  (
#    atihdf::ACE(2024)
#    |> filtre_ES_HDF()
#  )


(
  ct(MCO, VALO_EXTERNE_DGF, 2024)
)


(
  ct(NOMPMSI, ALL_CCAM_RGP_TARIF)
  |> dplyr::count(code_rgp)
  |> dplyr::collect()
  |> dplyr::arrange(code_rgp)
)  |> pinf()

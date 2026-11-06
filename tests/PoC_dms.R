library(scales)
library(ggrepel)
devtools::load_all()
teradata()

years <- 2015:2024

## TODO faire les boucles pour tous les ES et toutes les années
ip_dms_pour_ES_HDF <- function(année) {
  ES <- ct(MCO, FIXE, année) |>
    select(finess) |>
    distinct() |>
    pull()
}

## + boucle sur ex-DGF / ex-OQN sur toutes les années (plus simple)
ip_dms_HDF_par_aso <- function(année) {
  (
    ct(MCO, FIXE, année)
    |> filtre_ES_HDF()
    |> caractérise_ES(regroupe_les_ex_DGF = T)
    |> type_séjour()
  ) -> pre

  (
    pre
    |> dplyr::filter(ES_type == "ex-DGF")
    |> exclusion_des_séjours_trop_longs()
  ) -> ex_DGF

  (
    pre
    |> dplyr::filter(ES_type == "ex-OQN")
    |> exclusion_des_séjours_trop_longs()
  ) -> ex_OQN

  (
    dplyr::union_all(ex_DGF, ex_OQN)
    |> jointure_dms_théoriques()
    |> ip_dms_setup(c("ES_type", "aso"))
    |> ip_dms()
    |> dplyr::collect()
    |> dplyr::mutate(annee = année)
    |> dplyr::arrange(ES_type, aso)
  )
}


((
  2015:2024 # years
  |> purrr::map(ip_dms_HDF_par_aso)
) -> ip_dms_hdf_each_year)

(do.call(bind_rows, ip_dms_hdf_each_year) -> ip_dms_hdf_all_years)

(
  ip_dms_hdf_all_years
  |> ungroup()
  |> mutate(ip_label = scales::label_number(accuracy = .001)(ip_dms))
  |> ggplot(aes(x = annee, y = ip_dms, color = aso, label = ip_label))
  +  geom_line()
  +  geom_hline(yintercept = 1, linetype = "dashed")
  +  facet_grid(. ~ ES_type)
  +  geom_label(show.legend = F)
)

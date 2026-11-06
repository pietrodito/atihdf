teradata()

CH_DENAIN <- "590782165"
COL <- "590000188"

ES <- CH_DENAIN
ES <- COL


prépare_volume_valo_aso_hc_hp <- function(df_FIXE) {
  (
    df_FIXE
    |> nettoie_séjours()
    |> caractérise_hospitalisation()
    |> caractérise_séjour()
    |> dplyr::filter(type_hospitalisation != "Séances", aso != "Z")
    |> dplyr::mutate(categ_sej = paste0(aso, "_", type_hospitalisation))
    |> dplyr::select(ident, finess, ghs, mois, categ_sej)
  ) -> out
}

prépare_volume_valo_gp_hc_hp <- function(df_FIXE) {
  (
    df_FIXE
    |> nettoie_séjours()
    |> caractérise_hospitalisation()
    |> caractérise_séjour()
    |> dplyr::filter(type_hospitalisation != "Séances", aso != "Z")
    |> dplyr::mutate(categ_sej = paste0(
      stringr::str_sub(gp,             end = 1L), "_",
      stringr::str_sub(gp, start = 2L, end = 3L), "_",
      type_hosp))
    |> dplyr::select(ident, finess, ghs, mois, categ_sej)
  ) -> out
}


prépare_volume_valo_cmd_cas <- function(df_FIXE) {
  (
    df_FIXE
    |> nettoyage_séjours()
    |> type_hospitalisation()
    |> type_séjour()
    |> atihdf::caractérise_GHS()
    |> dplyr::filter(cas != "S")
    |> dplyr::mutate(cas = case_when(
     cas == "O15"                                   ~ "Nouveau né" ,
     cas == "O14"                                   ~ "Obstétrique",
     cas == "C"                                     ~ "Chirurgie"  ,
     cas == "PI"                                    ~ "Peu invasif",
     str_detect(GHS_type, "GHS") & annee >= 2020    ~ "Gradation"  ,
     .default                                       = "Médecine"   ))
    |> dplyr::mutate(cmd = str_sub(ghm2, end = 2L))
    |> dplyr::mutate(categ_sej = paste0(cmd, "_", cas, "_", type_hosp))
    |> dplyr::select(ident, finess, ghs, mois, categ_sej)
  ) -> out
}

prépare_volume_valo_aso_seances_CT_RT <- function(df_FIXE) {
  (
    df_FIXE
    |> nettoyage_séjours()
    |> type_hospitalisation()
    |> type_séjour()
    |> atihdf::caractérise_GHS()
    |> dplyr::mutate(aso = case_when(
     substr(gp, 1, 3) == "S04"                      ~ "Radiothérapie" ,
     substr(gp, 1, 1) == "S"                        ~ "Chimiothérapie",
     .default                                       = aso   ))
    |> dplyr::mutate(categ_sej = paste0(aso, "_",  type_hosp))
    |> dplyr::select(ident, finess, ghs, mois, categ_sej)
  ) -> out
}

first_year <- 2016
last_year <- 2025


(séjours_par_an <- compte_les_séjours(prépare_volume_valo_aso_seances_CT_RT,
                                      finess_ES = ES,
                                      première_année = first_year,
                                      dernière_année = last_year,
                                      mois_de_sortie_dernière_année = 6))


(valos_par_an <- calcule_les_valos(prépare_volume_valo_aso_seances_CT_RT,
                                   finess_ES = ES,
                                   première_année = first_year,
                                   dernière_année = last_year,
                                   mois_de_sortie_dernière_année = 6))

readr::write_rds(séjours_par_an, "./draft/COL_séjours.rds")
readr::write_rds(valos_par_an, "./draft/COL_valos.rds")

(séjours_par_an <- readr::read_rds("./draft/COL_séjours.rds"))
(valos_par_an <- readr::read_rds("./draft/COL_valos.rds"))


library(tidyverse)


graph_mco <- function(data, première_année, dernière_année, année_base = 2019,
                      nom_ES = "COL", type) {


  (years <- première_année:dernière_année)

  if(type == "valo") {
    title <- "{nom_ES} : Valorisation neutralisée de l'effet prix et des mesures Ségur "
  } else {
    title <- "Activité en séjours sur 10 ans"
  }
  title <- NULL

  if(type == "valo") {
    ylab <-  "Valoristaion en base {année_base}"
  } else {
    ylab <-  "Nbr. séjours en base {année_base}"
  }

  ((
    data
    %>%  do.call(dplyr::bind_rows, .)
    # |> dplyr::filter(type_hosp != "Séances")
    # |> dplyr::filter(aso != "Z")
  ) -> data_par_an)

  ((
    data_par_an
    # |> dplyr::ungroup()
    |> dplyr::rename(#ASO = aso, Hospitalisation = type_hosp,
      Année = année, Valorisation = value)
  ) -> df_long)

  ((
    df_long
    |> dplyr::filter(Année == année_base)
    |> dplyr::summarise(total = sum(Valorisation))
    |> dplyr::pull(total)
  ) -> total_base)



  ((
    df_long
    |> dplyr::group_by(Année)
    |> dplyr::mutate(Valorisation_norm = Valorisation / total_base * 100)
    |> dplyr::ungroup()
    |> dplyr::filter(categ_sej != "O_HC")
    |> dplyr::mutate(
      categ_sej = factor(
        categ_sej,
        levels = c(
          "C_HDJ",
          "M_HDJ",
          "Chimiothérapie_Séances",
          "C_HC",
          "M_HC",
          "Radiothérapie_Séances"
        ),
        labels = c(
          "Chir ambu",
          "Méd ambu",
          "Chimioth.",
          "Chir HC",
          "Méd  HC",
          "Radioth."
        )
      ))
  ) -> df_long_2)

  ((
    df_long_2
    |> dplyr::group_by(Année)
    |> dplyr::summarise(Total = sum(Valorisation))
  ) -> totaux_par_année)

  ((
    df_long_2
    |> dplyr::left_join(totaux_par_année)
    |> dplyr::mutate(Part = Valorisation / Total)
    |> dplyr::mutate(label_Part = scales::label_percent(accuracy = .1)(Part))
    |> dplyr::arrange(Année, desc(categ_sej))
    |> group_by(Année)
    |> dplyr::mutate(cumsum_valo = cumsum(Valorisation))
    |> dplyr::mutate(cumsum_valo_lag = dplyr::lag(cumsum_valo))
    |> dplyr::mutate(dplyr::across(dplyr::any_of(c("cumsum_valo_lag")),
                                   ~ tidyr::replace_na(., 0)))
    |> dplyr::mutate(y_label_part_raw = cumsum_valo_lag +
                       (cumsum_valo - cumsum_valo_lag) / 2)
    |> dplyr::mutate(y_label_part = y_label_part_raw / total_base * 100)
  ) -> evo_part_valo)


  ((
    df_long_2
    |> dplyr::group_by(Année)
    |> dplyr::summarise(Total = sum(Valorisation_norm))
  ) -> totaux)

  ((
    df_long_2
    |> dplyr::group_by(Année, categ_sej)
    |> dplyr::summarise(Valorisation = sum(Valorisation))
    |> dplyr::group_by(categ_sej)
    |> dplyr::mutate(valo_last_year = dplyr::lag(Valorisation))
    |> dplyr::ungroup()
    |> dplyr::mutate(Evo = (Valorisation / valo_last_year - 1) |>
                       scales::percent_format(accuracy = 1,
                                              style_positive = "plus",
                                              suffix = "%")())
    |> dplyr::arrange(Année, categ_sej |> desc())
    |> dplyr::group_by(Année)
    |> dplyr::mutate(Valo_cumul = cumsum(Valorisation))
    # |> dplyr::mutate(evo_y = dplyr::case_when(
    #   ASO == "M" ~ Valorisation / total_base / 2 * 100,
    #   ASO == "C" ~ (Valo_cumul - Valorisation / 2) / total_base * 100))
    # |> dplyr::filter(ASO != "O")
    # |> dplyr::mutate(Evo = ifelse(is.na(Evo), dplyr::case_when(
    #   ASO == "C" ~ "Chir.",
    #   ASO == "M" ~ "Med."),
    #   Evo
    # ))
    |> dplyr::arrange(categ_sej, Année)
    |> dplyr::group_by(categ_sej)
    # |> dplyr::mutate(arrow_y_begin = evo_y - 4,
    #                  arrow_y_end = dplyr::lead(evo_y) - 4)
    # |> dplyr::mutate(arrow_x_begin = Année + .25,
    #                  arrow_x_end = dplyr::lead(Année) - .25)
  ) -> evo_chir_med)

  ((
    df_long_2
    |> ggplot2::ggplot(ggplot2::aes(x = Année, y = Valorisation_norm,
                                    fill = categ_sej))
    + ggplot2::geom_line(data = totaux, ggplot2::aes(x = Année, y = Total),
                         linewidth = 3,
                         alpha = 0.5, inherit.aes = FALSE)
    + ggplot2::geom_bar(stat = "identity", position = "stack", colour = "black")
    + ggplot2::geom_point(data = totaux, ggplot2::aes(x = Année, y = Total),
                          size = 3,
                          alpha = 0.5,
                          inherit.aes = FALSE)
    + ggplot2::geom_label(data = totaux, ggplot2::aes(x = Année, y = Total+7,
                                                      label = paste0(round(Total, 1), "%")),
                          size = 5,
                          inherit.aes = FALSE)
    # + ggplot2::geom_label(data = evo_chir_med,
    #                       ggplot2::aes(x = Année, y = evo_y, label = Evo,
    #                                    fill = ASO),
    #                       inherit.aes = FALSE, show.legend = F)
    # + ggplot2::geom_curve(
    #   data = (evo_chir_med
    #           |> dplyr::filter(! is.na(arrow_x_end) & ! is.na(arrow_y_end))),
    #   ggplot2::aes(x = arrow_x_begin, y = arrow_y_begin,
    #                xend = arrow_x_end, yend = arrow_y_end),
    #   arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
    #   alpha = 0.3,
    #   inherit.aes = FALSE, show.legend = F)
    + ggrepel::geom_label_repel(data = evo_part_valo, ggplot2::aes(x = Année,
                                                                   y = y_label_part,
                                                                   label = label_Part),
                                show.legend = F,
                                max.overlaps = Inf,
                                force = 0.001)
    + ggplot2::theme_minimal()
    + ggplot2::scale_x_continuous(breaks = years)
    + ggplot2::xlab("")
    + ggplot2::ylab(glue::glue(ylab))
    + ggplot2:: ggtitle(glue::glue(title))
    + ggplot2::theme(axis.text.x = ggtext::element_markdown(size = 15))
    + ggplot2::ylim(0, 123)
    + labs(fill = NULL)
    + theme(legend.text = element_text(size = 14))
    + theme(axis.title.y = element_text(size = 14))
  ))
}



graph_mco(valos_par_an, 2016, 2025, 2019, type = "valo")
(plot_valo <- graph_mco(valos_par_an, 2016, 2025, 2019, type = "valo"))
(plot_sej <- graph_mco(séjours_par_an, 2016, 2025, 2019, type = "séj"))

(
  ggpubr::ggarrange(plot_valo, plot_sej, common.legend = T, legend = "left")
  |> ggpubr::annotate_figure(
    fig.lab = "Évolution de la valorisation et du nombre de séjours et séances",
    fig.lab.size = 22,
    bottom = "Les pourcentages de couleur représentent la part de chaque activité.\nL'année 2025 est extrapolée à partir du premier semestre."
  )
)


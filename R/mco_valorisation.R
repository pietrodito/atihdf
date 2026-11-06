
#' Coefficients pour exprimer tarifs prestations en année de base
#'
#' @export
prestations_coef_pour_base <- function(année_base) {

  (dernière_colonne <- paste0("v", année_base))
  (années <- (année_base - 10):année_base)
  (colonnes <- paste0("v", années))

  ct(NOMPMSI, MCO_TARIF_HGHS)
  ((
    années
    |>  purrr::map(function(année) {
      (
        MCO_TARIF_HGHS
        |> dplyr::filter(! code %in% c("TSE", "TDE"))
        |> dplyr::filter(is.na(code_comp))
        |> dplyr::mutate(mi_année = paste0(année, "-06-30"))
        |> dplyr::filter(mi_année > date_deb, mi_année < date_fin)
        |> dplyr::select(-mi_année)
        |> dplyr::mutate(version = paste0("v", as.character(année)))
        |> dplyr::select(secteur_pmsi, type_presta, code, tarif, version)
      )
    })
    |> purrr::reduce(dplyr::union_all)
  ) -> temp)
  ((
    temp
    |> dplyr::filter(version == dernière_colonne)
    |> dplyr::select(secteur_pmsi, type_presta, code, tarif_dernier = tarif)
  ) -> tarifs_derniers)
  (
    temp
    |> dplyr::left_join(tarifs_derniers)

    |> dplyr::mutate(coef = tarif_dernier / tarif)
    |> dplyr::group_by(version)
    |> dplyr::mutate(coef_moyen_version = mean(coef))
    |> dplyr::ungroup()
    |> dplyr::mutate(coef = ifelse(is.na(tarif_dernier),
                                   coef_moyen_version,
                                   coef))


    |> dplyr::select(secteur_pmsi, type_presta, code, version, coef)
  )
}




#' Coefficients pour exprimer tarifs GHS en année de base
#'
#' @export
ghs_coef_pour_base <- function(année_base, secteur = "DGF") {


  (dernière_colonne <- paste0("v", année_base))

  (colonnes <- paste0("v", (année_base - 10):année_base))

  ct(NOMPMSI, MCO_GHS)

  ((
    MCO_GHS
    |> dplyr::filter(secteur_pmsi == secteur)
    |> dplyr::select(secteur_pmsi, ghs, version, tarif)
    |> dplyr::mutate(version = dplyr::case_when(
      version == "v11f" ~ "v2014",
      version == "v11g" ~ "v2015",
      .default = version))
    |> dplyr::filter(version %in% colonnes)
  ) -> temp)
  ((
    temp
    |> dplyr::filter(version == dernière_colonne)
    |> dplyr::select(secteur_pmsi, ghs, tarif_dernier = tarif)
  ) -> tarifs_derniers)
  ((
    temp
    |> dplyr::left_join(tarifs_derniers)
    |> dplyr::mutate(coef = tarif_dernier / tarif)
    |> dplyr::group_by(version)
    |> dplyr::mutate(coef_moyen_version = mean(coef))
    |> dplyr::ungroup()
    |> dplyr::mutate(coef = ifelse(is.na(tarif_dernier),
                                   coef_moyen_version,
                                   coef))
    |> dplyr::select(code_valo = ghs, version, coef)
  ) -> temp_ghs)

  (
    prestations_coef_pour_base(année_base)
    |> dplyr::filter(type_presta %in% c("dialyse", "innovation"))
    |> dplyr::select(code_valo = code, version, coef)
    |> dplyr::union_all(temp_ghs)
  )
}




#' Coefficients pour calcul des suppléments en année de base
#'
#' @export
suppléments_coef_pour_base <- function(année_base, secteur = "DGF") {

## TODO il n'a y a pas v2025 dans le retour de cette fonction

## construire table version coef pour tous les suppléments
  (dernière_colonne <- paste0("v", année_base))
  (années <- (année_base - 10):année_base)
  (versions <- paste0("v", années))


  ## 9614 = caisson

  (supp_ghs_codes <- c(9610, 9614, 9615, 9619, 9620, 9621, 9622,
                       9623, 9625, 9631, 9632, 9633, 9639) |> as.character())

  ((
    ghs_coef_pour_base(année_base)
    |> dplyr::filter(version %in% versions)
    |> dplyr::filter(code_valo %in% supp_ghs_codes)
    |> tidyr::pivot_wider(names_from = code_valo, values_from = coef)
    |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), 1, .)))
  ) -> supp_ghs)


  ((
    prestations_coef_pour_base(année_base)
    |> dplyr::filter(type_presta %in% c("supplements", "po", "dialyse"))
    |> dplyr::select(code, version, coef)
    |> tidyr::pivot_wider(names_from = code, values_from = coef)
    |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), 1, .)))
    |> dplyr::inner_join(supp_ghs)
  ) -> avant_jointure_valo_dgf)

}



#' Valorisation totale exprimée en une autre année
#'
#' @export
valo_base <- function(fonction_groupage_volume_valorisation,
                      année_activité,
                      année_base,
                      mois_de_sortie_année_activité = 12) {

  (tibble::tribble(
    ~ version,  ~ type_ES,    ~ segur,    ~ charge,
      "v2014",    "CH"   ,          1,           1,
      "v2014",    "CHU"  ,          1,           1,
      "v2014",    "EBNL",           1,           1,
      "v2015",    "CH"   ,          1,           1,
      "v2015",    "CHU"  ,          1,           1,
      "v2015",    "EBNL",           1,           1,
      "v2016",    "CH"   ,          1,           1,
      "v2016",    "CHU"  ,          1,           1,
      "v2016",    "EBNL",           1,           1,
      "v2017",    "CH"   ,          1,           1,
      "v2017",    "CHU"  ,          1,           1,
      "v2017",    "EBNL",           1,           1,
      "v2018",    "CH"   ,          1,           1,
      "v2018",    "CHU"  ,          1,           1,
      "v2018",    "EBNL",           1,      0.9852,
      "v2019",    "CH"   ,          1,           1,
      "v2019",    "CHU"  ,          1,           1,
      "v2019",    "EBNL",           1,      0.9823,
      "v2020",    "CH"   ,          1,           1,
      "v2020",    "CHU"  ,          1,           1,
      "v2020",    "EBNL",           1,      0.9827,
      "v2021",    "CH"   ,     1.0019,           1,
      "v2021",    "CHU"  ,     1.0019,           1,
      "v2021",    "EBNL",      0.9862,      0.9840,
      "v2022",    "CH"   ,     1.0013,           1,
      "v2022",    "CHU"  ,     1.0013,           1,
      "v2022",    "EBNL",      0.9841,      0.9931,
      "v2023",    "CH"   ,     1.0023,           1,
      "v2023",    "CHU"  ,     1.0023,           1,
      "v2023",    "EBNL",      0.9853,      0.9897,
      "v2024",    "CH"   ,     1.0042,           1,
      "v2024",    "CHU"  ,     1.0042,           1,
      "v2024",    "EBNL",      0.9936,      0.9825,
      "v2025",    "CH"   ,     1.0035,           1,
      "v2025",    "CHU"  ,     1.0035,           1,
      "v2025",    "EBNL",           1,      0.9823,
  ) -> segur_charge)

  ((
    helper_ES_type(champ = "mco")
    |> dplyr::filter(annee == année_activité)
    |> dplyr::select(finess, type_ES, TOP_DIALYSE)
    |> dplyr::distinct()
  ) -> typage_ES)

  (version_avant_mars <- paste0("v", (année_activité - 1) |> as.integer()))
  (version_apres_mars <- paste0("v", (année_activité)     |> as.integer()))

  ((
    typage_ES
    |> dplyr::inner_join(segur_charge, copy = TRUE)
    |> dplyr::filter(version %in% c(version_avant_mars, version_apres_mars))
    |> dplyr::select(finess, version, segur, charge)
  ) -> coefs)


  ct(MCO, FIXE, année = année_activité)
  ct(MCO, VALO_DGF, année = année_activité)

  ((
      FIXE
      |> fonction_groupage_volume_valorisation()
      |> dplyr::mutate(version = dplyr::case_when(
        mois < 3  ~ version_avant_mars,
        mois >= 3 ~ version_apres_mars))
      |> dplyr::inner_join(coefs)
      %>% { if (année_activité < 2017)
        dplyr::select(., finess, ident, dplyr::starts_with("groupage_"),
                      code_valo = ghs, ghs,
                      version, segur, charge, mois)
        else
          dplyr::select(., finess, ident, dplyr::starts_with("groupage_"),
                        ghs,
                        version, segur, charge, mois) }
      |> dplyr::inner_join(VALO_DGF)
      |> dplyr::left_join(suppléments_coef_pour_base(année_base))
      |> dplyr::left_join(ghs_coef_pour_base(année_base))
      |> dplyr::filter(mois <= mois_de_sortie_année_activité)
  ) -> x)

  (colonnes_valo_simples <- c(
    "mnt_ghs_br",   'mnt_exinf_br', 'mnt_ghs50_br', 'mnt_exh_br',
    'mnt_9615_br', "mnt_rdthp_br", 'mnt_ante_br',  'mnt_rea_br',
    'mnt_rep_br', 'mnt_nn1_br', 'mnt_nn2_br', 'mnt_nn3_br',
    'mnt_caisson_br', "mnt_rdthp_br", "mnt_supp_inno_br",
    'mnt_sdc_br', "mnt_ctc_br",  "mnt_sc_br",  "mnt_si_br"))

  (colonnes_valo_radioth <- c(
    "mnt_9610_br", "mnt_9619_br", "mnt_9620_br", "mnt_9621_br",
    "mnt_9622_br", "mnt_9623_br", "mnt_9625_br", "mnt_9631_br",
    "mnt_9632_br", "mnt_9633_br", "mnt_9639_br"))

  (colonnes_valo_organe <- c(
    'mnt_po1_br', 'mnt_po2_br', 'mnt_po3_br',
    'mnt_po4_br', 'mnt_po5_br', 'mnt_po6_br',
    'mnt_po7_br', 'mnt_po8_br', 'mnt_po9_br', 'mnt_poa_br'))

  (colonnes_valo_dialyse <- c(
    "mnt_suppent1_br",
    "mnt_suppent2_br",
    "mnt_suppent3_br",
    "mnt_supphd_br",
    "mnt_dip_br"))

  (
    x

    # Valorisation des GHS
    |> dplyr::mutate(mnt_ghs_br   = mnt_ghs_br   * coef / segur / charge)

    # Valorisation extrême bas
    %>% { if ("mnt_exinf_br" %in% names(.))
      dplyr::mutate(., mnt_exinf_br   = mnt_exinf_br   * coef / segur / charge) else . }

    # Valorisation séjours avec rehosp dans même GHM
    %>% { if ("mnt_ghs50_br" %in% names(.))
      dplyr::mutate(., mnt_ghs50_br   = mnt_ghs50_br   * coef / segur / charge) else . }

    # Valorisation journées extrême haut
    |> dplyr::mutate(mnt_exh_br   = mnt_exh_br   * coef / segur / charge)

    # Valorisation actes GHS 9615 en Hospit.
    %>% { if ("mnt_9615_br" %in% names(.))
      dplyr::mutate(., mnt_9615_br   = mnt_9615_br   * `9615`) else . }

    # Valorisation suppléments radiothérapie pédiatrique
    |> dplyr::mutate(mnt_rdthp_br = mnt_rdthp_br * RAP)

    # Valorisation suppléments antepartum
    |> dplyr::mutate(mnt_ante_br  = mnt_ante_br  * ANT)

    # Valorisation actes RDTH en Hospit.
    %>% { if ("mnt_9610_br" %in% names(.))
      dplyr::mutate(., mnt_9610_br   = mnt_9610_br   * `9610`) else . }
    %>% { if ("mnt_9619_br" %in% names(.))
      dplyr::mutate(., mnt_9619_br   = mnt_9619_br   * `9619`) else . }
    %>% { if ("mnt_9620_br" %in% names(.))
      dplyr::mutate(., mnt_9620_br   = mnt_9620_br   * `9620`) else . }
    %>% { if ("mnt_9621_br" %in% names(.))
      dplyr::mutate(., mnt_9621_br   = mnt_9621_br   * `9621`) else . }
    %>% { if ("mnt_9622_br" %in% names(.))
      dplyr::mutate(., mnt_9622_br   = mnt_9622_br   * `9622`) else . }
    %>% { if ("mnt_9623_br" %in% names(.))
      dplyr::mutate(., mnt_9623_br   = mnt_9623_br   * `9623`) else . }
    %>% { if ("mnt_9625_br" %in% names(.))
      dplyr::mutate(., mnt_9625_br   = mnt_9625_br   * `9625`) else . }
    %>% { if ("mnt_9631_br" %in% names(.))
      dplyr::mutate(., mnt_9631_br   = mnt_9631_br   * `9631`) else . }
    %>% { if ("mnt_9632_br" %in% names(.))
      dplyr::mutate(., mnt_9632_br   = mnt_9632_br   * `9632`) else . }
    %>% { if ("mnt_9633_br" %in% names(.))
      dplyr::mutate(., mnt_9633_br   = mnt_9633_br   * `9633`) else . }
    %>% { if ("mnt_9639_br" %in% names(.))
      dplyr::mutate(., mnt_9639_br   = mnt_9639_br   * `9639`) else . }

    # Valorisation suppléments de réanimation
    |> dplyr::mutate(mnt_rea_br   = mnt_rea_br   * REA)

    # Valorisation suppléments de réa pédiatrique
    |> dplyr::mutate(mnt_rep_br   = mnt_rep_br   * REP)

    # Valorisation suppléments de néonat sans SI
    |> dplyr::mutate(mnt_nn1_br   = mnt_nn1_br   * NN1)

    # Valorisation suppléments de néonat avec SI
    |> dplyr::mutate(mnt_nn2_br   = mnt_nn2_br   * NN2)

    # Valorisation suppléments de réanimation néonat
    |> dplyr::mutate(mnt_nn3_br   = mnt_nn3_br   * NN3)

    # Valorisation prélévements organe
    %>% { if ("mnt_po1_br" %in% names(.))
      dplyr::mutate(., mnt_po1_br   = mnt_po1_br   * `PO1`) else . }
    %>% { if ("mnt_po2_br" %in% names(.))
      dplyr::mutate(., mnt_po2_br   = mnt_po2_br   * `PO2`) else . }
    %>% { if ("mnt_po3_br" %in% names(.))
      dplyr::mutate(., mnt_po3_br   = mnt_po3_br   * `PO3`) else . }
    %>% { if ("mnt_po4_br" %in% names(.))
      dplyr::mutate(., mnt_po4_br   = mnt_po4_br   * `PO4`) else . }
    %>% { if ("mnt_po5_br" %in% names(.))
      dplyr::mutate(., mnt_po5_br   = mnt_po5_br   * `PO5`) else . }
    %>% { if ("mnt_po6_br" %in% names(.))
      dplyr::mutate(., mnt_po6_br   = mnt_po6_br   * `PO6`) else . }
    %>% { if ("mnt_po7_br" %in% names(.))
      dplyr::mutate(., mnt_po7_br   = mnt_po7_br   * `PO7`) else . }
    %>% { if ("mnt_po8_br" %in% names(.))
      dplyr::mutate(., mnt_po8_br   = mnt_po8_br   * `PO8`) else . }
    %>% { if ("mnt_po9_br" %in% names(.))
      dplyr::mutate(., mnt_po9_br   = mnt_po9_br   * `PO9`) else . }
    %>% { if ("mnt_poa_br" %in% names(.))
      dplyr::mutate(., mnt_poa_br   = mnt_poa_br   * `POA`) else . }

    # Valorisation des actes de caissons hyperbares en sus
    %>% { if ("mnt_caisson_br" %in% names(.))
      dplyr:: mutate(., mnt_caisson_br   = mnt_caisson_br   * `9614`) else . }

    # Valorisation suppléments de dialyse
    %>% { if ("mnt_suppent1_br" %in% names(.))
      dplyr::mutate(., mnt_suppent1_br   = mnt_suppent1_br   * `D20`) else . }
    %>% { if ("mnt_suppent2_br" %in% names(.))
      dplyr::mutate(., mnt_suppent2_br   = mnt_suppent2_br   * `D21`) else . }
    %>% { if ("mnt_suppent3_br" %in% names(.))
      dplyr::mutate(., mnt_suppent3_br   = mnt_suppent3_br   * `D24`) else . }
    %>% { if ("mnt_supphd_br" %in% names(.))
      dplyr::mutate(., mnt_supphd_br   = mnt_supphd_br   * `D11`) else . }
    %>% { if ("mnt_dip_br" %in% names(.))
      dplyr::mutate(., mnt_dip_br   = mnt_dip_br   * `DIP`) else . }

    # Valorisation suppléments pour pose de défibrillateur
    %>% { if ("mnt_sdc_br" %in% names(.))
      dplyr::mutate(., mnt_sdc_br   = mnt_sdc_br   * `SDC`) else . }

    # Valorisation suppléments Car-t Cells
    %>% { if ("mnt_ctc_br" %in% names(.))
      dplyr::mutate(., mnt_ctc_br   = mnt_ctc_br   * `CTC`) else . }

    # Valorisation suppléments de surveillance continue validés
    |> dplyr::mutate(mnt_sc_br    = mnt_sc_br    * SRC)

    # Valorisation suppléments de soins intensifs validés
    |> dplyr::mutate(mnt_si_br    = mnt_si_br    * STF)

    # Valorisation suppléments Forfait Innovation
    # /!\ Je ne trouve pas de colonnes dans la table VALO_DGF
    # /!\ Ça ne marche pas non plus avec les "GHS" I10 innovation...


    |> table_temporaire(MONTANTS_RECALCULÉS)
  )

  cli::cli_alert_info("ORGANES")

  ((
    MONTANTS_RECALCULÉS
    |> dplyr::group_by(dplyr::across(c(finess, dplyr::starts_with("groupage"))))
    |> dplyr::summarise(dplyr::across(dplyr::any_of(colonnes_valo_organe), sum))
    |> dplyr::ungroup()
    %>% {
      if(sum(colonnes_valo_organe %in% colnames(.)) > 0)
        (tidyr::pivot_longer(., cols = dplyr::any_of(colonnes_valo_organe),
                             names_to = "names",
                             values_to = "value")
         |> dplyr::group_by(dplyr::across(c(finess, dplyr::starts_with("groupage"))))
         |> dplyr::summarise(
           value = sql('SUM(CAST("value" AS DECIMAL(38,0)))')
         )
        )
      else dplyr::mutate(., value = 0)
    }
    |> table_temporaire(ORGANE)
  ))

  cli::cli_alert_info("RADIOTH")

  (
    MONTANTS_RECALCULÉS
    |> dplyr::filter(valo == 1)
    |> dplyr::group_by(dplyr::across(c(finess, dplyr::starts_with("groupage"))))
    |> dplyr::summarise(dplyr::across(dplyr::any_of(colonnes_valo_radioth), sum))
    |> dplyr::ungroup()
    %>% { if(sum(colonnes_valo_radioth %in% names(.)) > 0)
      (tidyr::pivot_longer(., cols = dplyr::any_of(colonnes_valo_radioth),
                           names_to = "names",
                           values_to = "value")
       |> dplyr::group_by(dplyr::across(c(finess, dplyr::starts_with("groupage"))))
       |> dplyr::summarise(
         value = sql('SUM(CAST("value" AS DECIMAL(38,0)))')
       )
      )
      else dplyr::mutate(., value = 0) }
    |> table_temporaire(RADIOTHÉRAPIE)
  )

  cli::cli_alert_info("DIALYSE")

  ((
    MONTANTS_RECALCULÉS
    |> dplyr::filter(valo == 1)
    |> dplyr::group_by(dplyr::across(c(finess, dplyr::starts_with("groupage"))))
    |> dplyr::summarise(dplyr::across(dplyr::any_of(colonnes_valo_dialyse), sum))
    |> dplyr::ungroup()
    %>% { if(sum(colonnes_valo_dialyse %in% names(.)) > 0)
      (tidyr::pivot_longer(., cols = dplyr::any_of(colonnes_valo_dialyse),
                           names_to = "names",
                           values_to = "value")
       |> dplyr::group_by(dplyr::across(c(finess, dplyr::starts_with("groupage"))))
       |> dplyr::summarise(
         value = sql('SUM(CAST("value" AS DECIMAL(38,0)))')
       )
      )
      else dplyr::mutate(., value = 0) }
    |> table_temporaire(DIALYSE)
  ))

  cli::cli_alert_info("AUTRES")

  ((
    MONTANTS_RECALCULÉS
    |> dplyr::filter(valo == 1)
    |> dplyr::group_by(dplyr::across(c(finess, dplyr::starts_with("groupage"))))
    |> dplyr::summarise(dplyr::across(dplyr::any_of(colonnes_valo_simples), sum))
    |> dplyr::ungroup()
    |> tidyr::pivot_longer(cols = dplyr::any_of(colonnes_valo_simples),
                           names_to = "names",
                           values_to = "value")
    |> dplyr::group_by(dplyr::across(c(finess, dplyr::starts_with("groupage"))))
    |> dplyr::summarise(
      value = sql('SUM(CAST("value" AS DECIMAL(38,0)))')
    )
    |> table_temporaire(AUTRES)
  ))


  ((
    dplyr::bind_rows(
      dplyr::collect(ORGANE       ), #  |> dplyr::filter(value != 0)),
      dplyr::collect(RADIOTHÉRAPIE), #  |> dplyr::filter(value != 0)),
      dplyr::collect(DIALYSE      ), #  |> dplyr::filter(value != 0)),
      dplyr::collect(AUTRES)
    )
    |> dplyr::group_by(dplyr::across(c(finess, dplyr::starts_with("groupage"))))
    |> dplyr::summarise(value = sum(value) )
    |> dplyr::mutate(année = année_activité)
    # |> dplyr::mutate(value = value / mois_de_sortie_année_activité)
    # |> dplyr::mutate(value = value * 12)
    |> dplyr::rename_with(~ stringr::str_replace(., "groupage_", ""))
    |> dplyr::rename(Valorisation = value)
  ) -> total_valo)

  (total_global <- sum(total_valo |> dplyr::pull(Valorisation)))

  cli::cli_alert_success(
    glue::glue(
      "Année {année_activité} : ",
      "{total_global |> scales::number_format(accuracy = 1)()} EUR"))

  total_valo
}

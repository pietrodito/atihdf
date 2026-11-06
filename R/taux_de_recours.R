#'@export
territoires_patient <- function(année) {
  ct(NOMGEN, ALL_CORRESP_GEO)
  ((
    ALL_CORRESP_GEO
    %>% dplyr::filter(annee_deb <= année & annee_fin >= année)
    %>% dplyr::mutate(zone_France = dplyr::case_when(
      lib_pays != "FRANCE" ~ NA,
      lib_reg %in%  c("TOM", "CORSE") ~ lib_reg,
      stringr::str_length(code_dep) > 2 ~ "DOM",
      .default = "HEXAGONE"
    ))
    %>% dplyr::select(code_codegeo,
                      code_reg, lib_reg,
                      code_dep, lib_dep,
                      code_zon, lib_zon,
                      zone_France, lib_pays)
    %>% dplyr::rename(codegeo = code_codegeo,
                      code_zone_région = code_reg,
                      zone_Région = lib_reg,
                      code_zone_département = code_dep,
                      zone_Département = lib_dep,
                      code_zone_zas = code_zon,
                      zone_ZAS = lib_zon,
                      Pays = lib_pays)
  ))
}

#'@export
population_codegeo <- function(année) {
  ct(NOMGEN, POP_PAR_REG_DEP_TS_ZON_GEO_FR)
((
  POP_PAR_REG_DEP_TS_ZON_GEO_FR
  %>% dplyr::filter(annee == année, cl_age != "9", sexe != "9")
  %>% dplyr::filter(typrgp == "geo")
  %>% dplyr::select(codegeo = code, cl_age, sexe, pop)
))
}

summarise_sexe_cl_age_by_zone_column <- function(df, column) {
  group_columns <- c(column, "sexe", "cl_age")
  (
    df
    %>% dplyr::group_by(dplyr::across(dplyr::all_of(group_columns)))
    %>% dplyr::mutate( "Population_{column}" := sum(pop) )
    %>% dplyr::ungroup()
  )
}

#'@export
population_toutes_zones <- function(année) {
  ((
    territoires_patient(année)
    %>% dplyr::inner_join(population_codegeo(année))
    %>% summarise_sexe_cl_age_by_zone_column("zone_ZAS")
    %>% summarise_sexe_cl_age_by_zone_column("zone_Région")
    %>% summarise_sexe_cl_age_by_zone_column("zone_Département")
    %>% summarise_sexe_cl_age_by_zone_column("zone_France")
  ) -> avant_ajout_pop_standard_hexagonale)
  ((
    avant_ajout_pop_standard_hexagonale
    %>% dplyr::filter(zone_France == "HEXAGONE")
    %>% dplyr::select(cl_age, sexe, pop_standard = Population_zone_France)
    %>% dplyr::distinct()
  ) -> pop_standard_hexagonale)
  dplyr::left_join(avant_ajout_pop_standard_hexagonale,
                   pop_standard_hexagonale)
}

#'@export
standardisation_toutes_zones <- function(df,
                                         k = 100000,
                                         keep_strata = FALSE,
                                         std_from = c("column","zone"),
                                         std_col = "pop_standard",
                                         std_zone_var = NULL,
                                         std_zone_value = NULL) {

  ## df doit être construit avec
  ## population_toutes_zones(année)
  ## Avec des colonnes préfixées recours
  ## Tout colonne recours sera alors standardisée

  std_from <- base::match.arg(std_from)

  # 1) Détection colonnes -----------------------------------------------------
  cn <- base::colnames(df)

  event_cols <- cn[stringr::str_starts(cn, "recours_")]
  if (length(event_cols) == 0) base::stop("Aucune colonne 'recours_' trouvée.")

  zone_cols  <- cn[stringr::str_starts(cn, "zone_")]
  if (length(zone_cols) == 0) base::stop("Aucune colonne 'zone_' trouvée.")

  if (!("cl_age" %in% cn)) base::stop("Colonne 'cl_age' manquante.")
  if (!("sexe" %in% cn))   base::stop("Colonne 'sexe' manquante.")

  # mapping zone -> colonne de population
  pop_zone_map <- stats::setNames(paste0("Population_", zone_cols), zone_cols)
  missing_pop_cols <- base::setdiff(base::unname(pop_zone_map), cn)
  if (length(missing_pop_cols) > 0) {
    missing_for <- base::names(pop_zone_map)[pop_zone_map %in% missing_pop_cols]
    base::stop("Colonnes Population_zone_* manquantes pour : ", paste(missing_for, collapse = ", "))
  }

  # Vérifier le mode standardisation
  if (base::identical(std_from, "column")) {
    if (!(std_col %in% cn)) {
      base::stop("std_from = 'column' mais la colonne '", std_col, "' est absente.")
    }
  } else {
    if (base::is.null(std_zone_var) || base::is.null(std_zone_value)) {
      base::stop("std_from = 'zone' requiert std_zone_var et std_zone_value.")
    }
    if (!(std_zone_var %in% zone_cols)) {
      base::stop("std_zone_var='", std_zone_var, "' n'est pas une variable zone_* de df.")
    }
  }

  # 2) Long sur les indicateurs ----------------------------------------------
  long_events <- df |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(event_cols),
      names_to = "indic",
      values_to = "events"
    )

  # 2bis) Table des pondérations si std_from = "zone" -------------------------
  if (base::identical(std_from, "zone")) {
    popc_std <- pop_zone_map[[std_zone_var]]  # p.ex. "Population_zone_France"
    std_weights <- df |>
      dplyr::filter(.data[[std_zone_var]] == std_zone_value) |>
      dplyr::group_by(.data[["cl_age"]], .data[["sexe"]]) |>
      dplyr::summarise(
        pop_std = base::min(.data[[popc_std]], na.rm = TRUE),
        .groups = "drop"
      )
  }

  # 3) Calcul par zone --------------------------------------------------------
  res_by_zone <- purrr::map(zone_cols, function(zc) {
    popc <- pop_zone_map[[zc]]

    # Agrégation (zone, âge, sexe, indic)
    by_strata <- long_events |>
      dplyr::select(
        tidyselect::all_of(c(zc, "cl_age", "sexe", "indic", "events", popc)),
        tidyselect::all_of(if (base::identical(std_from, "column")) std_col else character())
      ) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(zc, "cl_age", "sexe", "indic")))) |>
      dplyr::summarise(
        events_zone = base::sum(.data[["events"]], na.rm = TRUE),
        pop_zone    = base::min(.data[[popc]], na.rm = TRUE),
        !!!{
          if (base::identical(std_from, "column")) {
            rlang::list2(pop_std = rlang::expr(base::min(.data[[!!std_col]], na.rm = TRUE)))
          } else {
            base::list()
          }
        },
        .groups = "drop"
      )

    if (base::identical(std_from, "zone")) {
      by_strata <- by_strata |>
        dplyr::left_join(std_weights, by = c("cl_age","sexe"))
    }

    by_strata <- by_strata |>
      dplyr::mutate(
        rate_stratum = dplyr::if_else(pop_zone > 0, events_zone / pop_zone, NA_real_)
      )

    # Agrégats (en 2 temps)
    summary_zone <- by_strata |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(zc, "indic")))) |>
      dplyr::summarise(
        crude_events = base::sum(.data[["events_zone"]], na.rm = TRUE),
        crude_pop    = base::sum(.data[["pop_zone"]],    na.rm = TRUE),
        num          = base::sum(.data[["pop_std"]] * .data[["rate_stratum"]], na.rm = TRUE),
        den          = base::sum(.data[["pop_std"]],                                 na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        crude_rate  = dplyr::if_else(crude_pop > 0, crude_events / crude_pop, NA_real_),
        dsr         = dplyr::if_else(den > 0,        num          / den,      NA_real_),
        rate_brut_k = k * crude_rate,
        rate_std_k  = k * dsr,
        zone_var    = zc                      # <- plus de translittération : nom exact, accents inclus
      ) |>
      dplyr::rename(zone = !!rlang::sym(zc)) |>
      dplyr::select(
        zone_var, zone, indic,
        Événements = crude_events, Population = crude_pop,
        Taux_brut = crude_rate, Taux_std = dsr,
        "Taux_brut_pour_{k}" := rate_brut_k,
        "Taux_std_pour_{k}" := rate_std_k
      )

    if (keep_strata) {
      by_strata_out <- by_strata |>
        dplyr::mutate(
          rate_stratum_k = k * rate_stratum,
          zone_var = zc
        ) |>
        dplyr::rename(zone = !!rlang::sym(zc)) |>
        dplyr::select(
          zone_var, zone, indic, cl_age, sexe,
          events_zone, pop_zone, pop_std, rate_stratum, rate_stratum_k
        )
      return(base::list(summary = summary_zone, strata = by_strata_out))
    } else {
      return(summary_zone)
    }
  })

  # 4) Sortie (concaténer en lazy avec UNION ALL) -----------------------------
  if (keep_strata) {
    summary_list <- purrr::map(res_by_zone, "summary")
    strata_list  <- purrr::map(res_by_zone, "strata")

    out_summary <- if (length(summary_list) == 1L) {
      summary_list[[1L]]
    } else {
      purrr::reduce(summary_list, dplyr::union_all)
    }

    out_strata <- if (length(strata_list) == 1L) {
      strata_list[[1L]]
    } else {
      purrr::reduce(strata_list, dplyr::union_all)
    }

    return(base::list(summary = out_summary, strata = out_strata))
  } else {
    if (length(res_by_zone) == 1L) {
      return(res_by_zone[[1L]])
    } else {
      return(purrr::reduce(res_by_zone, dplyr::union_all))
    }
  }
}

#'@export
standardisation_focus_HDF <- function(df) {
  ct(NOMGEN, ALL_CORRESP_GEO)
  ((
    ALL_CORRESP_GEO
    %>% dplyr::filter(code_reg == "32", ! is.na(lib_zon))
    %>% dplyr::filter(annee_deb <= 2024 & annee_fin >= 2024)
    %>% dplyr::distinct(lib_zon)
    %>% dplyr::pull()
  ) -> ZAS_HDF)
  ((
    départements_hdf$lib_département
    %>% stringr::str_to_upper()
  ) -> DPT_HDF)

  (régions_exclues <- c("GUADELOUPE", "GUYANE", "LA RÉUNION",
                        "MARTINIQUE", "MAYOTTE", "TOM"))

  (
    standardisation_toutes_zones(df)
    %>% dplyr::filter(
      ! zone_var == "zone_ZAS" | zone %in% ZAS_HDF,
      ! zone_var == "zone_Départ" | zone %in% DPT_HDF,
      ! zone_var == "zone_Région" | ! zone %in% régions_exclues,
      ! zone_var == "zone_France" | zone == "HEXAGONE",
    )
    %>% dplyr::collect()
    %>% dplyr::mutate(zone_var = factor(
      zone_var,
      levels = c("zone_France", "zone_Région", "zone_Départ", "zone_ZAS"),
      labels = c("Héxgone", "Région", "Département", "ZAS")
    ))
  )
}

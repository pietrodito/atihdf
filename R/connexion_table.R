#' Crée une connexion vers une table TERADATA
#' @description
#' En même temps, la fonction assigne une variable du même nom dans
#' l'environnement global.
#'
#' Si une année est passée en paramètre, l'année devient un attribut de la
#' variable retournée et créée. Ainsi, les autres fonctions du package ont
#' "conscience" de l'année de la table ainsi pointée. Idem pour le schéma.
#'
#'@export
connexion_table <- function(schéma, table, année = NULL, verbose = TRUE) {

  # Normalisation légère pour comparer proprement (NULL reste NULL)
  norm_chr <- function(x) if (is.null(x)) NULL else as.character(x)

  # 1) Si la variable existe déjà, vérifier si la connexion est identique
  if (exists(table, envir = .GlobalEnv, inherits = FALSE)) {
    existing <- get(table, envir = .GlobalEnv, inherits = FALSE)

    same_schema <- identical(norm_chr(attr(existing, "schéma")),
                             norm_chr(schéma))
    same_table  <- identical(norm_chr(attr(existing, "table")),
                             norm_chr(table))
    same_year   <- identical(norm_chr(attr(existing, "année")),
                             norm_chr(année))

    if (isTRUE(same_schema && same_table && same_year)) {
      # => Connexion identique : on ne fait rien
      if (isTRUE(verbose)) {
        cli::cli_alert_info(
          "Variable {table} déjà connectée à cette table (aucune action).")
      }
      return(invisible(existing))
    } else {
      # => Existe mais différente : on remplace
      if (isTRUE(verbose)) {
        cli::cli_alert_warning(
          "La variable {table} existe déjà mais pointe vers une autre table."
        )
      }
    }
  }

  # 2) Sinon (ou si différente), créer la connexion
  complete_name <- if (!is.null(année)) {
    glue::glue("PRD_VUE_{schéma}_{année}.{table}")
  } else {
    glue::glue("PRD_VUE_{schéma}.{table}")
  }

  connection_to_assign <- dplyr::tbl(the$connection, I(complete_name))
  attr(connection_to_assign, "année")  <- année
  attr(connection_to_assign, "table")  <- table
  attr(connection_to_assign, "schéma") <- schéma

  assign(table, connection_to_assign, .GlobalEnv)

  if (isTRUE(verbose)) {
    cli::cli_alert_success("Variable {table} connectée à {complete_name}")
  }
}

#' Crée une connexion vers une table TERADATA
#'@export
ct <-  function(schéma, ..., année = NULL, verbose = TRUE) {

  schema <- rlang::quo_name(rlang::enquo(schéma))

  dots <- rlang::enquos(...)

  # Si année n'est pas fournie explicitement,
  # on tente de l'inférer depuis le dernier élément de ...
  if (is.null(année) && length(dots) > 0) {
    last_expr <- rlang::quo_get_expr(dots[[length(dots)]])

    # Cas typique: ct(MCO, FIXE, ACTE, 2024)
    if (is.numeric(last_expr) && length(last_expr) == 1) {
      année <- last_expr
      dots <- dots[-length(dots)]
    }
  }

  if (length(dots) == 0) {
    stop("Aucune table fournie. Usage: ct(MCO, FIXE, ACTE, 2024) ",
         call. = FALSE)
  }

  tables <- vapply(dots, rlang::quo_name, character(1))

  res <- purrr::walk(tables, function(tbl) {
    connexion_table(schema, tbl, année, verbose)
  })

  invisible()
}


#' Crée une table temporaire côté TERADATA
#'
#' @description
#'
#' Utile si beaucoup de requêtes imbriquées car :
#' - le code SQL généré peut finir  par être trop long
#' - les performances peuvent êtretrès dégradées
#'
#' À noter, une variable du nom de la table est créée dans l'environnement
#' global.
#'
#'@export
table_temporaire <- function(df, nom_de_table) {

  nom_de_table <- rlang::quo_name(rlang::enquo(nom_de_table))
  dplyr::compute(df, nom_de_table, overwrite = TRUE)

  connection_to_assign <- dplyr::tbl(the$connection, I(nom_de_table))

  assign(nom_de_table,
         connection_to_assign,
         .GlobalEnv)

}

#' Copie une table R vers TERADATA
#'
#' @description
#'
#' À noter, une variable du nom de la table est créée dans l'environnement
#' global.
#'
#'@export
copier_vers_teradata <- function(df, nom_de_table) {

  nom_de_table <- rlang::quo_name(rlang::enquo(nom_de_table))
  dplyr::copy_to(connexion(), df, nom_de_table, overwrite = TRUE)

  connection_to_assign <- dplyr::tbl(connexion(), I(nom_de_table))

  assign(nom_de_table,
         connection_to_assign,
         .GlobalEnv)

}

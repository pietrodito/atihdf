the <- new.env(parent = emptyenv())

supprime_anciens_lien_tbl_teradata <- function() {

  cli::cli_h1("Nettoyage des anciens liens vers {.emph TERADATA}")

  # Environnement ciblé (le plus courant : global)
  env <- .GlobalEnv

  # Liste des objets
  objs <- ls(envir = env)

  # Filtrer ceux de classe tbl_Teradatih
  is_target <- vapply(
    objs,
    function(x) {
      obj <- get(x, envir = env)
      inherits(obj, "tbl_Teradatih")
    },
    logical(1)
  )

  targets <- objs[is_target]

  # Si on en trouve
  if (length(targets) > 0) {

    packageStartupMessage(
      sprintf("Suppression des liens : %s",
              paste(targets, collapse = ", "))
    )

    rm(list = targets, envir = env)
  } else {
    packageStartupMessage("Aucun lien trouvé.")
  }
}

.onAttach <-function(libname, pkgname) {

  cli::cli_h1("Chargement package {.emph ATIHDF}")

  packageStartupMessage("    _  _____ ___ _   _ ____  _____")
  packageStartupMessage("   / \\|_   _|_ _| | | |  _ \\|  ___|" )
  packageStartupMessage("  / _ \\ | |  | || |_| | | | | |_")
  packageStartupMessage(" / ___ \\| |  | ||  _  | |_| |  _|" )
  packageStartupMessage("/_/   \\_\\_| |___|_| |_|____/|_|")

  supprime_anciens_lien_tbl_teradata()

}


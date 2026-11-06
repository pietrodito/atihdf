connect_teradata_if_necessary <- function() {
  necessary <- is.null(the$connection) || ! DBI::dbIsValid(the$connection)
    if (necessary) {
      cli::cli_h1("Connexion au serveur {.emph TERADATA}")
      cli::cli_alert_info(
        "Tentative de connexion...")

      # quiet_connector <-  purrr::quietly(pRatihque::connection_database)
      # connection_return <- quiet_connector(
      #   .connection_string = paste()
      # )
      #
      # the$connection <- connection_return$result

      the$connection <- pRatihque::connection_database(
        MechanismName = 'LDAP',
        uid = rstudioapi::showPrompt("Utilisateur", "Vous êtes ?"),
        pwd = rstudioapi::askForPassword("Mot de passe :"))

      cli::cli_alert_success(
        "Connecté au serveur !")
    } else {
      cli::cli_h1("Connexion au serveur {.emph TERADATA}")
      cli::cli_alert_info(
        "Déjà connecté au serveur...")
    }
  invisible(necessary)
}


#'@export
teradata <- function()  {
  ## return TRUE if connection already up
  connect_teradata_if_necessary()
}

#'@export
connexion <- function() {
  the$connection
}



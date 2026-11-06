teradata()

connexion()

DBI::dbWriteTable(
  connexion(),
  "ccc-6673.MTCARS",
  mtcars
)

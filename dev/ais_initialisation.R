#' ---
#' title : "talassaR - ais_initialisation"
#' author : Aubin Woehrel
#' creation date : 2026-05-07
#' ---
#'
#' =============================================================================
#'
#' talassaR :
#' Initialisation de base de données AIS
#' 
#' Description :
#' Initialisation d'entités dans une db postgreSQL permettant par la suite 
#' (dans un autre script) de pouvoir compiler toutes les données AIS au sein 
#' d'une même base de données. Ultérieurement, objectif de correction et de 
#' spatialisation des entités au sein du Parc pour la computation du modèle 
#' TALASSA. 
#'
#' =============================================================================


# Initialisation ----

# Nettoyage 
rm(list = ls())

# Manipulations sql et json
library("rjson")
library("DBI")

# Ressources locales 
source("r/paths.R")
source("r/fct_ais_reading.R")

# Connection base de données locale
con <- dbConnect(
  RPostgres::Postgres(),
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres",
  dbname   = "talassa_dev"
)

# Liste des schémas, objets, tables
all_schemas <- DBI::dbGetQuery(con, "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA")
dbListTables(con)
dbListObjects(con)

# Creation liste simple 
marinetraffic_list <- list.files(paths$raw_ais_marinetraffic)
df_towrite <- read_section(marinetraffic_list[1])[0,]

# Write the empty dataframe to PostgreSQL to create the table
dbWriteTable(
  conn = con,
  name = Id(schema = "ais_marinetraffic", table = "ais_raw"),  # Table name
  value = df_towrite,
  row.names = FALSE,
  overwrite = FALSE, 
  append = FALSE
)

# Liste des tables dispos
dbListTables(co cn
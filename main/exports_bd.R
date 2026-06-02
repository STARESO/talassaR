#' ---
#' title : "talassaR - exports_bd"
#' author : Aubin Woehrel
#' creation date : 2026-05-12
#' ---
#'
#' =============================================================================
#'
#' talassaR : Export BD
#'
#' Description :
#' Script permettant d'enregister les exports des données finales du format
#' talassa pour le projet dans une bd postgreSQL dédiée. 
#'
#' =============================================================================


# Initialisation ----

rm(list = ls())

# Import des librairies et ressources locales 

# Manipulations de données
library("dplyr")
library("tidyr")
library("stringr")

# Données spatiales
library("sf")

# Connections BD
library("RPostgres")
library("rpostgis")

# Config file import
library("yaml")

# Ressources locales projet
paths <- yaml::read_yaml("config/paths.yml")
 

# Import des données R talassa traitées ----
talassa_activites <- st_read(paths$processed$hex_activites)
talassa_activites_intitule <- st_read(paths$processed$hex_activites_intitule)
talassa_habitats <- st_read(paths$processed$hex_habitats)
talassa_carroyage <- st_read(paths$processed$hex_carroyage)


# Connection et exports BD ----

# Import fichier config contenant les paramètres de connection
config <- yaml.load_file("config.yml")

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = config$db$dbname,
  host = config$db$host,
  port = config$db$port,
  user = config$db$user,
  password = config$db$password
)

# Check des tables existantes
dbListTables(con)

# Check présence postgis
pgPostGIS(con) # Voir output pour statut installation

# Attention, enregistrement des données dans la BD par écrasement !
overwrite <- TRUE # Flag d'écrasement des BD existantes
schema_chosen <- "source_hex" # Nom du schema choisi

if (overwrite) {

  # Activites
  dbWriteTable(
    conn = con, 
    name = Id(schema = schema_chosen, table = "activites_hex"),
    value = talassa_activites,
    overwrite = TRUE
  )

  # Activites
  dbWriteTable(
    conn = con,
    name = Id(schema = schema_chosen, table = "activites_intitules_hex"),
    value = talassa_activites_intitule,
    overwrite = TRUE
  )

  # Activites
  dbWriteTable(
    conn = con,
    name = Id(schema = schema_chosen, table = "habitats_hex"),
    value = talassa_habitats,
    overwrite = TRUE
  )

  # Activites
  dbWriteTable(
    conn = con,
    name = Id(schema = schema_chosen, table = "carroyage_hex"),
    value = talassa_carroyage,
    overwrite = TRUE
  )
}

RPostgres::dbDisconnect(con)

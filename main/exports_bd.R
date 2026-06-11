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


## Configurations export et chemins ----

# Fichiers yaml
paths <- yaml::read_yaml("config/paths.yml") # Chemins
config <- yaml.load_file("config/secrets.yml") # Secrets de config connection db

# Choix généraux
choix_carroyage <- "arp"
overwrite <- TRUE # Flag d'écrasement des BD existantes. Attention, enregistrement des données dans la BD par écrasement !
schema_chosen <- "source_data" # Nom du schema choisi

# Prise en compte du choix
if (choix_carroyage == "hex5") {
  talassa_activites <- st_read(paths$processed$hex_activites)
  talassa_activites_intitule <- st_read(paths$processed$hex_activites_intitule)
  talassa_habitats <- st_read(paths$processed$hex_habitats)
  talassa_carroyage <- st_read(paths$processed$hex_carroyage)
  table_activites <- "activites_hex5"
  table_activites_intitules <- "activites_intitules_hex5"
  table_habitats <- "habitats_hex5"
  table_carroyage <- "carroyage_hex5"

} else if (choix_carroyage == "arp") {
  talassa_activites <- st_read(paths$processed$arp_activites)
  talassa_activites_intitule <- st_read(paths$processed$arp_activites_intitule)
  talassa_habitats <- st_read(paths$processed$arp_habitats)
  talassa_carroyage <- st_read(paths$processed$arp_carroyage)
  table_activites <- "activites_arp"
  table_activites_intitules <- "activites_intitules_arp"
  table_habitats <- "habitats_arp"
  table_carroyage <- "carroyage_arp"
  
} else {
  stop("Mauvais choix de carroyage. Choisir un carroyage valide")
}


# Connection et exports BD ----
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


if (overwrite) {

  # Activites
  dbWriteTable(
    conn = con, 
    name = Id(schema = schema_chosen, table = table_activites),
    value = talassa_activites,
    overwrite = TRUE
  )

  # Activites
  dbWriteTable(
    conn = con,
    name = Id(schema = schema_chosen, table = table_activites_intitules),
    value = talassa_activites_intitule,
    overwrite = TRUE
  )

  # Activites
  dbWriteTable(
    conn = con,
    name = Id(schema = schema_chosen, table = table_habitats),
    value = talassa_habitats,
    overwrite = TRUE
  )

  # Activites
  dbWriteTable(
    conn = con,
    name = Id(schema = schema_chosen, table = table_carroyage),
    value = talassa_carroyage,
    overwrite = TRUE
  )
}

RPostgres::dbDisconnect(con)

print(paste("Exports dans bd fait pour", choix_carroyage))

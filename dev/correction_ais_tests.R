#' ---
#' title : "talassaR - correction_ais_test"
#' author : Aubin Woehrel
#' creation date : 2026-04-14
#' ---
#'
#' =============================================================================
#'
#' talassaR : Correction des données ais
#'
#' Description :
#' Compilation des anciens test dev données ais diverses (marine traffic, 
#' vessel finder, etc). A permis de savoir quelles données sont exploitables ou 
#' non pour le modèle. 
#'
#' =============================================================================

# Initialisation ----

## Nettoyage ----
rm(list = ls())

## Import des librairies ----

# Data: Import et manipulations
library("readr")
library("dplyr")
library("tidyr")
library("stringr")

# Manipulations sql et json
library("rjson")
library("DBI")

# Data: Spatial
library("sf")

## Ressources locales ----
source("r/paths.R")
source("r/fct_ais_reading.R")

# Test Vessel Finder ----
ais_vf <- st_read(paths$raw_ais_vesselfinder) 

#  Corrections initiales
skimr::skim(ais_vf)
names(ais_vf)
head(ais_vf)

ais_vf <- ais_vf %>%
  rename_with(str_to_lower)

ais_vf <- ais_vf %>%
  rename(date = date_local) %>%
  mutate(date = as.Date(date, format = "%d/%m/%y"))

ais_vf <- ais_vf %>%
  arrange(date, type, etat)

ais_vf <- ais_vf %>%
  mutate(etat = str_to_lower(etat))

# Check caractéristiques variable "type"
ais_vf %>%
  st_drop_geometry() %>%
  count(type)

# Check caractéristiques variable "etat"
ais_vf %>%
  st_drop_geometry() %>%
  count(etat)

# Check unique des dates
unique(ais_vf$date)

type_navire <- ais_vf %>%
  st_drop_geometry() %>%
  count(etat, type, classe)

write.csv2(type_navire, paths$processed_ais_type_navire)


# Import données marine traffic

# Liste des fichiers d'AIS
marinetraffic_list<- list.files(paths$raw_ais_marinetraffic)
length(marinetraffic_list)


test <- bind_ais(marinetraffic_list[1:200])
test <- read_section(marinetraffic_list[1])
test <- test[0,]

con <- dbConnect(
  RPostgres::Postgres(),
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres",
  dbname   = "talassa_dev"
)

dbListTables(con)
dbListObjects(con)
all_schemas <- DBI::dbGetQuery(con, "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA")


df_towrite <- read_section(marinetraffic_list[1])[0,]
# Write the empty dataframe to PostgreSQL to create the table
dbWriteTable(
  conn = con,
  name = Id(schema = "ais_marinetraffic", table = "ais_raw"),  # Table name
  value = df_towrite,
  row.names = FALSE,
  overwrite = FALSE,  # Set to TRUE if you want to replace an existing table
  append = FALSE      # Set to TRUE if you want to append to an existing table
)
dbListTables(con)


# AIS marine traffic integration data Mathile
ais_mt <- read.csv(paths$raw_ais_marinetraffic_mathilde)
dim(ais_mt)
names(ais_mt)
skimr::skim(ais_mt)

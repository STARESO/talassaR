#' ---
#' title : "talassaR - correction_ais"
#' author : Aubin Woehrel
#' creation date : 2026-06-01
#' ---
#'
#' =============================================================================
#'
#' talassaR : Correction des données ais
#'
#' Description :
#' Script de correction des données ais fournies par le parc. Plusieurs sources
#' de données possibles
#'
#' Etapes réalisées :
#' 1) Nomenclature des colonnes et contenus
#' 2) Check et correction générale
#' 3) Jointure des codes RESOBLO OFB
#' 4) Export au format spatial gpkg
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

# SQL database
library("DBI")

# Données spatiales
library("sf")

## Ressources locales ----
source("r/fct_ais_reading.R")
paths <- yaml::read_yaml("config/paths.yml")


# Compilation données Marine Traffic 2025 ----
# Compilation des données Marine Traffic 2025 à partir des fichiers json fournis

# Liste des fichiers Marine traffic
mt_list <- list.files(paths$raw$ais_marinetraffic)
length(mt_list) # 34017 fichiers json

# Nombre de coeurs de l'ordi disponible
# (Permet d'adapter le nombre de coeurs utilisés pour la fonction parallélisée de lecture json)
parallel::detectCores()


# Test de comparaison performance fonction de lecture parallélisée vs non parallélisée
speed_flag <- FALSE

if (speed_flag) {
  
  speedtest1 <- bind_ais(mt_list[1:1000])
  speedtest2 <- bind_ais_parallel(mt_list[1:1000], workers = 14) # Workers à adapter selon le nombre de coeurs

  names(speedtest1)
  speedtest1$computation_time
  speedtest2$computation_time

  dim(speedtest2$data)
  names(speedtest2$data)
}

compute_compilation <- FALSE

if (compute_compilation) {

  # Compilation données version parallélisée avec choix nb de coeurs (workers)
mt_all <- bind_ais_parallel(mt_list, workers = 14)
length(mt_all$warnings) # 416

# Sauvegarde temporaire données 
  saveRDS(object = mt_all, file = paths$dev$ais_tempcompil)

} else {
  tryCatch(
    mt_all <- readRDS(file = paths$dev$ais_tempcompil),
    error = function(e) {stop("No mt_all processed file : ", e$message)}
  )
}

# Version df des erreurs pour meilleure lecture
warnings_df <- data.frame(
  row.names = NULL,
  file = sapply(mt_all$warnings, function(x) strsplit(x, " : ")[[1]][1]),  # Extract filename
  error = sapply(mt_all$warnings, function(x) strsplit(x, " : ")[[1]][2]),  # Extract error message
  stringsAsFactors = FALSE
)

# Temps de compilation
mt_time <- mt_all$computation_time
mt_time

# Enregistrement données
mt_all <- mt_all$data

# Exploration mt_all ----

# Structure
dim(mt_all)
names(mt_all)
head(mt_all, 10)

# MMSI
length(unique(mt_all$mmsi))
t1 <- mt_all %>% count(mmsi, shipname, sort = TRUE)
View(t1)

# Format temps
head(mt_all)$timestamp
class(mt_all$timestamp)

# Check de variables
mt_all %>% count(status, sort = TRUE) # Statut navigation encodé
mt_all %>% count(heading, sort = TRUE) # Direction de navigation
mt_all %>% count(ship_country, sort = TRUE) # Pays d'attache

mt_all %>% count(type_name, sort = TRUE) # Catégorie précise textuelle
mt_all %>% count(ais_type_summary, sort = TRUE) # Catégorie générale
mt_all %>% count(shiptype, sort = TRUE) # Catégorie standard encodée

mt_all %>% count(ship_class, sort = TRUE) # Type de tonnage bateau
mt_all %>% count(market, sort = TRUE) # Type d'activité générale

hist(as.numeric(mt_all$distance_travelled))
hist(as.numeric(mt_all$width))
hist(as.numeric(mt_all$length))

# Check des variables types de bateaux
types_comparison_all <- mt_all %>% 
  count(ais_type_summary, type_name, shiptype, name = "n_ping") %>%
  group_by(ais_type_summary) %>%
  arrange(ais_type_summary, type_name, desc(n_ping))

View(types_comparison_all)

types_comparison <- mt_all %>% 
  group_by(type_name, ais_type_summary) %>%
  summarise(
    n_ping = n(),
    n_mmsi = n_distinct(mmsi),
    .groups = "drop"
  ) %>%
  group_by(ais_type_summary) %>%
  mutate(
    group_n_ping = sum(n_ping),
    group_n_mmsi = sum(n_mmsi)
  ) %>%
  arrange(desc(group_n_ping), desc(n_ping)) %>%
  select(ais_type_summary, group_n_ping, group_n_mmsi, type_name, n_ping, n_mmsi)

View(types_comparison)


# Recherche fun curiosité
names <- mt_all %>%
  filter(grepl("S", shipname)) %>%
  dplyr::select(shipname) %>%
  arrange(shipname) %>%
  distinct()
View(names)

# Stareso
stareso <- mt_all %>%
  filter(shipname == 'STARESO') # Catégorie drague, bizarre


# Nettoyage ----

## Association code RESOBLO ----

# Export à compléter
openxlsx::write.xlsx(
    x = types_comparison,
    file = paths$raw$codes_ais_tocomplete,
    sheetName = "ref"
  )

# Check détails pour complétion
passenger_to_check <- mt_all %>%
  filter(
    ais_type_summary %in% c("Pleasure Craft", "Passenger") &
    type_name %in% c("Inland, Unknown", "Passenger Ship", "Passenger", "Houseboat")
  ) %>% 
  select(ais_type_summary, type_name, shipname, mmsi) %>%
  distinct() %>%
  arrange(ais_type_summary, type_name, shipname)

write.csv(passenger_to_check, file = paste0(paths$dev$ais, "passenger_to_check.csv"))

cargo_to_check <- mt_all %>%
  filter(
    ais_type_summary == "Cargo" &
    type_name %in% c("Ro-Ro Cargo", "Vehicles Carrier", "Ro-Ro/Container Carrier")
  ) %>% 
  select(ais_type_summary, type_name, shipname, mmsi) %>%
  distinct() %>%
  arrange(ais_type_summary, type_name, shipname)

write.csv(cargo_to_check, file = paste0(paths$dev$ais, "cargo_to_check.csv"))

# Import version association resoblo utilisable
ais_resoblo <- openxlsx::read.xlsx(paths$raw$codes_ais)

## Nettoyage bateaux sans code RESOBLO ----

## Nettoyage bateaux changement MMSI
toomanytypes <- mt_all %>% distinct(mmsi, type_name) %>% count(mmsi) %>% filter(n > 1) %>% pull(mmsi)
length(toomanytypes) # 34 boats have multiple mmsi
mt_all %>% filter(mmsi %in% toomanytypes) %>% distinct(mmsi, type_name) %>% arrange(mmsi)



# Spatialisation ----
# Besoin de spatialisation des données de ping pour une future agrégation en maille







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
library("lubridate")
library("openxlsx")

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

## Recherche association codes Resoblo ----
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

ais_resoblo <- ais_resoblo %>%
  filter(!is.na(resoblo_intitule_n2)) %>%
  filter(talassa_conserver) %>%
  select(
    ais_type_summary, 
    type_name, 
    resoblo_intitule_n2, 
    resoblo_code_n2,
    resoblo_intitule_n1,
    resoblo_code_n1
  )

## Nettoyage bateaux sans code RESOBLO ----
# Filter les bateaux de mt_all avec les combinaisons existantes de ais_type_summary et type_name 
# de la référence ais_resoblo

# Sélectionner les combinaisons uniques de type valides dans ais_resoblo
valid_types <- ais_resoblo %>%
  select(ais_type_summary, type_name) %>%
  distinct()# Filtrer mt_all pour garder seulement les bateaux avec des combinaisons valides

mt_all_filtered <- mt_all %>%
  semi_join(valid_types, by = c("ais_type_summary", "type_name"))

# Résumé du filtrage
cat("Nombre de lignes avant filtrage :", nrow(mt_all), "\n")
cat("Nombre de lignes après filtrage :", nrow(mt_all_filtered), "\n")
cat("Nombre de lignes supprimées :", nrow(mt_all) - nrow(mt_all_filtered), "\n")


## Check plusieurs types de bateaux par mmsi
toomanytypes <- mt_all_filtered %>% distinct(mmsi, type_name) %>% count(mmsi) %>% filter(n > 1) %>% pull(mmsi)
length(toomanytypes) # 8 bateaux avec plusieurs types par mmsi
toomanytypes <- mt_all_filtered %>% filter(mmsi %in% toomanytypes) %>% distinct(mmsi, type_name, shipname) %>% arrange(mmsi)
toomanytypes
write.csv(toomanytypes, file = paste0(paths$dev$ais, "toomanytypes_to_check.csv"))

# Check plusieurs noms de bateaux par mmsi
toomanynames <- mt_all_filtered %>% distinct(mmsi, shipname) %>% count(mmsi) %>% filter(n > 1) %>% pull(mmsi)
length(toomanynames) # 26 bateaux ayant des noms différents pour le même mmsi
mt_all_filtered %>% filter(mmsi %in% toomanynames) %>% distinct(mmsi, shipname) %>% arrange(mmsi)


# Intégration codes RESOBLO ----

## Étape 1 : Jointure codes RESOBLO par type_name ----
# Utilisation de reference_codes_ais_resoblo pour jointure des codes RESOBLO
# aux catégories par la variable type_name

mt_all_resoblo <- mt_all_filtered %>%
  left_join(
    ais_resoblo %>% 
      select(ais_type_summary, type_name, resoblo_code_n2, resoblo_intitule_n2, resoblo_code_n1, resoblo_intitule_n1) %>%
      distinct(),
    by = c("ais_type_summary", "type_name")
  )

cat("Après étape 1 : Codes RESOBLO associés par type_name\n")
cat("Nombre de lignes avec codes n2 :", sum(!is.na(mt_all_resoblo$resoblo_code_n2)), "\n")
cat("Nombre de lignes sans codes n2 :", sum(is.na(mt_all_resoblo$resoblo_code_n2)), "\n\n")

## Étape 2 : Override codes RESOBLO par MMSI depuis resoblo_passenger_cargo_review ----
# Ecraser codes RESOBLO pour résoudre le problème des type_name
# qui ne sont pas bien associés à des codes RESOBLO

resoblo_passenger_cargo <- openxlsx::read.xlsx(paste0(paths$dev$ais, "resoblo_passenger_cargo_review.xlsx"))

resoblo_passenger_cargo <- resoblo_passenger_cargo %>% 
  select(mmsi, resoblo_code_n2, resoblo_intitule_n2, resoblo_code_n1, resoblo_intitule_n1) %>%
  # Dédupliquer par MMSI - garder la première occurrence
  distinct(mmsi, .keep_all = TRUE) %>%
  rename_with(~ paste0(., "_override1"), -mmsi) %>%
  mutate(mmsi = as.character(mmsi))

mt_all_resoblo <- mt_all_resoblo %>%
  left_join(., resoblo_passenger_cargo, by = "mmsi") %>%
  mutate(
    resoblo_code_n2 = coalesce(resoblo_code_n2_override1, resoblo_code_n2),
    resoblo_intitule_n2 = coalesce(resoblo_intitule_n2_override1, resoblo_intitule_n2),
    resoblo_code_n1 = coalesce(resoblo_code_n1_override1, resoblo_code_n1),
    resoblo_intitule_n1 = coalesce(resoblo_intitule_n1_override1, resoblo_intitule_n1)
  ) %>%
  select(-ends_with("_override1"))

## Étape 3 : Override codes RESOBLO par MMSI depuis toomanytypes_to_check ----
# Ecraser codes RESOBLO pour éviter le problème des type_name multiples
# pour un même MMSI

toomanytypes_review <- openxlsx::read.xlsx(paste0(paths$dev$ais, "toomanytypes_corrected.xlsx"))

toomanytypes_review <- toomanytypes_review %>%
  select(mmsi, resoblo_code_n2, resoblo_intitule_n2, resoblo_code_n1, resoblo_intitule_n1) %>%
  # Dédupliquer par MMSI - garder la première occurrence
  distinct(mmsi, .keep_all = TRUE) %>%
  rename_with(~ paste0(., "_override2"), -mmsi) %>%
  mutate(mmsi = as.character(mmsi))

mt_all_resoblo <- mt_all_resoblo %>%
  left_join(toomanytypes_review, by = "mmsi") %>%
  mutate(
    resoblo_code_n2 = coalesce(resoblo_code_n2_override2, resoblo_code_n2),
    resoblo_intitule_n2 = coalesce(resoblo_intitule_n2_override2, resoblo_intitule_n2),
    resoblo_code_n1 = coalesce(resoblo_code_n1_override2, resoblo_code_n1),
    resoblo_intitule_n1 = coalesce(resoblo_intitule_n1_override2, resoblo_intitule_n1)
  ) %>%
  select(-ends_with("_override2"))


# Check unicité types
names(mt_all_resoblo)
dim(mt_all_resoblo)

check_resoblo <- mt_all_resoblo %>%
  distinct(mmsi, ais_type_summary, type_name, resoblo_intitule_n2, resoblo_code_n2, resoblo_intitule_n1, resoblo_code_n1) %>%
  count(ais_type_summary, type_name, resoblo_intitule_n2, resoblo_code_n2, resoblo_intitule_n1, resoblo_code_n1) %>%
  arrange(ais_type_summary)

View(check_resoblo)


# Corrections formats ----

# Selection colonnes utiles
mt_resoblo <- mt_all_resoblo %>%
  select(
    mmsi, 
    lat, 
    lon, 
    heading,
    status, 
    timestamp, 
    flag, 
    length, 
    width, 
    draught,
    ship_country,
    destination, 
    avg_speed, 
    max_speed, 
    speed, 
    contains("resoblo")
  )

mt_resoblo <- mt_resoblo %>%
  mutate(
    across(
      c(mmsi, lat, lon, heading, length, width, avg_speed, max_speed, speed), 
      function(x) {as.numeric(x)}),
    timestamp = lubridate::ymd_hms(as.character(timestamp), tz = "UTC"),
    date = as.Date(timestamp),
    time = format(timestamp, "%H:%M:%S")
  )

str(mt_resoblo)


# Spatialisation ----
# Besoin de spatialisation des données de ping pour une future agrégation en maille
mt_resoblo <- st_as_sf(mt_resoblo, coords = c("lon", "lat"), crs = 4326)

dim(mt_resoblo)

st_write(
  obj = mt_resoblo, 
  dsn = paths$processed$observatoire_ais,
  driver = "GPKG",
  append = FALSE
)



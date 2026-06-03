#' ---
#' title : "talassaR - maillage_talassa_ais"
#' author : Aubin Woehrel
#' creation date : 2026-06-03
#' ---
#'
#' =============================================================================
#'
#' talassaR : Maillage des données AIS
#'
#' Description :
#' Script de préparation et d'agrégation des pings AIS sur le carroyage TALASSA.
#'
#' =============================================================================

#' ---
#' title : "talassaR - maillage_talassa_ais"
#' author : Aubin Woehrel
#' creation date : 2026-06-03
#' ---
#'
#' =============================================================================
#'
#' talassaR : Maillage des données AIS
#'
#' Description :
#' Script de calcul de l'état de mouvement des pings AIS et agrégation
#' sur le carroyage TALASSA avec distinction transit vs ancrage.
#'
#' Workflow:
#' 1) Charger AIS formaté (talassa_code/intitule déjà assignés)
#' 2) Calculer état de mouvement (transit vs stationnaire)
#' 3) Recoder l'activité : transit -> keep code / stationnaire -> ancrage
#' 4) Agréger par maille et activité
#'
#' =============================================================================

## Nettoyage ----
rm(list = ls())

## Import des librairies ----
library("dplyr")
library("sf")
library("lubridate")
library("yaml")

## Ressources locales ----
paths <- yaml::read_yaml("config/paths.yml")
source("r/fct_ais_grid.R")
source("r/fct_jointure_id.R")

## Paramètres ----
speed_threshold <- 2        # knots: vitesse en dessous -> stationnaire
distance_threshold <- 200   # metres: distance en dessous -> stationnaire
value_method <- "unique_vessels"  # ou "pings"
ancrage_code <- "recz_00_f00_a03"
ancrage_intitule <- "ancrage"

## Chargement des données ----
carroyage_hex <- st_read(paths$raw$carroyage_hexcinquieme, quiet = TRUE) %>%
  st_transform(crs = 4326)

ais_talassa <- st_read(paths$processed$talassa_ais, quiet = TRUE)

## Calcul état de mouvement ----
ais_movement <- compute_ais_movement_state(
  ais = ais_talassa,
  speed_field = "speed",
  time_field = "timestamp",
  speed_threshold = speed_threshold,
  distance_threshold = distance_threshold
)

# Résumé mouvement
cat("\nRésumé états de mouvement:\n")
print(ais_movement %>%
  st_drop_geometry() %>%
  count(movement_state, name = "n_pings"))


## Recodage activité par mouvement ----
ais_recoded <- recode_ais_activity_by_movement(
  ais = ais_movement,
  ancrage_code = ancrage_code,
  ancrage_intitule = ancrage_intitule
)

check_recoding <- ais_recoded %>%
  st_drop_geometry() %>%
  count(talassa_code, talassa_code_final, talassa_intitule, talassa_intitule_final) %>%
  arrange(talassa_intitule_final, talassa_code)
View(check_recoding)

ais_recoded <- ais_recoded %>%
  mutate(
    talassa_code = talassa_code_final,
    talassa_intitule = talassa_intitule_final
  ) %>%
  select(-talassa_code_final, -talassa_intitule_final)


## Agrégation à la maille ----
ais_grid <- aggregate_ais_to_grid(
  ais = ais_recoded,
  carroyage = carroyage_hex,
  id_name = "id_hex",
  activity_code_field = "talassa_code",
  activity_intitule_field = "talassa_intitule",
  value_method = value_method
)
 
# Sauvegarde du résultat AIS en grille pour intégration ultérieure
ais_grid <- ais_grid %>%
  rename(id_hex = id)


cat("\nNombre de combinaisons maille-activité :", nrow(ais_grid), "\n")

## Résumé final ----
if (exists("ais_grid")) {
  cat("\nStructure du gridded output :\n")
  print(glimpse(ais_grid))
  
  cat("\nActivités par code (top 15):\n")
  print(ais_grid %>%
    count(talassa_code, sort = TRUE) %>%
    head(15))
}


# Version wide explorable sur qgis avec colonnes par activité : 
ais_grid_wide <- ais_grid %>%
  select(id_hex, talassa_intitule, activity_value, geom) %>%
  pivot_wider(
    names_from = talassa_intitule, 
    values_from = activity_value
  )

# Version longue
st_write(
  obj = ais_grid,
  dsn = paths$processed$talassa_ais_grid,
  driver = "GPKG",
  delete_dsn = TRUE,
  append = FALSE
)

# Version wide 
st_write(
  obj = ais_grid_wide,
  dsn = paths$processed$talassa_ais_grid_wide,
  driver = "GPKG",
  delete_dsn = TRUE,
  append = FALSE
)

cat("\nAIS grid sauvegardé :", paths$processed$talassa_ais_grid, "\n")


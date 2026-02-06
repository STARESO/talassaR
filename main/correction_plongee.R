#' ---
#' title : "talassaR - correction_plongee"
#' author : Aubin Woehrel
#' creation date : 2026-02-06
#' ---
#'
#' =============================================================================
#'
#' talassaR : Correction des données sites de plongée
#'
#' Description :
#' Script de correction des données de sites de plongée. Etapes réalisées :
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

# Data: Spatial
library("sf")
library("leaflet")
library("rlang")

## Ressources locales ----
source("r/paths.R")

## Import des données ----

# Données source plongée
plongee_2023 <- sf::st_read(paths$raw_plongee_2023)
plongee_2025 <- read.csv2(paths$raw_plongee_2025)

# Délimitation spatiale du Parc
pnm_borders <- sf::st_read(paths$raw_pnmcca_borders) %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(NOM_SITE == "cap Corse et Agriate")

# Check initial ----
st_crs(plongee_2023) # WGS84 EPSG 4326
skimr::skim(plongee_2023)
skimr::skim(plongee_2025)
# View(plongee_2023)
# View(plongee_2025)

# Corrections et transformations ----

# Amélioration colonnes
plongee_2025 <- plongee_2025 %>%
  rename_with(., str_to_lower) %>%
  select(-id) %>%
  rename(id_prest = id_1, prestataire = prestatair) %>%
  relocate(id_prest, .before = prestataire)

# Changement vers numérique pour coordonnées
plongee_2025 <- plongee_2025 %>%
  mutate(across(c(y_lat, x_lon), as.numeric))

# Check des sites uniques
sort(unique(plongee_2025$nom_site))

# Modification des sites uniques réalisée sous excel puis export csv
# Le script est relancé à partir de cette étape

# Check des unicités des ids & noms de structures
check_idprest <- plongee_2025 %>%
  select(id_prest, prestataire) %>%
  group_by(pick(everything())) %>%
  summarize(n = n()) %>%
  arrange(id_prest, prestataire)

View(check_idprest)

# Association codes resoblo ----

# Check uniformité intitulés données sources
unique(plongee_2025$act_n1)

# Transfo nom de colonne
plongee_2025 <- plongee_2025 %>%
  rename(resoblo_intitule_n1 = act_n1) %>%
  mutate(resoblo_intitule_n1 = str_to_lower(resoblo_intitule_n1))

# Map RESOBLO
map_resoblo <- data.frame(
  resoblo_intitule_n1 = c("plongee scaphandre", "plongee en apnee"),
  resoblo_code_n1 = c("RECM.01.F01.A03", "RECM.01.F02.A01")
)

# Jointure
plongee_2025 <- left_join(
  x = plongee_2025,
  y = map_resoblo,
  by = join_by(resoblo_intitule_n1)
)

plongee_2025 <- plongee_2025 %>%
  relocate(resoblo_code_n1, .after = resoblo_intitule_n1) %>%
  relocate(x_lon, .before = y_lat)

View(plongee_2025)

# Nettoyage espaces début et fin non visibles au cas où ----
plongee_2025 <- plongee_2025 %>%
  mutate(across(where(is.numeric), str_trim))


# Transformation au format spatial ----

# Infos entités sans coordonnées
no_coord <- plongee_2025 %>%
  filter(is.na(x_lon) | is.na(y_lat))

# Transfo vers format spatial sf
plongee_spatial_2025 <- plongee_2025 %>%
  filter(!is.na(x_lon) & !is.na(y_lat)) %>%
  st_as_sf(., coords = c("x_lon", "y_lat"), crs = st_crs(4326))

# Carte de visualisation
map_optimized <- leaflet(plongee_spatial_2025) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(data = pnm_borders, color = "lightblue", weight = 10) %>%
  addCircleMarkers(
    radius = 4,
    stroke = FALSE,
    fillOpacity = 1,
    color = colors,
    popup = ~ paste(
      "Nom site :", nom_site, "<br>",
      "Prestataire :", prestataire, "<br>",
      "Intitulé RESOBLO :", resoblo_intitule_n1, "<br>",
      "Code RESOBLO :", resoblo_code_n1, "<br>"
    )
  )

map_optimized

# Export ----
st_write(
  obj = plongee_spatial_2025,
  dsn = paths$processed_obs_plongee,
  driver = "GPKG",
  append = FALSE
)

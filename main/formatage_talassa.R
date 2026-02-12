#' ---
#' title : "talassaR - formatage_talassa"
#' author : Aubin Woehrel
#' creation date : 2026-02-06
#' ---
#'
#' =============================================================================
#'
#' talassaR : Formatage des données observatoire en format TALASSA
#'
#' Description :
#' Script permettant de passer les jeux de données corrigés au format
#' observatoire en jeux de données au format TALASSA.
#' Consiste à :
#' 1) Eliminer les colonnes inutiles
#' 2) Uniformiser certaines colonnes si besoin
#' 3) Remplacer les codes et intitulés RESOBLO par les codes TALASSA
#' 4) Exporter au format gpkg (avec crs 4326)
#'
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
library("openxlsx")

# Data: Spatial
library("sf")
library("leaflet")
library("rlang")

## Ressources locales ----
source("r/paths.R")
source("r/fct_category_map.R")

## Import des données ----

# Référence codes RESOBLO-TALASSA
codes_talassa <- read.xlsx(
  xlsxFile = paths$raw_codes_talassa,
  sheet = "codes",
  fillMergedCells = TRUE,
  startRow = 2
)

# Activités format observatoire corrigé
survolusage_obs <- st_read(paths$processed_obs_survolusage)
peche_obs <- st_read(paths$processed_obs_peche)
donia_obs <- st_read(paths$processed_obs_donia)
plongee_obs <- st_read(paths$processed_obs_plongee)

# Délimitation PNMCCA
pnm_borders <- sf::st_read(paths$raw_pnmcca_borders) %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(NOM_SITE == "cap Corse et Agriate")

# Transformations initiales ----
codes_talassa <- codes_talassa %>%
  select(resoblo_intitule_n1:talassa_commentaires) %>%
  select(-resoblo_precision_n0) %>%
  filter(!is.na(talassa_code))

View(codes_talassa)


# Modification activites ----

## Modification survols usages ----


## Modification survols plaba ----


## Modification plongee ----
plongee_bool <- unique(plongee_obs$resoblo_code_n1) %in% codes_talassa$code_resoblo_plus_proche

if (sum(!plongee_bool) != 0) {
  simpleWarning("Codes Talassa plongée non valides, données non enregistrées.")
}

plongee_talassa <- codes_talassa %>%
  select(code_resoblo_plus_proche, talassa_code, talassa_intitule) %>%
  left_join(plongee_obs, ., by = join_by(resoblo_code_n1 == code_resoblo_plus_proche))

plongee_talassa <- plongee_talassa %>%
  select(-c(resoblo_intitule_n1, resoblo_code_n1))


## Modification peche ----


## Modification donia ----

# map_taille <- category_map(donia_obs, "taille")
# map_taille

# map_region <- category_map(donia_obs, "region")
# map_region

# Check bateaux sans taille
donia_obs %>%
  filter(is.na(taille)) %>%
  dim(.) # 195 entités sans taille

# Elimination des bateaux sans taille
donia_obs <- donia_obs %>%
  filter(!is.na(taille))

# Check presence
donia_bool <- unique(donia_obs$resoblo_code) %in% codes_talassa$code_resoblo_plus_proche


if (sum(!donia_bool) != 0) {
  simpleWarning("Codes Talassa Donia non valides, données non enregistrées.")

  check_donia <- donia_obs %>%
    st_drop_geometry() %>%
    filter(resoblo_code %in% unique(donia_obs$resoblo_code)[!donia_bool]) %>%
    select(resoblo_intitule, resoblo_code, resoblo_niveau) %>%
    distinct()

  View(check_donia)
}

# Modification habitats ----


# Exports ----

# Sites plongée
st_write(
  obj = plongee_talassa,
  dsn = paths$processed_tal_pts_plongee,
  driver = "GPKG",
  append = FALSE
)

st_write()

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

## Import des données ----

# Référence codes RESOBLO-TALASSA
codes_talassa <- read.xlsx(
  xlsxFile = paths$raw_codes_talassa,
  sheet = "codes",
  fillMergedCells = TRUE,
  startRow = 2
)

# Activités
plongee_obs <- st_read(paths$processed_obs_plongee)


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
unique(plongee_obs$resoblo_code_n1) %in% codes_talassa$code_resoblo_plus_proche

plongee_talassa <- codes_talassa %>%
  select(code_resoblo_plus_proche, talassa_code, talassa_intitule) %>%
  left_join(plongee_obs, ., by = join_by(resoblo_code_n1 == code_resoblo_plus_proche))

plongee_talassa <- plongee_talassa %>%
  select(-c(resoblo_intitule_n1, resoblo_code_n1))

## Modification peche ----
## Modification donia ----


# Modification habitats ----


# Exports ----

# Sites plongée
st_write(
  obj = plongee_talassa,
  dsn = paths$processed_tal_pts_plongee,
  driver = "GPKG",
  append = FALSE
)

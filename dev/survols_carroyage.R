#' ---
#' title : "talassaR - survols_carroyage"
#' author : Aubin Woehrel
#' creation date : 2026-02-03
#' ---
#'
#' =============================================================================
#'
#' talassaR :
#' Survols carroyage
#'
#' Description :
#' Mise en carroyage des données ponctuelles de survols aériens. Utilisé
#' principalement pour le carroyage hexagonal d'1/5 de mile. Transformation
#' générale des données pour correspondre aux exigences du format TALASSA
#'
#' =============================================================================

# Initialization ----

## Clean up and working directory ----
rm(list = ls())

## Library imports ----

# Data import and tidying
library("readr")
library("dplyr")
library("tidyr")
library("stringr")
library("purrr")

# Data exploration
library("skimr")

# Data representations
library("ggplot2")

# Spatial
library("sf")

## Sourcing local resources ----
paths <- yaml::read_yaml("config/paths.yml")


# Import des données ----

# Donnees ponctuelles survols
survols_pts <- st_read(paste0(
  paths$processed$survols_corrected,
  "us_med_pnmcca_observatoire_survols_usages_ofb_pt_4326.gpkg"
))

# Donnees carroyage
hex_cinquieme <- st_read(paste0(
  paths$raw$carroyage,
  "grille_talassa_2025_cotier_hexagone_cinquiemedemile.shp"
))

# Check du CRS
sf::st_crs(survols_pts) # 4326 WGS
sf::st_crs(hex_cinquieme) # 2154 Lambert

# Structure générale
skimr::skim(survols_pts)

# Transformation format talassa ----
survols_talassa_pts <- survols_pts %>%
  select(
    date,
    annee,
    mois,
    jour,
    resoblo_code,
    resoblo_intitule,
    taille_nav,
    etat_nav,
    geom
  ) %>%
  mutate(resoblo_code = str_to_snake(resoblo_code))


# Test à enlever
test <- survols_pts %>%
  sf::st_drop_geometry() %>%
  group_by(resoblo_code, resoblo_intitule, etat_nav, act, nom_acti) %>%
  summarize(n = n()) %>%
  arrange(etat_nav, resoblo_code)
View(test)

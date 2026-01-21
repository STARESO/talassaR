#' ---
#' title : "talassaR - peche verification"
#' author : Aubin Woehrel
#' creation date : 2026-01-20
#' ---
#'
#' =============================================================================
#'
#' talassaR : Peche verification
#'
#' Description :
#' Script takes care of checking the overview of the leisure fishing data used
#' in the talassa project. General verification process, but dataset should
#' be already clean thanks to the previous use of the dataset by Jonathan
#' Richir in another contract with the PNMCCA
#'
#' =============================================================================
#'
#' # Initialization ----
#'
#' ## Clean up and working directory ----
rm(list = ls())

## Library imports ----

# Data import and tidying
library("readr")
library("dplyr")
library("tidyr")
library("stringr")

# Spatial
library("sf")
library("leaflet")

## Sourcing local resources ----
source("r/paths.R")
source("r/fct_category_map.R") # Custom data spatial map function


# Importing data ----

# Peche de loisir
peche_loisir <- sf::st_read(paths$raw_peche_shp) %>%
  sf::st_transform(crs = 4326)

peche_loisir_gpkg <- sf::st_read(paths$raw_peche_gpkg) %>%
  sf::st_transform(crs = 4326)

peche_loisir_Quentin <- sf::st_read(paths$raw_peche_quentin) %>%
  sf::st_transform(crs = 4326)

# PNMCCA Border
pnm_borders <- sf::st_read(paths$raw_pnmcca_borders) %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(NOM_SITE == "cap Corse et Agriate")


# Visual investigation plots

str(peche_loisir)
str(peche_loisir_gpkg)
str(peche_loisir_Quentin) # Not exactly all the same variable names

peche_loisir_Quentin <- peche_loisir_Quentin %>%
  rename_with(stringr::str_to_lower) %>%
  rename_with(function(x) {
    str_replace_all(x, "\\.", "_")
  }) %>%
  rename(mod_pech = mod_peche)

map_peche <- function(data_peche, type_peche = "all") {
  # Case when only a type of fishing is selected
  if (type_peche != "all") {
    data_peche <- data_peche %>%
      filter(mod_pech == type_peche)
  }

  map_peche_selected <- data_peche %>%
    leaflet(.) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addPolygons(data = pnm_borders, color = "lightblue", weight = 10)

  if ("bd" %in% names(data_peche)) { # Quentin treated data
    map_peche_selected <- map_peche_selected %>%
      addCircleMarkers(
        radius = 4,
        stroke = FALSE,
        fillOpacity = 1,
        color = colors, # Use the precomputed colors (red for NA)
        popup = ~ paste(
          "Source Données:", bd, "<br>",
          "ID sortie :", id_obs, "<br>",
          "Type de pêche", mod_pech, "<br>"
        )
      )
  } else { # PNMCCA og data
    map_peche_selected <- map_peche_selected %>%
      addCircleMarkers(
        radius = 4,
        stroke = FALSE,
        fillOpacity = 1,
        color = colors, # Use the precomputed colors (red for NA)
        popup = ~ paste(
          "ID sortie :", id_obs, "<br>",
          "Type de pêche", mod_pech, "<br>"
        )
      )
  }
  map_peche_selected
}

# All data
map_peche(peche_loisir)
map_peche(peche_loisir_gpkg)
map_peche(peche_loisir_Quentin)

# Map of og fishing dataset by type
map_peche(data_peche = peche_loisir, type_peche = "po")
map_peche(data_peche = peche_loisir, type_peche = "pe")
map_peche(data_peche = peche_loisir, type_peche = "pdb")
map_peche(data_peche = peche_loisir, type_peche = "csm")

# Map of corrected dataset (Quentin 2025) per type
map_peche(data_peche = peche_loisir_Quentin, type_peche = "po")
map_peche(data_peche = peche_loisir_Quentin, type_peche = "pe")
map_peche(data_peche = peche_loisir_Quentin, type_peche = "pdb")
map_peche(data_peche = peche_loisir_Quentin, type_peche = "csm")

# General variable investigation ----
View(peche_loisir)

sort(unique(peche_loisir$id_obs))
unique(peche_loisir$mod_pech) # All levels are ok

# Testing unicity of identifiers : which are specific to each survey ?
test <- peche_loisir_Quentin %>%
  as.data.frame() %>%
  select(fiche_n, id_sortie, id_obs, bd) %>%
  distinct() %>%
  arrange(bd, fiche_n)

View(test)

test2 <- test %>%
  group_by(id_obs) %>%
  summarize(n = n())

View(test2)

# Unicity of each survey is the combination of fiche_n
# and the origin dataset (column bd)


# Cheking total amount of enquêtes for mod_pech = pdb before elimination
count_pdb <- peche_loisir %>%
  as.data.frame() %>%
  filter(mod_pech == "pdb") %>%
  select(id_obs, saisie) %>%
  distinct() %>%
  group_by(saisie) %>%
  mutate(n = n())

View(count_pdb)

# Preparing for grid aggregation ----
test <- peche_loisir_Quentin %>%
  group_by(bd, fiche_n) %>%
  summarise()

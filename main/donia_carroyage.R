#' ---
#' title : "talassaR - donia carroyage"
#' author : Aubin Woehrel
#' creation date : 2025-10-21
#' last modification : 2025-10-23
#' ---
#'
#' =============================================================================
#'
#' talassaR :
#' Donia carroyage
#'
#' Description :
#' Aggregating data of Donia in the cells of different types of spatial grids
#' build for the TALASSA project.
#'
#' =============================================================================


# Initialization ----

## Clean up and working directory ----
rm(list = ls())

## Libraries ----

# Data import and tidying
library("dplyr")
library("tidyr")
library("stringr")

# Spatial
library("sf")
library("leaflet")
library("rlang")

## Sourcing local resources ----
source("r/paths.R")
source("r/fct_aggregate_to_grid.R")

# Importing data ----
donia_talassa  <- st_read(paths$processed_donia_points)

# Carroyage data
hex_demi  <- st_read(paste0(paths$raw_carroyage, "grille_talassa_2025_cotier_hexagone_demimile.shp"))
hex_quart <- st_read(paste0(paths$raw_carroyage, "grille_talassa_2025_cotier_hexagone_quartdemile.shp"))
hex_cinquieme <- st_read(paste0(paths$raw_carroyage, "grille_talassa_2025_cotier_hexagone_cinquiemedemile.shp"))
hex_dizieme <- st_read(paste0(paths$raw_carroyage, "grille_talassa_2025_cotier_hexagone_diziemedemile.shp"))
rect_demi <- st_read(paste0(paths$raw_carroyage, "grille_talassa_2025_cotier_rectangle_demimile.shp"))
rect_quart <- st_read(paste0(paths$raw_carroyage, "grille_talassa_2025_cotier_rectangle_quartdemile.shp"))
rect_cinquieme <- st_read(paste0(paths$raw_carroyage, "grille_talassa_2025_cotier_rectangle_cinquiemedemile.shp"))
rect_carpediem <- st_read(paste0(paths$raw_carroyage, "grille_carpediem_1m_2019_pnmcca_cotier.shp"))

# Aggregation ----

## Hexagonal exports ----

# Weights : ln (log) for dr_mllg
hex_demi_lnduree <- aggregate_to_grid(
  grid_layer = hex_demi,
  transfo = "ln_mouillage",
  id_field   = "id",
  output     = paste0(paths$processed_donia_hex, "hex_demi_donia_lnduree.gpkg"),
  layer_name = "hex_demi_donia_lnduree",
  overwrite  = TRUE
)

hex_quart_lnduree <- aggregate_to_grid(
  grid_layer = hex_quart,
  transfo = "ln_mouillage",
  id_field   = "id",
  output     = paste0(paths$processed_donia_hex, "hex_quart_donia_lnduree.gpkg"),
  layer_name = "hex_quart_donia_lnduree",
  overwrite  = TRUE
)

hex_cinquieme_lnduree <- aggregate_to_grid(
  grid_layer = hex_cinquieme,
  transfo = "ln_mouillage",
  id_field   = "id",
  output     = paste0(paths$processed_donia_hex, "hex_cinquieme_donia_lnduree.gpkg"),
  layer_name = "hex_cinquieme_donia_lnduree",
  overwrite  = TRUE
)

hex_dizieme_lnduree <- aggregate_to_grid(
  grid_layer = hex_dizieme,
  id_field   = "id",
  output     = paste0(paths$processed_donia_hex, "hex_dizieme_donia_lnduree.gpkg"),
  layer_name = "hex_dizieme_donia_lnduree",
  overwrite  = TRUE
)

# Weight : ln (log) for both taille and dr_mllg
hex_demi_lnall <- aggregate_to_grid(
  grid_layer = hex_demi,
  transfo = "ln_all",
  id_field   = "id",
  output     = paste0(paths$processed_donia_hex, "hex_demi_donia_lnall.gpkg"),
  layer_name = "hex_demi_donia_lnall",
  overwrite  = TRUE
)

hex_quart_lnall <- aggregate_to_grid(
  grid_layer = hex_quart,
  transfo = "ln_all",
  id_field   = "id",
  output     = paste0(paths$processed_donia_hex, "hex_quart_donia_lnall.gpkg"),
  layer_name = "hex_quart_donia_lnall",
  overwrite  = TRUE
)

hex_cinquieme_lnall <- aggregate_to_grid(
  grid_layer = hex_cinquieme,
  transfo = "ln_all",
  id_field   = "id",
  output     = paste0(paths$processed_donia_hex, "hex_cinquieme_donia_lnall.gpkg"),
  layer_name = "hex_cinquieme_donia_lnall",
  overwrite  = TRUE
)

hex_dizieme_lnall <- aggregate_to_grid(
  grid_layer = hex_dizieme,
  transfo = "ln_all",
  id_field   = "id",
  output     = paste0(paths$processed_donia_hex, "hex_dizieme_donia_lnall.gpkg"),
  layer_name = "hex_dizieme_donia_lnall",
  overwrite  = TRUE
)

# Note : transfo "sqrt_ln" and "cubert_ln" exist and can be used if necessary

# Rectangular exports ----

# Weight : ln (log) for both taille and dr_mllg

rect_carpediem_lnall <- rect_carpediem %>%
  rename(id = fid) %>%
  aggregate_to_grid(
    grid_layer = .,
    transfo = "ln_all",
    id_field   = "id",
    output     = paste0(paths$processed_donia_rect, "rect_carpediem_donia_lnall.gpkg"),
    layer_name = "rect_carpediem_donia_lnall",
    overwrite  = TRUE
  )

rect_demi_lnall <- aggregate_to_grid(
  grid_layer = rect_demi,
  transfo = "ln_all",
  id_field   = "id",
  output     = paste0(paths$processed_donia_rect, "rect_demi_donia_lnall.gpkg"),
  layer_name = "rect_demi_donia_lnall",
  overwrite  = TRUE
)

rect_quart_lnall <- aggregate_to_grid(
  grid_layer = rect_quart,
  transfo = "ln_all",
  id_field   = "id",
  output     = paste0(paths$processed_donia_rect, "rect_quart_donia_lnall.gpkg"),
  layer_name = "rect_quart_donia_lnall",
  overwrite  = TRUE
)

rect_cinquieme_lnall <- aggregate_to_grid(
  grid_layer = rect_cinquieme,
  transfo = "ln_all",
  id_field   = "id",
  output     = paste0(paths$processed_donia_rect, "rect_cinquieme_donia_lnall.gpkg"),
  layer_name = "rect_cinquieme_donia_lnall",
  overwrite  = TRUE
)

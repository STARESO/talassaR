#' ---
#' title : "talassaR - donia carroyage"
#' author : Aubin Woehrel
#' creation date : 2025-10-16
#' ---
#'
#' =============================================================================
#' 
#' talassaR : 
#' Donia carroyage
#' 
#' Description : 
#' Aggregating data of donia in the cells of different types of spatial grids 
#' build for the TALASSA project.
#' 
#' =============================================================================


# Initialization ----

## Clean up and working directory ----
rm(list = ls())

## Library imports ----

# Data import and tidying
library("dplyr")
library("tidyr")
library("stringr")

# Spatial 
library("sf")
library("leaflet")
library("rlang")

## Sourcing paths and constants ----
# source("R/paths.R")


# Importing data ----
donia_talassa  <- st_read("data/processed/donia_talassa/donia_talassa.shp")
# plot(donia_talassa)


# Carroyage data
carroyage_path <- "data/raw/carroyage/zone_biocenoses/"


hex_demi  <- st_read(
  paste0(carroyage_path, "grille_talassa_2025_cotier_hexagone_demimile.shp")
)  

hex_quart <- st_read(
  paste0(carroyage_path, "grille_talassa_2025_cotier_hexagone_quartdemile.shp")
)  

hex_cinquieme   <- st_read(
  paste0(carroyage_path, "grille_talassa_2025_cotier_hexagone_cinquiemedemile.shp")
)  

hex_dizieme <- st_read(
  paste0(carroyage_path, "grille_talassa_2025_cotier_hexagone_diziemedemile.shp")
)  

rect_demi <- st_read(
  paste0(carroyage_path, "grille_talassa_2025_cotier_rectangle_demimile.shp")
) 

rect_quart <- st_read(
  paste0(carroyage_path, "grille_talassa_2025_cotier_rectangle_quartdemile.shp")
)  

rect_cinquieme <- st_read(
  paste0(carroyage_path, "grille_talassa_2025_cotier_rectangle_cinquiemedemile.shp")
)  

rect_carpediem <- st_read(
  paste0(carroyage_path, "grille_carpediem_1m_2019_pnmcca_cotier.shp")
)  


# Aggregation ----

## Aggregating function ----
aggregate_donia_to_grid <- function(
    grid_layer,
    transfo = "ln_mouillage",
    id_field   = "id",
    output     = NULL,          # e.g. "data/processed/donia_talassa/hex_donia/out.gpkg"
    layer_name = "hex_donia",
    overwrite  = TRUE
) {
  stopifnot(inherits(grid_layer, "sf"))
  if (is.na(st_crs(grid_layer))) stop("grid_layer has no CRS.")
  
  # 1) Bring DONIA points to the grid CRS
  pts <- donia_talassa %>% st_transform(st_crs(grid_layer))
  
  # 2) Spatial join to attach grid id to points
  pts_j <- pts %>%
    st_join(grid_layer %>% select(all_of(id_field)), join = st_intersects, left = TRUE)
  
  # 3) Weighted value per point
  if (transfo == "ln_mouillage") {
    base <- pts_j %>%
      st_drop_geometry() %>%
      mutate(
        w = if_else(
          !is.na(taille) & !is.na(dr_mllg) & dr_mllg > 0,
          taille * log(dr_mllg), 
          NA_real_
        )
      )
  } else if (transfo == "ln_all") {
    base <- pts_j %>%
      st_drop_geometry() %>%
      mutate(
        w = if_else(
          !is.na(taille) & !is.na(dr_mllg) & dr_mllg > 0,
          log(taille) * log(dr_mllg), 
          NA_real_
        )
      )
  } else if (transfo == "sqrt_ln") {
    base <- pts_j %>%
      st_drop_geometry() %>%
      mutate(
        w = if_else(
          !is.na(taille) & !is.na(dr_mllg) & dr_mllg > 0,
          sqrt(taille) * log(dr_mllg), 
          NA_real_
        )
      )
  } else if (transfo == "cubert_ln") {
    base <- pts_j %>%
      st_drop_geometry() %>%
      mutate(
        w = if_else(
          !is.na(taille) & !is.na(dr_mllg) & dr_mllg > 0,
          (taille ^ (1/3)) * log(dr_mllg), 
          NA_real_
        )
      )
  }
  
  
  # 4) Total per cell
  agg_tot <- base %>%
    group_by(.data[[id_field]]) %>%
    summarise(act_all = sum(w, na.rm = TRUE), .groups = "drop")
  
  # 5) Per-category per cell, pivot wide
  agg_wide <- base %>%
    group_by(.data[[id_field]], rsbl_nt) %>%
    summarise(act_sum = sum(w, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from  = rsbl_nt,
      values_from = act_sum,
      values_fill = 0
    )
  
  # 6) Merge totals, tidy
  agg <- agg_wide %>%
    left_join(agg_tot, by = id_field) %>%
    relocate(act_all, .after = all_of(id_field))
  
  # 7) Shorter names (applies only to columns that exist)
  rename_map <- c(
    ferry     = "transport de passagers ferry",
    moteur    = "plaisance a moteur",
    marchand  = "transport maritime de marchandises",
    ravitalr  = "transport par ravitailleur",
    voile     = "plaisance a voile",
    yacht     = "grande plaisance",
    depollu   = "activite de depollution",
    cargo     = "transport par cargo",
    croisr    = "croisere sur paquebot",
    tankr     = "transport par tanker",
    portur    = "circulation des navires professionnels dans les ports",
    navette   = "transport de passagers navette ou high speed craft",
    passagers = "transport maritime de passagers",
    svtage    = "recherche et sauvetage en mer",
    plongee   = "plongee avec assistance respiratoire",
    science   = "activite de recherche scientifique en mer",
    remorqg   = "activite de remorquage"
  )
  
  agg <- agg %>% 
    rename(!!!rename_map)
  
  # 8) Join back to grid
  grid_out <- grid_layer %>%
    left_join(., agg, by = id_field) %>%
    mutate(across(everything(), ~ replace_na(., 0)))
  
  # 9) Optional write
  if (!is.null(output)) {
    st_write(grid_out, output, layer = layer_name, delete_dsn = overwrite)
  }
  
  grid_out
}

## Hexagonal exports ----

# Weights : ln (log) for dr_mllg
hex_demi_lnduree <- aggregate_donia_to_grid(
  grid_layer = hex_demi,
  id_field   = "id",
  output     = "data/processed/donia_talassa/hex_donia/hex_demi_donia_lnduree.gpkg",
  layer_name = "hex_demi_donia_lnduree",
  overwrite  = TRUE
)

hex_quart_lnduree <- aggregate_donia_to_grid(
  grid_layer = hex_quart,
  id_field   = "id",
  output     = "data/processed/donia_talassa/hex_donia/hex_quart_donia_lnduree.gpkg",
  layer_name = "hex_quart_donia_lnduree",
  overwrite  = TRUE
)

hex_cinquieme_lnduree <- aggregate_donia_to_grid(
  grid_layer = hex_cinquieme,
  id_field   = "id",
  output     = "data/processed/donia_talassa/hex_donia/hex_cinquieme_donia_lnduree.gpkg",
  layer_name = "hex_cinquieme_donia_lnduree",
  overwrite  = TRUE
)

hex_dizieme_lnduree <- aggregate_donia_to_grid(
  grid_layer = hex_dizieme,
  id_field   = "id",
  output     = "data/processed/donia_talassa/hex_donia/hex_dizieme_donia_lnduree.gpkg",
  layer_name = "hex_dizieme_donia_lnduree",
  overwrite  = TRUE
)


# Weight : ln (log) for both taille and dr_mllg
hex_demi_lnall <- aggregate_donia_to_grid(
  grid_layer = hex_demi,
  transfo = "ln_all",
  id_field   = "id",
  output     = "data/processed/donia_talassa/hex_donia/hex_demi_donia_lnall.gpkg",
  layer_name = "hex_demi_donia_lnall",
  overwrite  = TRUE
)

hex_quart_lnall <- aggregate_donia_to_grid(
  grid_layer = hex_quart,
  transfo = "ln_all",
  id_field   = "id",
  output     = "data/processed/donia_talassa/hex_donia/hex_quart_donia_lnall.gpkg",
  layer_name = "hex_quart_donia_lnall",
  overwrite  = TRUE
)

hex_cinquieme_lnall <- aggregate_donia_to_grid(
  grid_layer = hex_cinquieme,
  transfo = "ln_all",
  id_field   = "id",
  output     = "data/processed/donia_talassa/hex_donia/hex_cinquieme_donia_lnall.gpkg",
  layer_name = "hex_cinquieme_donia_lnall",
  overwrite  = TRUE
)

hex_dizieme_lnall <- aggregate_donia_to_grid(
  grid_layer = hex_dizieme,
  transfo = "ln_all",
  id_field   = "id",
  output     = "data/processed/donia_talassa/hex_donia/hex_dizieme_donia_lnall.gpkg",
  layer_name = "hex_dizieme_donia_lnall",
  overwrite  = TRUE
)

# hex_cinquieme_sqrt_ln <- aggregate_donia_to_grid(
#   grid_layer = hex_cinquieme,
#   transfo = "sqrt_ln",
#   id_field   = "id",
#   output     = "data/processed/donia_talassa/hex_donia/hex_cinquieme_donia_sqrt_ln_.gpkg",
#   layer_name = "hex_cinquieme_logall_donia",
#   overwrite  = TRUE
# )
# 
# hex_cinquieme_cubert_ln <- aggregate_donia_to_grid(
#   grid_layer = hex_cinquieme,
#   transfo = "cubert_ln",
#   id_field   = "id",
#   output     = "data/processed/donia_talassa/hex_donia/hex_cinquieme_cubert_ln_donia.gpkg",
#   layer_name = "hex_cinquieme_logall_donia",
#   overwrite  = TRUE
# )


# Rectangular exports ----

# Weight : ln (log) for both taille and dr_mllg

rect_carpediem_lnall <- rect_carpediem %>%
  rename(id = fid) %>%
  aggregate_donia_to_grid(
    grid_layer = .,
    transfo = "ln_all",
    id_field   = "id",
    output     = "data/processed/donia_talassa/rect_donia/rect_carpediem_donia_lnall.gpkg",
    layer_name = "rect_carpediem_donia_lnall",
    overwrite  = TRUE
  )

rect_demi_lnall <- aggregate_donia_to_grid(
  grid_layer = rect_demi,
  transfo = "ln_all",
  id_field   = "id",
  output     = "data/processed/donia_talassa/rect_donia/rect_demi_donia_lnall.gpkg",
  layer_name = "rect_demi_donia_lnall",
  overwrite  = TRUE
)

rect_quart_lnall <- aggregate_donia_to_grid(
  grid_layer = rect_quart,
  transfo = "ln_all",
  id_field   = "id",
  output     = "data/processed/donia_talassa/rect_donia/rect_quart_donia_lnall.gpkg",
  layer_name = "rect_quart_donia_lnall",
  overwrite  = TRUE
)

rect_cinquieme_lnall <- aggregate_donia_to_grid(
  grid_layer = rect_cinquieme,
  transfo = "ln_all",
  id_field   = "id",
  output     = "data/processed/donia_talassa/rect_donia/rect_cinquieme_donia_lnall.gpkg",
  layer_name = "rect_cinquieme_donia_lnall",
  overwrite  = TRUE
)






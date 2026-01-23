#' ---
#' title : "talassaR - paths"
#' author : Aubin Woehrel
#' creation date : 2025-10-20
#' last modification : 2025-10-22
#' ---
#'
#' =============================================================================
#'
#' talassaR : Paths
#'
#' Description :
#' Script to store all paths of project
#'
#' =============================================================================

# Creating new environment just for paths
paths_env <- new.env()

# Populating paths in the path environment
with(paths_env, {
  # Raw data ----

  # Survols
  raw_survols_plaba <- "data/raw/survols/us_med_pnmcca_observatoire_survols_plaba.rds"
  raw_survols_usages <- "data/raw/survols/us_med_pnmcca_observatoire_survols_usages.rds"

  # Donia
  raw_donia <- "data/raw/donia/donia.csv"

  # Peche de loisir
  raw_peche_shp <- "data/raw/peche_recreative/peche_loisir_PNMCCA_2025.shp"
  raw_peche_gpkg <- "data/raw/peche_recreative/peche_loisir_PNMCCA_2025.gpkg"
  raw_peche_quentin <- "data/raw/peche_recreative/peche_loisir_PNMCCA_2025_traitement_Quentin.shp" # Donnees prétraitées par Quentin en 2025
  raw_peche_clean <- "data/raw/peche_recreative/us_med_pnmcca_observatoire_peche_loisir_ofb_pt_2020_2024.gpkg"
  raw_peche_resoblo <- "data/raw/peche_recreative/peche_resoblo.csv"

  # PNMCCA borders
  raw_pnmcca_borders <- "data/raw/pnm/N_ENP_PNM_S_000.shp"

  # Carroyage
  raw_carroyage <- "data/raw/carroyage/zone_biocenoses/"

  # Processed data ----

  # Survols
  processed_survols_codenom <- "data/processed/survols/survols_usages_code_vs_nom_a_completer.csv"
  processed_survols_resoblo <- "data/processed/survols/survols_usages_code_vs_nom.csv"
  processed_survols_errors <- "data/processed/survols/spatial/errors/"
  processed_survols_toverify <- "data/processed/survols/spatial/to_verify/"
  processed_survols_corrected <- "data/processed/survols/spatial/corrected/"
  processed_survols_corrected2 <- "data/processed/survols/spatial/corrected2/"

  # Donia
  processed_donia_resoblo <- "data/processed/donia/donia_resoblo.csv"
  processed_donia_points <- "data/processed/donia/donia_points/donia_talassa.gpkg"
  processed_donia_hex <- "data/processed/donia/donia_hex/"
  processed_donia_rect <- "data/processed/donia/donia_rect/"

  # Peche
  processed_peche_hex <- "data/processed/peche/peche_hex/"

  # Outputs ----
  output_donia_type_navire <- "data/processed/donia/donia_type_navire.csv"
})

# Create folders if missing ----

# Extract all character paths from the environment
all_paths <- as.list(paths_env)
all_paths_vector <- unlist(all_paths)

all_folders <- sapply(all_paths_vector, function(p) {
  if (grepl("/$", p)) {
    # It's already a directory path
    p
  } else {
    # It's a file path; extract folder
    dirname(p)
  }
}, USE.NAMES = FALSE)

all_folders <- unique(all_folders)

# Create directories if needed
sapply(all_folders, function(folder) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
})

# Export for sourcing in other scripts ----
paths <- as.list(paths_env)
rm(all_paths, all_folders, all_paths_vector, paths_env)

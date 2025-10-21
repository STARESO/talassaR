#' ---
#' title : "talassaR - paths"
#' author : Aubin Woehrel
#' creation date : 2025-10-20
#' last modification : 2025-10-20
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
  
  # PNMCCA borders
  raw_pnmcca_borders <- "data/raw/pnm/N_ENP_PNM_S_000.shp"
  
  # Carroyage
  raw_carroyage <- "data/raw/carroyage/zone_biocenoses/"
  
  # Processed data ----
  
  # Survols
  processed_survols_code_vs_nom <- "data/processed/survols/survols_usages_code_vs_nom.csv"
  processed_survols_code_ref <- "data/processed/survols/code_ref.csv"
  processed_survols_erreurs <- "data/processed/survols/carto_erreurs/"
  
  # Donia 
  processed_donia_resoblo <- "data/processed/donia/donia_resoblo.csv"
  processed_donia_points <- "data/processed/donia/donia_points/donia_talassa.gpkg"
  processed_donia_hex <- "data/processed/donia/donia_hex/"
  processed_donia_rect <- "data/processed/donia/donia_rect/"

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
    return(p)
  } else {
    # It's a file path; extract folder
    return(dirname(p))
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

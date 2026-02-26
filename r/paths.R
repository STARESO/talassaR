#' ---
#' title : "talassaR - paths"
#' author : Aubin Woehrel
#' creation date : 2025-10-20
#' ---
#'
#' =============================================================================
#'
#' talassaR : Paths
#'
#' Description :
#' Enregistrement de tous les chemins d'accès dans la variable unique paths :
#' 1) Enregistrement des chemins dans des variables au sein d'un environnement
#' dédié
#' 2) Creation des dossiers et emplacements si manquants
#' 3) Sauvegarde des chemins dans la variable unique paths et suppression
#' du reste des données considérées
#'
#' =============================================================================

# Creation d'un nouvel environement propre aux chemins
paths_env <- new.env()

# Creation des variables environnement dans cet environnement ----
with(paths_env, {
  ## Données sources ----

  ### Activités ----
  # Survols
  raw_survols_plaba <- "data/raw/survols/us_med_pnmcca_observatoire_survols_plaba.rds"
  raw_survols_usages <- "data/raw/survols/us_med_pnmcca_observatoire_survols_usages.rds"

  # Donia
  raw_donia <- "data/raw/donia/donia.csv"

  # Peche récréative
  raw_peche_shp <- "data/raw/peche_recreative/peche_loisir_PNMCCA_2025.shp"
  raw_peche_gpkg <- "data/raw/peche_recreative/peche_loisir_PNMCCA_2025.gpkg"
  raw_peche_quentin <- "data/raw/peche_recreative/peche_loisir_PNMCCA_2025_traitement_Quentin.shp" # Donnees prétraitées par Quentin en 2025
  raw_peche_clean <- "data/raw/peche_recreative/us_med_pnmcca_observatoire_peche_loisir_ofb_pt_2020_2024.gpkg"

  # Plongee
  raw_plongee_2023 <- "data/raw/plongee/Plongée_2023.shp"
  raw_plongee_2025 <- "data/raw/plongee/plongée_SIG_2025.csv"

  ### Habitats et carroyage ----
  # Habitats
  raw_habitats_andromede <- "data/raw/habitats/biocenoses_andromede_PNMCCA_2025_fixed_geometry_by_stareso.shp"
  raw_habitats_grottes <- "data/raw/habitats/grottes.shp"

  # PNMCCA borders
  raw_pnmcca_borders <- "data/raw/pnm/N_ENP_PNM_S_000.shp"

  # Carroyage
  raw_carroyage <- "data/raw/carroyage/zone_biocenoses/"
  raw_carroyage_final <- "data/raw/carroyage/zone_biocenoses/grille_talassa_2025_cotier_hexagone_cinquiemedemile.shp"

  ### Référentiels codes et intitulés----
  raw_codes_survolusage <- "data/raw/codes/referentiel_codes_survolusages_resoblo.csv"
  raw_codes_talassa <- "data/raw/codes/referentiel_codes_resoblo_talassa.xlsx"
  raw_codes_peche <- "data/raw/codes/referentiel_codes_peche_resoblo.csv"
  raw_codes_donia <- "data/raw/codes/referentiel_codes_donia_resoblo.csv"
  raw_codes_habitats <- "data/raw/codes/referentiel_codes_habitats.xlsx"

  ## Données traitées ----

  # Données developpement
  processed_survols_codenom <- "data/processed/dev/survols/survols_usages_code_vs_nom_a_completer.csv"
  # processed_survols_resoblo <- "data/processed/dev/survols/survols_usages_code_vs_nom.csv"
  processed_survols_errors <- "data/processed/dev/survols/spatial/errors/"
  processed_survols_toverify <- "data/processed/dev/survols/spatial/to_verify/"
  processed_survols_corrected <- "data/processed/dev/survols/spatial/corrected/"
  processed_survols_hex <- "data/processed/dev/survols/spatial/spatial_hex/"
  # processed_donia_resoblo <- "data/processed/dev/donia/donia_resoblo.csv"
  processed_donia_type_navire <- "data/processed/dev/donia/donia_type_navire.csv"
  processed_donia_hex <- "data/processed/dev/donia/donia_hex/"
  processed_donia_rect <- "data/processed/dev/donia/donia_rect/"

  # Données corrigées format observatoire
  processed_obs_survolusage <- "data/processed/observatoire/us_med_pnmcca_observatoire_survols_usages_ofb_pts_4326.gpkg"
  processed_obs_survolplaba <- "data/processed/observatoire/us_med_pnmcca_observatoire_survols_plaba_ofb_pts_4326.gpkg"
  processed_obs_peche <- "data/processed/observatoire/us_med_pnmcca_observatoire_peche_recreative_ofb_pts_4326.gpkg"
  processed_obs_donia <- "data/processed/observatoire/us_med_pnmcca_observatoire_donia_ofb_pts_4326.gpkg"
  processed_obs_plongee <- "data/processed/observatoire/us_med_pnmcca_observatoire_plongee_sites_ofb_pts_4326.gpkg"
  processed_obs_habitats <- "data/processed/observatoire/eco_med_pnmcca_observatoire_habitats_andromede_pol_4326.gpkg"
  processed_obs_grottes <- "data/processed/observatoire/eco_med_pnmcca_observatoire_grottes_ofb_pts_4326.gpkg"

  # Données format Talassa
  processed_tal_survolusage <- "data/processed/talassa_pts/us_med_pnmcca_talassa_survols_usages_ofb_pts_4326.gpkg"
  processed_tal_survolplaba <- "data/processed/talassa_pts/us_med_pnmcca_talassa_survols_plaba_ofb_pts_4326.gpkg"
  processed_tal_peche <- "data/processed/talassa_pts/us_med_pnmcca_talassa_peche_ofb_pts_4326.gpkg"
  processed_tal_donia <- "data/processed/talassa_pts/us_med_pnmcca_talassa_donia_ofb_pts_4326.gpkg"
  processed_tal_plongee <- "data/processed/talassa_pts/us_med_pnmcca_talassa_plongee_ofb_pts_4326.gpkg"
  processed_tal_habitats <- "data/processed/talassa_pts/eco_med_pnmcca_talassa_habitats_ofb_pol_4326.gpkg"
  processed_tal_grottes <- "data/processed/talassa_pts/eco_med_pnmcca_talassa_grottes_ofb_pts_4326.gpkg"

  # Données format Talassa maillées (hexagones)
  processed_hex_activites <- "data/processed/talassa_hex/us_med_pnmcca_talassa_activites_ofb_pol_4326.gpkg"
  processed_hex_habitats <- "data/processed/talassa_hex/us_med_pnmcca_talassa_habitats_ofb_pol_4326.gpkg"
  processed_hex_carroyage <- "data/processed/talassa_hex/us_med_pnmcca_talassa_carroyage_ofb_pol_4326.gpkg"

  # Exports ----
  # NA for now but need Donia
})

# Création des dossiers si manquants ----

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

# Export des chemins dans la variable paths pour un accès unique ----
paths <- as.list(paths_env)
rm(all_paths, all_folders, all_paths_vector, paths_env)

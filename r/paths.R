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

  # ais
  raw_ais_vesselfinder <- "data/raw/ais/ais_vessel_finder_2009-2024_pnmcca.gpkg"
  raw_ais_marinetraffic <- "data/raw/ais/ais_marine_traffic_2025"

  ### Habitats et carroyage ----
  # Habitats
  raw_habitats_andromede <- "data/raw/habitats/biocenoses_andromede_PNMCCA_2025_fixed_geometry_by_stareso.shp"
  raw_habitats_grottes <- "data/raw/habitats/grottes.shp"

  # Délimitations carto PNM & Corse
  raw_pnmcca_borders <- "data/raw/pnm/delimitation_pnmcca_2154.gpkg"
  raw_corsica_borders <- "data/raw/pnm/histolitt_corse_region_4326.gpkg"

  # Carroyage
  raw_carroyage <- "data/raw/carroyage/zone_biocenoses/"
  raw_carroyage_hexcinquieme <- "data/raw/carroyage/zone_biocenoses/grille_talassa_2025_cotier_hexagone_cinquiemedemile.gpkg"
  raw_carroyage_rect1mile <- "data/raw/carroyage/zone_biocenoses/grille_carpediem_1m_2019_pnmcca_cotier.gpkg"

  ### Référentiels codes et intitulés ----
  raw_codes_survolusage <- "data/raw/codes/reference_codes_survolusages_resoblo.csv"
  raw_codes_talassa <- "data/raw/codes/reference_codes_resoblo_talassa.xlsx"
  raw_codes_peche <- "data/raw/codes/reference_codes_peche_resoblo.csv"
  raw_codes_donia <- "data/raw/codes/reference_codes_donia_resoblo.csv"
  raw_codes_habitats <- "data/raw/codes/reference_codes_habitats.xlsx"
  raw_codes_pressions <- "data/raw/codes/reference_codes_pressions.xlsx"
  
  # Matrices de sensibilité et de pression
  raw_mat_sensibilite <- "data/raw/matrices/matrice_sensibilite_habitats_format_lariviere.xlsx"

  # Références formules et IC carroyage ----
  raw_devcarroyage_activites <- "data/raw/references_carroyage/reference_carroyage_activites_a_completer.xlsx"
  raw_devcarroyage_habitats <- "data/raw/references_carroyage/reference_carroyage_habitats_a_completer.xlsx"
  raw_refcarroyage_activites <- "data/raw/references_carroyage/reference_carroyage_activites.xlsx"
  raw_refcarroyage_habitats <- "data/raw/references_carroyage/reference_carroyage_habitats.xlsx"

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
  processed_obs_ais <- "data/processed/observatoire/us_med_pnmcca_observatoire_ais_vesselfinder_pts_4326.gpkg"
  processed_obs_habitats <- "data/processed/observatoire/eco_med_pnmcca_observatoire_habitats_andromede_pol_4326.gpkg"
  processed_obs_grottes <- "data/processed/observatoire/eco_med_pnmcca_observatoire_grottes_ofb_pts_4326.gpkg"

  # Données format Talassa
  processed_tal_survolusage <- "data/processed/talassa_pts/us_med_pnmcca_talassa_survols_usages_ofb_pts_4326.gpkg"
  processed_tal_survolplaba <- "data/processed/talassa_pts/us_med_pnmcca_talassa_survols_plaba_ofb_pts_4326.gpkg"
  processed_tal_peche <- "data/processed/talassa_pts/us_med_pnmcca_talassa_peche_ofb_pts_4326.gpkg"
  processed_tal_donia <- "data/processed/talassa_pts/us_med_pnmcca_talassa_donia_ofb_pts_4326.gpkg"
  processed_tal_plongee <- "data/processed/talassa_pts/us_med_pnmcca_talassa_plongee_ofb_pts_4326.gpkg"
  processed_tal_ais <- "data/processed/talassa_pts/us_med_pnmcca_talassa_ais_vesselfinder_pts_4326.gpkg"
  processed_tal_habitats <- "data/processed/talassa_pts/eco_med_pnmcca_talassa_habitats_ofb_pol_4326.gpkg"
  processed_tal_grottes <- "data/processed/talassa_pts/eco_med_pnmcca_talassa_grottes_ofb_pts_4326.gpkg"
  processed_tal_habitats_intermediaire <- "data/processed/talassa_pts/talassa_habitats_decoupe_intermediaire_par_hexcinquieme.gpkg"

  # Données matrices
  processed_mat_sensibilites <- "data/processed/matrices/sensibilites.rds"

  # Données format Talassa maillées (hexagones)
  processed_hex_activites <- "data/processed/talassa_hex/us_med_pnmcca_talassa_activites_ofb_pol_4326.gpkg"
  processed_hex_activites_intitule <- "data/processed/talassa_hex/us_med_pnmcca_talassa_activites_intitules_ofb_pol_4326.gpkg"
  processed_hex_habitats <- "data/processed/talassa_hex/us_med_pnmcca_talassa_habitats_ofb_pol_4326.gpkg"
  processed_hex_habitats_m2 <- "data/processed/talassa_hex/us_med_pnmcca_talassa_habitats_wide_surface_ofb_pol_4326.gpkg"
  processed_hex_habitats_pct <- "data/processed/talassa_hex/us_med_pnmcca_talassa_habitats_wide_pourcentage_ofb_pol_4326.gpkg"
  processed_hex_carroyage <- "data/processed/talassa_hex/us_med_pnmcca_talassa_carroyage_ofb_pol_4326.gpkg"

  # Aide parametrage
  processed_params_combinaisons <- "data/processed/params/talassa_liste_combinaisons_activites.xlsx"


  # Exports ----
  # NA --> pas d'exports pour l'instant
})

# Création des dossiers si manquants ----

# Enregistrement des chemins de l'environnement paths_env dans la variable all_paths
all_paths <- as.list(paths_env)
all_paths_vector <- unlist(all_paths)

# Liste de l'architecture des dossiers des chemins de all_paths
all_folders <- sapply(all_paths_vector, function(p) {
  if (grepl("/$", p)) { # Déjà un chemin
    p
  } else { # Extraction du dossier concerné par le chemin
    dirname(p)
  }
}, USE.NAMES = FALSE)

all_folders <- unique(all_folders)

# Création des dossiers en local si inexistants
sapply(all_folders, function(folder) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
})

# Export des chemins dans la variable paths pour un accès dans d'autres scripts ----
paths <- as.list(paths_env)
rm(all_paths, all_folders, all_paths_vector, paths_env)

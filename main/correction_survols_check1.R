#' ---
#' title : "talassaR - correction survols check1"
#' author : Aubin Woehrel
#' creation date : 2025-09-16
#' ---
#'
#' =============================================================================
#'
#' talassaR : Correction survols check n1
#'
#' Description :
#' Script permettant de faire la première étape de check des données de survol
#' des usages (plan d'eau) du PNMCCA.
#'
#' =============================================================================

# Initialisation ----

## Nettoyage ----
rm(list = ls())

## Import des librairies ----

# Data: Import et manipulations
library("dplyr")
library("tidyr")

# Data: Spatial
library("sf")

## Ressources locales ----
paths <- yaml::read_yaml("config/paths.yml")

## Import des données ----
survols_usages <- readRDS(paths$raw$survols_usages)

# Code vs intitule verifications ----

## Check structure ----
skimr::skim(survols_usages)

## Checks association codes - intitulés ----
code_vs_nom <- survols_usages %>%
  select(cod_act, act) %>%
  group_by(cod_act, act) %>%
  summarize(n = n()) %>%
  arrange(cod_act)

code_vs_nom_byyear <- survols_usages %>%
  select(cod_act, act, annee) %>%
  group_by(cod_act, act, annee) %>%
  summarize(erreur_code_n = n()) %>%
  arrange(cod_act, annee)

## Exports status associations codes intitulés ----

# Ecriture csv à corriger/completer
output_ref <- TRUE
if (output_ref) {
  write.csv2(code_vs_nom, paths$processed$survols_codenom)
}

# Import du csv des nouvelles associations et description erreurs
input_ref <- TRUE
if (input_ref) {
  survols_resoblo <- read.csv(paths$raw$codes_survolusage, sep = ";")
}

# Jointure données avec description erreurs ----
survols_usages_fusion <- left_join(
  x = survols_usages,
  y = survols_resoblo,
  by = join_by(act, cod_act)
)

## Checks n2 ----

# Structure générale
skimr::skim(survols_usages_fusion)

# Nombre d'erreurs par saison
t1 <- survols_usages_fusion %>%
  filter(erreur_code == "invalide") %>%
  group_by(
    annee,
    mois,
    cod_act,
    act,
    erreur_code,
    erreur_code_description,
    erreur_code_suggestion,
    suggestion_resoblo_code,
    suggestion_resoblo_intitule
  ) %>%
  summarize(erreur_code_n = n()) %>%
  relocate(erreur_code_n, .after = erreur_code) %>%
  mutate(mois = case_when(
    mois == "June" ~ "Juin",
    mois == "July" ~ "Juillet",
    mois == "August" ~ "Aout",
    mois == "September" ~ "Septembre"
  )) %>%
  mutate(mois = factor(mois, levels = c("Juin", "Juillet", "Aout", "Septembre"))) %>%
  arrange(annee, mois, cod_act, act)

View(t1)

# Changement noms de colonnes vers resoblo
survols_usages_fusion <- survols_usages_fusion %>%
  mutate(
    resoblo_code = suggestion_resoblo_code,
    resoblo_intitule = suggestion_resoblo_intitule
  )

# Export vers format spatial ----

## Transfo spatiale ----
spatial_usages <- st_as_sf(
  x = survols_usages_fusion,
  coords = c("lon_x", "lat_y"),
  crs = 4326
)

## Export fichier spatial par date pour correction ----

# Recupération des années uniques
unique_years <- unique(spatial_usages$annee)

# Boucle sur les années
for (year in unique_years) {
  # Dossiers pour les années
  folder_toverify <- file.path(paths$processed$survols_toverify, year, fsep = "")
  folder_errors <- file.path(paths$processed$survols_errors, year, fsep = "")

  if (!dir.exists(folder_toverify)) {
    dir.create(folder_toverify)
  }

  if (!dir.exists(folder_errors)) {
    dir.create(folder_errors)
  }

  # Recupération de toutes les dates pour l'année considérée
  dates_in_year <- unique(spatial_usages$date[spatial_usages$annee == year]) %>% sort()

  # Boucle sur les dates de l'année
  for (i in seq_along(dates_in_year)) {
    # La date i considérée
    date_wanted <- dates_in_year[i]

    # Filtration des données sources pour la date considérée
    spatial_subdata <- spatial_usages %>%
      filter(date == date_wanted) %>%
      rename(
        valide = erreur_code,
        description = erreur_code_description,
        suggestion = erreur_code_suggestion
      ) %>%
      mutate(across(c(d_heur_sur, f_heur_sur), ~ as.character(hms::as_hms(.))))

    # Uniquement récupération des infos erreurs
    errors_subdata <- spatial_subdata %>%
      filter(valide == "invalide") %>%
      select(
        id_acti,
        date,
        annee,
        mois,
        nom_acti,
        act,
        cod_act,
        valide,
        description,
        suggestion,
        resoblo_code,
        resoblo_intitule
      )

    # Noms de fichiers avec la date de survol

    # Toutes les données à la date donnée
    subdata_filename <- file.path(
      folder_toverify,
      paste0("survol_usages_toverify_", format(date_wanted, "%Y-%m-%d"), ".gpkg")
    )

    # Que les erreurs à la date donnée
    errors_filename <- file.path(
      folder_errors,
      paste0("survol_usages_errors_", format(date_wanted, "%Y-%m-%d"), ".gpkg")
    )

    # Export des données au format spatial
    st_write(
      obj = spatial_subdata,
      dsn = subdata_filename,
      driver = "GPKG",
      append = FALSE
    )

    # Export des erreurs au format spatial
    if (dim(errors_subdata)[1] > 0) {
      st_write(
        obj = errors_subdata,
        dsn = errors_filename,
        driver = "GPKG",
        append = FALSE
      )
    }

    # Messages de confirmation
    message(paste("Exported spatial data to verify:", subdata_filename))
    message(paste("Exported error data to verify:", errors_filename))
  }
}

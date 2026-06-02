#' ---
#' title : "talassaR - correction survols check2"
#' author : Aubin Woehrel
#' creation date : 2025-11-18
#' ---
#'
#' =============================================================================
#'
#' talassaR : Correction survols check n2
#'
#' Description :
#' Script permettant de faire la seconde étape de check des données de survol
#' des usages (plan d'eau) du PNMCCA.
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
library("purrr")

# Data: représentations
library("ggplot2")

# Data: spatial
library("sf")

## Ressources locales ----
paths <- yaml::read_yaml("config/paths.yml")

## Import des données ----

# liste des fichiers spatiaux corrigés
file_names <- list.files(paths$processed$survols_corrected, full.names = TRUE)

# compilation des fichiers spatiaux corrigés
survols_resoblo <- file_names %>%
  map(st_read, quiet = TRUE) %>%
  bind_rows()


# Check général  ----

# Structure
skimr::skim(survols_resoblo)

# Verification des codes et intitulés resoblo
verif_codes_resoblo <- survols_resoblo %>%
  st_drop_geometry() %>%
  select(resoblo_code, resoblo_intitule) %>%
  group_by(resoblo_code, resoblo_intitule) %>%
  summarise(n = n()) %>%
  ungroup()

# Check des erreurs en identifiant les mauvaises correspondances de noms et codes.
# En cas d'erreurs, check du numéro de ligne avec erreur à vérifier dans le fichier source
# et numéro de ligne correspond à celui dans verif_codes_resoblo

slice_flag <- FALSE

if (slice_flag) {
  to_slice <- c(1, 3, 4, 6, 12, 14, 16, 20, 31, 39)
  to_slice <- c(13)

  resoblo_to_correct <- verif_codes_resoblo %>%
    slice(to_slice) %>%
    mutate(key_resoblo = str_c(resoblo_code, resoblo_intitule, sep = "_"))

  to_correct <- survols_resoblo %>%
    st_drop_geometry() %>%
    mutate(key_resoblo = stringr::str_c(resoblo_code, resoblo_intitule, sep = "_")) %>%
    right_join(., resoblo_to_correct) %>%
    select(id_acti, date, resoblo_code, resoblo_intitule, key_resoblo)
}

# Check du crs
sf::st_crs(survols_resoblo) # 4326

# Changement position colonnes
survols_resoblo <- survols_resoblo %>%
  relocate(resoblo_intitule, resoblo_code, .after = id_acti)

# Suppression colonnes inutiles
survols_resoblo <- survols_resoblo %>%
  select(-c(
    nom_acti,
    act,
    categorie_usage,
    cod_act
  ))

# Exporting final format spatial resoblo ----
st_write(
  obj = survols_resoblo,
  dsn = paths$processed$obs_survolusage,
  driver = "GPKG",
  append = FALSE
)

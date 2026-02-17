#' ---
#' title : "talassaR - correction peche"
#' author : Aubin Woehrel
#' creation date : 2026-01-20
#' ---
#'
#' =============================================================================
#'
#' talassaR : Correction peche
#'
#' Description :
#' Script permettant de checker les données d'enquêtes de plongées loisir du
#' Parc (sans les données Stareso).
#' Check de divers types de données sources, mais au final rabattu vers une
#' correction manuelle sur QGIS (qui ne contient pas les données d'enquêtes
#' STARESO).
#'
#' =============================================================================


# Initialisation ----

## Nettoyage  ----
rm(list = ls())

## Import des librairies ----

# Lecture et manipulations de données
library("readr")
library("dplyr")
library("tidyr")
library("stringr")

# Spatial
library("sf")
library("leaflet")

## Ressources locales ----
source("r/paths.R")
source("r/fct_category_map.R") # Fonction personnalisée carto R
source("r/fct_map_peche.R")


# Import des données ----

# Peche de loisir (versions différentes des données sources)
peche_loisir <- st_read(paths$raw_peche_shp) %>% st_transform(crs = 4326)
peche_loisir_gpkg <- st_read(paths$raw_peche_gpkg) %>% st_transform(crs = 4326)
peche_loisir_quentin <- st_read(paths$raw_peche_quentin) %>% st_transform(crs = 4326)

# Jeu de données nettoyé manuellement pour TALASSA
peche_loisir_clean <- st_read(paths$raw_peche_clean) %>% st_transform(crs = 4326)

# Données carroyage
hex_cinquieme <- st_read(paste0(paths$raw_carroyage, "grille_talassa_2025_cotier_hexagone_cinquiemedemile.shp"))

# Délimitation PNMCCA
pnm_borders <- sf::st_read(paths$raw_pnmcca_borders) %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(NOM_SITE == "cap Corse et Agriate")

# Liens peche-RESOBLO
codes_resoblo <- read.csv2(paths$raw_codes_peche)

# Investigation et modifs initiales ----

## Premiers checks ----

# Check structure
str(peche_loisir)
str(peche_loisir_gpkg)
str(peche_loisir_clean)
str(peche_loisir_quentin) # Not exactly all the same variable names

# Few column names changes for quentin's dataset
peche_loisir_quentin <- peche_loisir_quentin %>%
  rename_with(stringr::str_to_lower) %>%
  rename_with(function(x) {
    str_replace_all(x, "\\.", "_")
  }) %>%
  rename(mod_pech = mod_peche)

## Représentations cartographiques interactives ----

# All data
map_peche(peche_loisir)
map_peche(peche_loisir_gpkg)
map_peche(peche_loisir_quentin)
map_peche(peche_loisir_clean)

# Map of og fishing dataset by type
map_peche(data_peche = peche_loisir, type_peche = "po")
map_peche(data_peche = peche_loisir, type_peche = "pe")
map_peche(data_peche = peche_loisir, type_peche = "pdb")
map_peche(data_peche = peche_loisir, type_peche = "csm")

# Map of corrected dataset (quentin 2025) per type
map_peche(data_peche = peche_loisir_quentin, type_peche = "po")
map_peche(data_peche = peche_loisir_quentin, type_peche = "pe")
map_peche(data_peche = peche_loisir_quentin, type_peche = "pdb")
map_peche(data_peche = peche_loisir_quentin, type_peche = "csm")

## General variable investigation ----
View(peche_loisir)

sort(unique(peche_loisir$id_obs))
unique(peche_loisir$mod_pech) # All levels are ok

# Testing unicity of identifiers : which are specific to each survey ?
test <- peche_loisir_quentin %>%
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
# View(count_pdb)

# Correction des données pré-corrigées ----

# Jointure codes resoblo
peche_resoblo <- left_join(peche_loisir_clean, codes_resoblo, by = join_by(mod_pech))

# Verification des dimensions post-jointure
dim(peche_resoblo)
dim(peche_loisir_clean)

# Verification des codes post-jointure
verif_codes <- peche_resoblo %>%
  st_drop_geometry() %>%
  select(resoblo_intitule_n3, resoblo_code_n3, mod_pech, mod_pech_intitule) %>%
  distinct()

# Check visuel données et carto
map_peche(peche_resoblo)

# Elimination des colonnes inutiles
peche_resoblo <- peche_resoblo %>%
  select(-c(
    y_lat,
    y_degre,
    y_min_dec,
    y_lat_DD,
    lat,
    x_lon,
    x_degre,
    x_min_dec,
    x_lon_DD,
    long
  ))

# Relocalistion des colonnes
peche_resoblo <- peche_resoblo %>%
  relocate(resoblo_intitule_n3, resoblo_code_n3, mod_pech_intitule, mod_pech, .after = id_sortie) %>%
  rename(resoblo_intitule = resoblo_intitule_n3, resoblo_code = resoblo_code_n3)

# Changement type colonnes
str(peche_resoblo)

peche_resoblo <- peche_resoblo %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))

# Correction contenu colonne fond_rech
peche_resoblo <- peche_resoblo %>%
  mutate(fond_rech = case_when(
    fond_rech == "aucun_part" ~ "aucun",
    TRUE ~ str_replace_all(fond_rech, ". ", "_")
  ))

# Elimination de la portion espèces pour l'instant afin d'éviter tout souci
# sur les problèmes initiaux de fusion des données Stareso-PNMCCA
peche_resoblo <- peche_resoblo %>%
  select(-c(esp_cib:obs, choix_site)) %>%
  group_by(id_obs) %>%
  distinct()

# Verification entités uniques
t1 <- peche_resoblo %>%
  group_by(id_obs) %>%
  summarize(n = n()) %>%
  filter(n > 1)

dim(t1)[1] # Nombre d'entités avec répétition de l'id_obs --> si > 0 voir erreurs

# Transfo type
View(peche_resoblo)

st_write(
  obj = peche_resoblo,
  dsn = paths$processed_obs_peche,
  driver = "gpkg",
  append = FALSE
)

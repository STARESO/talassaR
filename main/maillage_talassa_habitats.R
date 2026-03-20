#' ---
#' title : "talassaR - maillage_talassa_habitats"
#' author : Aubin Woehrel
#' creation date : 2026-03-18
#' ---
#'
#' =============================================================================
#'
#' talassaR : Maillage des données d'habitat
#'
#' Description :
#' Script permettant de passer des données d'habitats au format TALASSA polygone
#' ou ponctuel vers un format au maillage TALASSA choisi.
#'
#' =============================================================================

# Initialisation ----

## Nettoyage ----
rm(list = ls())

## Import des librairies ----

# Manipulations de données
library("dplyr")
library("tidyr")
library("stringr")

# Spatial
library("sf")

## Ressources locales ----
source("r/paths.R")

## Import des données -----

# Habitats
habitats_talassa <- st_read(paths$processed_tal_habitats_intermediaire)
grottes_talassa <- st_read(paths$processed_tal_grottes)

# Carroyage
carroyage_hex <- st_read(paths$raw_carroyage_final) %>%
  st_transform(., crs = 4326)

# Côte Corse
cote_corse <- st_read(paths$raw_corsica_borders)

# Precheck ----
names(habitats_talassa)
names(habitats_intermediaire)
names(grottes_talassa)

names(carroyage_hex)
names(cote_corse)

# Check validité délimitation Corse
# Si invalide --> corriger source Qgis (pour vérifications autre que R)
st_is_valid(cote_corse, reason = TRUE)[which(st_is_valid(cote_corse) == FALSE)]
which(st_is_valid(cote_corse) == FALSE)

# Calculs des surfaces carroyage ----

## Calcul surface maille totale ----
carroyage_hex <- carroyage_hex %>%
  select(id_hex) %>%
  st_transform(., crs = 2154) %>%
  mutate(aire_maille = round(st_area(geometry))) %>%
  relocate(aire_maille, .after = id_hex) %>%
  st_transform(., crs = 4326)

## Calcul surface terrestre et mer hexagones intersectants ----

# Liste des numéros de lignes des carroyages en contact avec le trait de côte
intersection_cote <- st_intersects(cote_corse, carroyage_hex)

# Découpe des hexagones selon le trait de côte puis calcul surface mer et terre
carroyage_cote <- st_difference(carroyage_hex, cote_corse) %>%
  st_transform(., crs = 2154) %>%
  mutate(
    aire_mer = round(st_area(geometry)),
    aire_terre = aire_maille - aire_mer
  ) %>%
  relocate(geometry, .after = last_col()) %>%
  st_transform(., crs = 4326)

# Jointure données carroyage cote sur couche hexagones entiers
carroyage_final <- carroyage_cote %>%
  st_drop_geometry() %>%
  select(-aire_maille) %>%
  left_join(
    x = carroyage_hex,
    y = .,
    by = join_by(id_hex)
  )

# Réduction utilisation environnement
rm(carroyage_hex, carroyage_cote, intersection_cote)


# Calcul surface habitats ----

# Transfo crs
st_crs(habitats_talassa)$epsg # 2154

# Calcul surface de chaque polygone habitat
habitats_talassa <- habitats_talassa %>%
  rename(geometry = geom) %>%
  mutate(aire_habitat = as.numeric(st_area(geometry)))

# Calcul surface sommé de chaque type d'habitat par hexagone (par id_hex)
habitats_sum <- habitats_talassa %>%
  st_drop_geometry(.) %>%
  group_by(id_hex, talassa_code) %>%
  summarise(
    aire_habitat = round(sum(aire_habitat, na.rm = TRUE)),
    .groups = "drop"
  )

# Jointure du carroyage final sur les polygones d'habitats sommés
# pour calcul pourcentage surface avec 1 ligne = 1 polygone habitat
# via le rajout de l'aire de la mer de chaque hexagone
carroyage_habitats <- left_join(
  x = habitats_sum,
  y = carroyage_final,
  by = "id_hex"
)

# Calcul pourcentages surface aire habitat, aire terre et aire mer
carroyage_habitats <- carroyage_habitats %>%
  mutate(
    pourcentage_habitat = as.numeric(round(aire_habitat / aire_mer * 100, 2)), # pourcentage habitat par rapport à la surface de mer
    pourcentage_terre = as.numeric(round(aire_terre / aire_maille * 100, 2)),
    pourcentage_mer = 100 - pourcentage_terre
  )

# Pivot vers format large avec une ligne = un hexagone pour valeurs aires en m2
habitats_wide_m2 <- carroyage_habitats %>%
  pivot_wider(
    id_cols = c("id_hex", "aire_maille", "aire_mer", "aire_terre"),
    names_from = talassa_code,
    values_from = aire_habitat,
    values_fill = 0
  )

# Quelques pertes d'infos hexagones
dim(habitats_wide_m2)
dim(carroyage_final)

# Jointure sur le carroyage complet final pour éviter les pertes d'hexagones
habitats_wide_m2 <- left_join(
  x = carroyage_final %>% select(id_hex, geometry),
  y = habitats_wide_m2,
  by = join_by(id_hex)
)

# Pivot vers format large avec une ligne = un hexagone pour valeurs pourcentage
habitats_wide_pct <- carroyage_habitats %>%
  pivot_wider(
    id_cols = c("id_hex", "aire_maille", "pourcentage_mer", "pourcentage_terre"),
    names_from = talassa_code,
    values_from = pourcentage_habitat,
    values_fill = 0
  )

# Quelques pertes d'hexagones
dim(habitats_wide_pct)
dim(carroyage_final)

# Jointure sur le carroyage complet final
habitats_wide_pct <- left_join(
  x = carroyage_final %>% select(id_hex, geometry),
  y = habitats_wide_pct,
  by = join_by(id_hex)
)


# Exports ----

# Carroyage final
st_write(
  obj = carroyage_final,
  dsn = paths$processed_hex_carroyage,
  driver = "gpkg",
  append = FALSE
)

# Données habitats en unité surfacique m2
st_write(
  obj = habitats_wide_m2,
  dsn = paths$processed_hex_habitats_m2,
  driver = "gpkg",
  append = FALSE
)

# Données habitats en pourcentage d'aire
st_write(
  obj = habitats_wide_pct,
  dsn = paths$processed_hex_habitats_pct,
  driver = "gpkg",
  append = FALSE
)

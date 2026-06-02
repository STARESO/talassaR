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
paths <- yaml::read_yaml("config/paths.yml")

## Import des données -----

# Habitats
habitats_talassa <- st_read(paths$processed$talassa_habitats_intermediaire) # Version découpée sur mailles via QGIS
grottes_talassa <- st_read(paths$processed$talassa_grottes)

# Carroyage
carroyage_hex <- st_read(paths$raw$carroyage_hexcinquieme) %>%
  st_transform(., crs = 4326)

# Côte Corse
cote_corse <- st_read(paths$raw$corsica_borders)

# Sensibilités aux pressions
sensibilite <- readRDS(paths$processed$mat_sensibilites)


# Precheck ----
if ("geom" %in% names(carroyage_hex)) {
  carroyage_hex <- carroyage_hex %>%
    rename(geometry = geom)
}

if ("geom" %in% names(habitats_talassa)) {
  habitats_talassa <- habitats_talassa %>%
    rename(geometry = geom)
}


names(habitats_talassa)
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

carroyage_final <- carroyage_final %>%
  mutate(srm = "MED") %>%
  relocate(srm, .after = id_hex)

# Réduction utilisation environnement
rm(carroyage_hex, carroyage_cote, intersection_cote)


# Calcul surface habitats ----

# Transfo crs
st_crs(habitats_talassa)$epsg # 2154

# Calcul surface de chaque polygone habitat
habitats_talassa <- habitats_talassa %>%
  mutate(aire_habitat = as.numeric(st_area(geometry)))

# Calcul surface sommé de chaque type d'habitat par hexagone (par id_hex)
habitats_sum <- habitats_talassa %>%
  st_drop_geometry(.) %>%
  group_by(id_hex, talassa_code, talassa_intitule, hab_iq) %>%
  summarise(
    aire_habitat = round(sum(aire_habitat, na.rm = TRUE))
  )

dim(habitats_talassa)
dim(habitats_sum)

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

carroyage_habitats <- carroyage_habitats %>%
  relocate(srm, .after = id_hex) %>%
  st_drop_geometry() %>%
  select(-geometry)

# Jointure geométrie hexagones carroyage final
###### ATTENTION, A INVESTIGUER MAIS IL SEMBLE Y AVOIR DES HEXAGONES SANS HABITATS ??
###### POUR L'INSTANT, SWITCH X ET Y DANS JOIN POUR EVITER HEXAGONES DE CARROYAGE FINAL 
###### SANS HABITATS EN PLUS
carroyage_habitats <- left_join(
  y = carroyage_final %>% select(id_hex, geometry),
  x = carroyage_habitats,
  by = join_by(id_hex)
)

# Jointure valeurs de sensiblite aux données spatiales d'habitats
sensibilite <- sensibilite %>%
  rename(talassa_intitule_check = talassa_intitule)

carroyage_habitats <- left_join(
  x = carroyage_habitats,
  y = sensibilite, 
  by = join_by(talassa_code)
)

# Finalisation données format final ----

## Réagencement et noms ----
# Changements noms et réagencement colonnes pour correspondance exacte format modélo
carroyage_habitats_final <- carroyage_habitats %>%
  rename(
    id2 = id_hex, 
    surf_cel = aire_maille, 
    surf_ter = aire_terre, 
    surfmer = aire_mer, 
    surfhab_cel = aire_habitat,
    surfhab_pcel = pourcentage_habitat,
    hab_lib = talassa_intitule
  ) %>%
  select(-c(talassa_intitule_check, pourcentage_mer, pourcentage_terre)) %>%
  select(id2, srm, surf_cel, surf_ter, surfmer, surfhab_cel, surfhab_pcel, hab_lib, talassa_code, hab_iq, everything())


# Changement nom id pour carroyage_final
carroyage_final <- carroyage_final %>%
  rename(
    id2 = id_hex,
    surf_cel = aire_maille, 
    surf_ter = aire_terre, 
    surfmer = aire_mer,
  ) %>%
  relocate(surf_ter, .after = surf_cel)

## Changement unités aires m2 vers km2 sauf pour surfhab_cel ----
carroyage_habitats_final <- carroyage_habitats_final %>%
  mutate(across(c(surf_cel, surf_ter, surfmer), \(x) {as.numeric(x) * 10^-6}))

carroyage_final <- carroyage_final %>%
  mutate(across(c(surf_cel, surf_ter, surfmer), \(x) {as.numeric(x) * 10^-6}))

## Ajout colonne zone carroyage ----
carroyage_final <- carroyage_final %>%
  mutate(zone = NA) %>%
  relocate(geometry, .after = last_col())

# Exports ----

# Carroyage final
st_write(
  obj = carroyage_final,
  dsn = paths$processed$hex_carroyage,
  driver = "gpkg",
  append = FALSE
)

# Carroyage habitat format long
st_write(
  obj = carroyage_habitats_final,
  dsn = paths$processed$hex_habitats,
  driver = "gpkg",
  append = FALSE
)

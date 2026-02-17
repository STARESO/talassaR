#' ---
#' title : "talassaR - correction_donia"
#' author : Aubin Woehrel
#' creation date : 2025-09-15
#' ---
#'
#' =============================================================================
#'
#' talassaR : Correction des données Donia
#'
#' Description :
#' Script permettant de vérifier et corriger les données de Donia pour
#' une utilisation finale dans le projet TALASSA. Etapes :
#' 1) Vérification des données
#' 2) Jointure codes RESOBLO
#'
#' =============================================================================


# Initialisation ----

## Nettoyage ----
rm(list = ls())

## Import des librairies ----

# Lecture et manipulations de données
library("readr")
library("dplyr")
library("tidyr")
library("stringr")

# Représentations
library("ggplot2")
library("ggbeeswarm")
library("ggExtra")
library("ggpubr")

# Spatial
library("sf")
library("leaflet")
library("rlang")

## Ressources locales ----
source("r/paths.R")
source("r/fct_category_map.R") # Fonction personnalisée carto R


## Import des données ----

# Donia
donia <- read_delim(
  paths$raw_donia,
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

# Délimitation PNMCCA
pnm_borders <- sf::st_read(paths$raw_pnmcca_borders) %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(NOM_SITE == "cap Corse et Agriate")

# Liens codes Resoblo - Donia
donia_resoblo <- read.csv2(paths$raw_codes_donia) %>%
  select(-type_navire)


# Investigation et modifs initiales ----

## Check structure ----
spec(donia)
skimr::skim(donia)
names(donia)

## Modifs initiales structure ----
donia <- donia %>%
  separate_wider_delim(
    col = date_mouillage_central_european_time,
    delim = " ",
    names = c("date", "time")
  ) %>%
  mutate(
    date = as.Date(date, format = "%d/%m/%Y"),
    classe_probabilite_impact = factor(
      classe_probabilite_impact,
      levels = c("tres_faible", "faible", "moyen", "fort", "tres_fort")
    ),
    taille = as.numeric(taille)
  ) %>%
  mutate(across(where(is.character), ~ na_if(., "/"))) %>%
  rename(
    lon_x = longitude_position_degres_decimaux,
    lat_y = latitude_position_degres_decimaux
  )

## Check structure 2 ----

# Récurrence observations noms de bateaux
t1 <- donia %>%
  group_by(nom) %>%
  summarize(n = n()) %>%
  arrange(nom, desc(n))

# Nombre bateaux nommées vs bateaux sans nom
t1sum <- t1 %>%
  mutate(type = case_when(
    is.na(nom) ~ "unnamed",
    TRUE ~ "named"
  )) %>%
  group_by(type) %>%
  summarize(n = sum(n, na.rm = FALSE))

t1sum # 1628 bateaux sans noms
t1sum$n[2] * 100 / sum(t1sum$n) # Proportion 16.5% de bateaux sans noms

# Récurrence bateaux uniques par jour ?
t2 <- donia %>%
  group_by(nom, date) %>%
  summarize(n = n()) %>%
  arrange(nom, desc(n), date)

# Récupération des types uniques de navires pour lien RESOBLO
type_navire <- donia %>%
  select(type_navire, type_navire_brut) %>%
  group_by_all() %>%
  summarize(n = n()) %>%
  arrange(desc(type_navire), desc(n))

# Sauvegarde csv types navires -> pour création reférence resoblo-donia
write.csv2(type_navire, paths$processed_donia_type_navire)

# Nombre de navires avec vs sans type défini
type_unknown <- type_navire %>%
  mutate(type_navire = case_when(
    type_navire == "Unknown" ~ "type_absent",
    is.na(type_navire) ~ "type_absent",
    TRUE ~ "type_existe"
  )) %>%
  group_by(type_navire) %>%
  summarize(n = sum(n, na.rm = FALSE))

type_unknown # 6142 bateaux sans type !
type_unknown$n[1] * 100 / sum(type_unknown$n) # 62% des données inutilisables
type_unknown$n[2] * 100 / sum(type_unknown$n) # Donc seulement 38% utilisables...

# Infos masses d'eau
unique(donia$nom_masse_eau)

# Contenu colonnes texte vers minuscules
donia <- donia %>%
  mutate(across(
    c(type_navire_brut, type_navire, nom_masse_eau, region),
    str_to_lower
  ))


# Investigation visuelle cartographique ----

# Transfo vers format spatial
donia_spatial <- st_as_sf(donia, coords = c("lon_x", "lat_y"), crs = 4326)

## Visualisation par variables ----

carto <- FALSE

if (carto) {
  # Création des cartes via fonction personnalisée
  map_region <- category_map(donia_spatial, "region")
  map_type <- category_map(donia_spatial, "type_navire")
  map_sous_type <- category_map(donia_spatial, "type_navire_brut")
  map_annee <- category_map(donia_spatial, "annee_mouillage")
  map_masse_eau <- category_map(donia_spatial, "code_masse_eau")
  map_proba_impact <- category_map(donia_spatial, "probabilite_impact", compressor = 10)
  map_duree_mouillage <- category_map(donia_spatial, "duree_mouillage", compressor = 100)
  map_taille <- category_map(donia_spatial, "taille", compressor = 1)

  # Visualisation des cartes interactives (choix lancement ligne pour visualisation)
  map_region
  map_type
  map_sous_type
  map_annee
  map_masse_eau
  map_proba_impact
  map_duree_mouillage
  map_taille

  ## Visualisations des types de navires ----

  # Cargos
  donia_spatial %>%
    filter(type_navire_brut %in% c("cargo ship", "general cargo ship")) %>%
    category_map(., "type_navire_brut")

  # Transport de passagers
  donia_spatial %>%
    filter(type_navire_brut %in% c("passenger ship", "passenger (cruise) ship", "passenger/ro-ro cargo ship")) %>%
    category_map(., "type_navire_brut")

  # Plongée
  donia_spatial %>%
    filter(type_navire_brut %in% c("diving ops")) %>%
    category_map(., "type_navire_brut")

  # Entrainement
  donia_spatial %>%
    filter(type_navire_brut %in% c("training ship")) %>%
    category_map(., "type_navire_brut")

  # Yacht
  donia_spatial %>%
    filter(type_navire_brut %in% c("yacht")) %>%
    category_map(., "taille")
}


# Ajout infos RESOBLO ----

## Jointure codes ----

# Ajout colonne code Resoblo niveau le plus précis
donia_resoblo <- donia_resoblo %>%
  mutate(
    resoblo_intitule = case_when(
      !is.na(resoblo_intitule_n1) ~ resoblo_intitule_n1,
      TRUE ~ resoblo_intitule_n2
    ),
    resoblo_code = case_when(
      !is.na(resoblo_code_n1) ~ resoblo_code_n1,
      TRUE ~ resoblo_code_n2
    ),
    resoblo_niveau = case_when(
      !is.na(resoblo_code_n1) ~ 1,
      TRUE ~ 2
    )
  )

# Jointure codes Resoblo à Donia
donia <- donia %>%
  filter(!is.na(type_navire_brut)) %>% # Navires sans type brut enlevés
  left_join(., donia_resoblo, by = join_by("type_navire_brut"))

# Check nombre observations niveau 2
t1 <- donia %>%
  select(resoblo_code_n2, resoblo_intitule_n2) %>%
  group_by_all() %>%
  summarize(n = n())

# Catégories RESOBLO avec NA au niveau 2
t2 <- donia %>%
  filter(is.na(resoblo_code_n2)) %>%
  select(
    resoblo_intitule_n2,
    type_navire_brut,
    type_navire
  ) %>%
  group_by_all() %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# Elimination des entités sans code RESOBLO existant
donia <- donia %>%
  filter(!is.na(resoblo_intitule_n2))

# Check type entités restantes
t3 <- donia %>%
  select(
    type_navire,
    type_navire_brut,
    resoblo_intitule_n2,
    resoblo_intitule_n1,
    resoblo_code_n2,
    resoblo_code_n1
  ) %>%
  group_by_all() %>%
  summarize(n = n()) %>%
  select(n, everything()) %>%
  arrange(desc(n)) %>%
  ungroup()

View(t3)

# Recheck complétion Donia
skimr::skim(donia) # Quelques bateaux sans taille !

# Check catégories sans tailles
t4 <- donia %>%
  filter(is.na(taille)) %>%
  select(
    type_navire_brut,
    resoblo_intitule_n2,
    resoblo_intitule_n1,
  ) %>%
  group_by_all() %>%
  summarize(n_na = n()) %>%
  arrange(desc(n_na)) %>%
  ungroup()

# Taux de complétion taille par catégorie
t4 <- t3 %>%
  filter(type_navire_brut %in% t4$type_navire_brut) %>%
  select(type_navire_brut, n) %>%
  left_join(., t4) %>%
  mutate(
    resoblo_intitule = case_when(
      !is.na(resoblo_intitule_n1) ~ resoblo_intitule_n1,
      TRUE ~ resoblo_intitule_n2
    ),
    taux_completion_taille = round(((n - n_na) * 100) / n, 1)
  ) %>%
  select(type_navire_brut, resoblo_intitule, n, n_na, taux_completion_taille) %>%
  rename(na_taille = n_na)

View(t4)
sum(t4$na_taille) # 195 bateaux sans taille

View(donia)
skimr::skim(donia)


# Dernières modifications
donia <- donia %>%
  rename(nom_bateau = nom) %>%
  relocate(c(resoblo_intitule, resoblo_code), .after = nom_bateau)


# Spatialisation ----

# Spatialisation WSG84
donia_spatial <- st_as_sf(donia, coords = c("lon_x", "lat_y"), crs = 4326)

# Export format gpkg
st_write(
  obj = donia_spatial,
  dsn = paste0(paths$processed_obs_donia),
  driver = "GPKG",
  append = FALSE
)

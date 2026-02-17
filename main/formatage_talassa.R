#' ---
#' title : "talassaR - formatage_talassa"
#' author : Aubin Woehrel
#' creation date : 2026-02-06
#' ---
#'
#' =============================================================================
#'
#' talassaR : Formatage des données observatoire en format TALASSA
#'
#' Description :
#' Script permettant de passer les jeux de données corrigés au format
#' observatoire en jeux de données au format TALASSA.
#' Consiste à :
#' 1) Eliminer les colonnes inutiles
#' 2) Uniformiser certaines colonnes si besoin
#' 3) Remplacer les codes et intitulés RESOBLO par les codes TALASSA
#' 4) Exporter au format gpkg (avec crs 4326)
#'
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
library("openxlsx")

# Data: Spatial
library("sf")
library("leaflet")
library("rlang")

## Ressources locales ----
source("r/paths.R")
source("r/fct_category_map.R")

## Import des données ----

# Référence codes RESOBLO-TALASSA
codes_talassa <- read.xlsx(
  xlsxFile = paths$raw_codes_talassa,
  sheet = "codes",
  fillMergedCells = TRUE,
  startRow = 2
)

# Activités format observatoire corrigé
survolus_obs <- st_read(paths$processed_obs_survolusage)
peche_obs <- st_read(paths$processed_obs_peche)
donia_obs <- st_read(paths$processed_obs_donia)
plongee_obs <- st_read(paths$processed_obs_plongee)

# Délimitation PNMCCA
pnm_borders <- sf::st_read(paths$raw_pnmcca_borders) %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(NOM_SITE == "cap Corse et Agriate")

# Transformations initiales ----
codes_talassa <- codes_talassa %>%
  select(resoblo_intitule_n1:talassa_commentaires) %>%
  select(-resoblo_precision_n0) %>%
  filter(!is.na(talassa_code))

View(codes_talassa)

# Création liens des codes en éliminant les potentielles répétitions
# des codes TALASSA pour plusieurs codes RESOBLO détaillés
codes_liens <- codes_talassa %>%
  select(code_resoblo_plus_proche, talassa_code, talassa_intitule) %>%
  distinct() %>%
  filter(!is.na(code_resoblo_plus_proche))


# Modification activites ----

## Modification survols usages ----

survolus_bool <- unique(survolus_obs$resoblo_code) %in% codes_talassa$code_resoblo_plus_proche

if (sum(!survolus_bool) != 0) {
  simpleWarning("Codes Talassa survols non valides, données non enregistrées.")
}

plongee_talassa <- left_join(
  x = survolus_obs,
  y = codes_liens,
  by = join_by(resoblo_code == code_resoblo_plus_proche)
)


## Modification plongee ----
plongee_bool <- unique(plongee_obs$resoblo_code_n1) %in% codes_talassa$code_resoblo_plus_proche

if (sum(!plongee_bool) != 0) {
  simpleWarning("Codes Talassa plongée non valides, données non enregistrées.")
}

plongee_talassa <- left_join(
  x = plongee_obs,
  y = codes_liens,
  by = join_by(resoblo_code_n1 == code_resoblo_plus_proche)
)

plongee_talassa <- plongee_talassa %>%
  select(-c(resoblo_intitule_n1, resoblo_code_n1)) %>%
  relocate(c(talassa_intitule, talassa_code), .after = id_prest)


## Modification pêche ----
peche_bool <- unique(peche_obs$resoblo_code) %in% codes_talassa$code_resoblo_plus_proche

if (sum(!peche_bool) != 0) {
  simpleWarning("Codes Talassa plongée non valides, données à vérifier.")
}

# Jointure codes TALASSA au jeu de données peche
peche_talassa <- left_join(
  x = peche_obs,
  y = codes_liens,
  by = join_by(resoblo_code == code_resoblo_plus_proche)
)

# Verification des correspondances de codes
peche_verif_liens <- peche_talassa %>%
  st_drop_geometry() %>%
  select(resoblo_intitule, talassa_intitule, resoblo_code, talassa_code) %>%
  distinct() %>%
  arrange(talassa_intitule)
View(peche_verif_liens)

# Elimination des colonnes inutiles
peche_talassa <- peche_talassa %>%
  select(
    id_obs,
    talassa_intitule,
    talassa_code,
    date,
    nb_pecheur,
    prof_m,
    temps_pech,
    temps_pe_1
  )

## Modification donia ----

# Check bateaux sans taille
donia_obs %>%
  filter(is.na(taille)) %>%
  dim(.) # 195 entités sans taille

# Elimination des bateaux sans taille
donia_obs <- donia_obs %>%
  filter(!is.na(taille))

# Check presence
donia_bool <- unique(donia_obs$resoblo_code) %in% codes_talassa$code_resoblo_plus_proche


# Check codes Talassa donia non valides
if (sum(!donia_bool) != 0) {
  simpleWarning("Codes Talassa Donia non valides, données non enregistrées.")

  check_donia <- donia_obs %>%
    st_drop_geometry() %>%
    filter(resoblo_code %in% unique(donia_obs$resoblo_code)[!donia_bool]) %>%
    select(resoblo_intitule, resoblo_code, resoblo_niveau) %>%
    distinct()

  View(check_donia) # Infos activitiés sans lien matrice Talassa -> éliminées par la suite
}

# Jointure donia talassa
donia_talassa <- left_join(
  x = donia_obs,
  y = codes_liens,
  by = join_by(resoblo_code == code_resoblo_plus_proche)
) %>%
  relocate(talassa_code, .after = resoblo_code) %>%
  relocate(talassa_intitule, .after = resoblo_intitule)

# Comparaison des dimensions pour verif bonne jointure
dim(donia_obs)
dim(donia_talassa)

# Vérification plus approfondie de la jointure des codes
donia_verif_liens <- donia_talassa %>%
  st_drop_geometry() %>%
  select(resoblo_intitule, talassa_intitule, resoblo_code, talassa_code) %>%
  distinct() %>%
  arrange(talassa_intitule)
View(donia_verif_liens)

# Elimination des entités sans codes TALASSA
donia_talassa <- donia_talassa %>%
  filter(!is.na(talassa_intitule))

# Check des dimensions finales
dim(donia_talassa)
dim(donia_obs)[1] - dim(donia_talassa)[1] # 105 entités éliminées

# Elimination des colonnes inutiles
donia_talassa <- donia_talassa %>%
  select(-contains("resoblo")) %>%
  select(-c(
    type_navire_brut,
    type_navire,
    time,
    region,
    code_masse_eau,
    nom_masse_eau,
    nom_bateau,
    probabilite_impact,
    classe_probabilite_impact
  ))

# Modification habitats ----

# Exports ----

# Survols usages
st_write(
  obj = survolus_talassa,
  dsn = paths$processed_tal_pts_survolusage,
  driver = "GPKG",
  append = FALSE
)

# Peche de loisir
st_write(
  obj = peche_talassa,
  dsn = paths$processed_tal_pts_peche,
  driver = "GPKG",
  append = FALSE
)

# Sites plongée
st_write(
  obj = plongee_talassa,
  dsn = paths$processed_tal_pts_plongee,
  driver = "GPKG",
  append = FALSE
)

# Donia
st_write(
  obj = donia_talassa,
  dsn = paths$processed_tal_pts_donia,
  driver = "gpkg",
  append = FALSE
)

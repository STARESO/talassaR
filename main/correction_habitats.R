#' ---
#' title : "talassaR - correction_habitats"
#' author : Aubin Woehrel
#' creation date : 2026-02-06
#' ---
#'
#' =============================================================================
#'
#' talassaR : Correction des habitats
#'
#' Description :
#' Script de correction des couches habitats de biocénose d'andromède.
#' Les corrections ont été réalisées principalement sur QGIS, mais ce script
#' permet de faire les étapes suivantes en plus :
#' 1) de vérifier la nomenclature des contenus de colonnes
#' 2) de joindre les codes d'identification nathab OFB officiels des biocénoses
#' 3) d'exporter les fichiers directement au format talassa et au format nathab
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
library("openxlsx")


# Spatial
library("sf")

## Ressources locales ----
source("r/paths.R")


## Import des données -----

# Habitats
habitats <- st_read(paths$raw_habitats_andromede)
grottes <- st_read(paths$raw_habitats_grottes)

codes_habitats <- read.xlsx(
  xlsxFile = paths$raw_codes_habitats,
  sheet = "codes",
  fillMergedCells = TRUE
)

# Check structure ----

# Biocénoses andromède
str(habitats)
names(habitats)

for (col in names(habitats)[-8]) {
  print("------------------------")
  print(col)
  habitats %>%
    st_drop_geometry() %>%
    pull(col) %>%
    unique() %>%
    print()
}

# Biocénoses grottes
str(grottes)
names(grottes)

# Modifications ----

## Biocénoses andromède ----

# Check validite référence codes - intitulés andromède
unique(habitats$surfstat) %in% codes_habitats$surfstat

# Jointure données habitats et codes habitats
habitats <- habitats %>%
  left_join(
    x = .,
    y = codes_habitats,
    by = "surfstat"
  )

dim(habitats)
head(habitats)

# Passage à un meilleur nom surfstat
habitats <- habitats %>%
  select(-surfstat) %>%
  rename(surfstat = surfstat_better)

# Format nathab
habitats_nathab <- habitats %>%
  select(-c(contains("talassa"), commentaire)) %>%
  relocate(surfstat, nathab_code, nathab_intitule, .before = EU_CD)

# Format talassa
habitats_talassa <- habitats %>%
  select(contains("talassa"), date, geometry)

## Biocénoses grottes ----

# Recodage
grottes <- grottes %>%
  mutate(type = recode_values(
    x = Type,
    "O" ~ "Grottes obscurite totale",
    "SO" ~ "Grottes semi obscures"
  ))

# Jointure codes nathab et talassa
grottes <- left_join(
  x = grottes,
  y = codes_habitats,
  by = join_by(type == surfstat_better)
)

# Réorganisation
grottes <- grottes %>%
  select(-c(Type, surfstat, commentaire))

# Format nathab
grottes_nathab <- grottes %>%
  select(-c(talassa_code, talassa_intitule))

# Format talassa
grottes_talassa <- grottes %>%
  select(-c(type, nathab_code, nathab_intitule))

# Exports ----

# Habitats nathab
st_write(
  obj = habitats_nathab,
  dsn = paths$processed_obs_habitats,
  driver = "gpkg",
  append = FALSE
)

# Habitats talassa
st_write(
  obj = habitats_talassa,
  dsn = paths$processed_tal_habitats,
  driver = "gpkg",
  append = FALSE
)

# Grottes nathab
st_write(
  obj = grottes_nathab,
  dsn = paths$processed_obs_grottes,
  driver = "gpkg",
  append = FALSE
)

# Grottes talassa
st_write(
  obj = grottes_talassa,
  dsn = paths$processed_tal_grottes,
  driver = "gpkg",
  append = FALSE
)

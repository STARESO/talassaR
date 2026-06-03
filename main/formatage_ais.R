#' ---
#' title : "talassaR - formatage_ais"
#' author : Aubin Woehrel
#' creation date : 2026-06-03
#' ---
#'
#' =============================================================================
#'
#' talassaR : Formatage des données AIS observatoire vers format TALASSA
#'
#' Description :
#' Script de passage des pings AIS du format observatoire au format TALASSA.
#' Utilise les codes RESOBLO intégrés durant correction_ais.
#'
#' =============================================================================

# Initialisation ----

## Nettoyage ----
rm(list = ls())

## Import des librairies ----
library("dplyr")
library("sf")
library("openxlsx")
library("yaml")

## Ressources locales ----
paths <- yaml::read_yaml("config/paths.yml")

## Import des données ----

# Référence codes RESOBLO-TALASSA
codes_talassa <- read.xlsx(
  xlsxFile = paths$raw$codes_talassa,
  sheet = "codes",
  fillMergedCells = TRUE,
  startRow = 2
)

# AIS format observatoire (avec codes RESOBLO n1 et n2)
ais_obs <- st_read(paths$processed$observatoire_ais)

# Nettoyage codes talassa ----
codes_talassa <- codes_talassa %>%
  select(resoblo_intitule_n1:talassa_commentaires) %>%
  select(-resoblo_precision_n0) %>%
  filter(!is.na(talassa_code))

# Création liens codes
codes_liens <- codes_talassa %>%
  select(code_resoblo_plus_proche, talassa_code, talassa_intitule) %>%
  distinct() %>%
  filter(!is.na(code_resoblo_plus_proche))

# Jointure codes TALASSA aux données AIS ----

# Utiliser resoblo_code_n2 si disponible, sinon resoblo_code_n1
ais_obs<- ais_obs %>%
  mutate(
    resoblo_code = coalesce(resoblo_code_n1, resoblo_code_n2),
    resoblo_intitule = coalesce(resoblo_intitule_n1, resoblo_intitule_n2)
  ) %>%
  mutate(
    resoblo_code = case_when(
      resoblo_code == "RECM.03.F02.A01" ~ "RECM.03.F02", # Plaisance en yacht à moteur vers grande plaisance
      resoblo_code == "RECM.03.F02.A02" ~ "RECM.03.F02", # Plaisance en yacht à voile vers grande plaisance
      TRUE ~ resoblo_code
    ), 
    resoblo_intitule = case_when(
      grepl("plaisance en yacht", resoblo_intitule, ignore.case = TRUE) ~ "grande plaisance",
      TRUE ~ resoblo_intitule
    )
  )

# Check ais obs
ais_obs %>% 
  st_drop_geometry() %>%
  distinct(resoblo_code, resoblo_intitule)

# Jointure codes talassa
ais_talassa <- ais_obs %>%
  left_join(., codes_liens, by = join_by(resoblo_code == code_resoblo_plus_proche))

# Check des correspondancess
check_correspondance <- ais_talassa %>% 
  st_drop_geometry() %>%
  count(resoblo_code, talassa_code, resoblo_intitule, talassa_intitule)
View(check_correspondance)

# Élimination pings sans code TALASSA
ais_talassa <- ais_talassa %>%
  filter(!is.na(talassa_code))

# Sélection colonnes utiles
ais_talassa <- ais_talassa %>%
  select(
    mmsi,
    heading,
    timestamp,
    date,
    speed,
    flag,
    length,
    width,
    ship_country,
    talassa_code,
    talassa_intitule,
    geom
  )

# Export ----
st_write(
  obj = ais_talassa,
  dsn = paths$processed$talassa_ais,
  driver = "GPKG",
  append = FALSE
)

cat("Export réussi vers :", paths$processed$talassa_ais, "\n")

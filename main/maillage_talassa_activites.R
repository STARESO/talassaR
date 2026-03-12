#' ---
#' title : "talassaR - maillage_talassa_activites"
#' author : Aubin Woehrel
#' creation date : 2026-02-12
#' ---
#'
#' =============================================================================
#'
#' talassaR : Maillage des données activités
#'
#' Description :
#' Script permettant de passer des données activités ponctuelles au format
#' TALASSA vers des données activités intégrées au maillage TALASSA choisi.
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
library("purrr")
library("openxlsx")

# Spatial
library("sf")

## Ressources locales ----
source("r/paths.R")
source("r/fct_jointure_id.R")


## Import des données -----

# Activités
survolus_talassa <- st_read(paths$processed_tal_survolusage)
peche_talassa <- st_read(paths$processed_tal_peche)
donia_talassa <- st_read(paths$processed_tal_donia)
plongee_talassa <- st_read(paths$processed_tal_plongee)

# Carroyages
carroyage_hex <- st_read(paths$raw_carroyage_final) %>%
  st_transform(., crs = 4326)

# Precheck ----
names(survolus_talassa)
names(peche_talassa)
names(donia_talassa)
names(plongee_talassa)


# Liste des jeux de données et leurs noms
data_list <- list(
  list(df = survolus_talassa, source = "survol_usage"),
  list(df = peche_talassa, source = "peche"),
  list(df = donia_talassa, source = "donia"),
  list(df = plongee_talassa, source = "plongee")
)

# Fonction pour obtenir les uniques des variables par jeux de données
process_df <- function(df, source) {
  vars <- df %>%
    st_drop_geometry() %>%
    select(-contains("talassa")) %>%
    names() %>%
    str_flatten(collapse = ", ")

  df %>%
    st_drop_geometry() %>%
    count(talassa_code, talassa_intitule) %>%
    mutate(
      source = source,
      variables = vars
    )
}

# Application de la fonction pour comparer les codes et variables à disposition
# dans l'intégralité des jeux de données à disposition
all_codes <- data_list %>%
  map(~ process_df(.x$df, .x$source)) %>%
  list_rbind() %>%
  arrange(talassa_code)

# Export des combinaisons de codes et jeux de données en format xlsx.
all_codes %>%
  select(-n) %>%
  mutate(
    formule = "inserer formule",
    ic = "inserer interfalle de confiance"
  ) %>%
  relocate(variables, .after = last_col()) %>%
  write.xlsx(
    x = .,
    file = paths$raw_devcarroyage_activites,
    sheetName = "ref"
  )

# Permet de compléter manuellement l'excel avec les formules de calcul
# d'agrégation et les intervalles de confiance pour la suite des étapes.
# Reprendre la suite après complétion de la référence puis renomer selon
# le chemin paths$raw_refcarroyage_activites
paths$raw_refcarroyage_activites

formula_ref <- read.xlsx(
  xlsxFile = paths$raw_refcarroyage_activites,
  sheet = "ref"
)

# Jointure ID maillage ----

# Jointure avec fonction personnalisée jointure_id (cf fct_jointure_id.R)
resultats_carroyage <- map(
  .x = list(survolus_talassa, peche_talassa, donia_talassa, plongee_talassa),
  .f = ~ jointure_id(carroyage = carroyage_hex, data = .x, id_name = "id_hex")
)

# Reassignation (format liste sortie map vers jeux de données)
survolus_carroyage <- resultats_carroyage[[1]]
peche_carroyage <- resultats_carroyage[[2]]
donia_carroyage <- resultats_carroyage[[3]]
plongee_carroyage <- resultats_carroyage[[4]]

# Check des dimensions
dim(survolus_talassa)
dim(survolus_carroyage)

dim(peche_talassa)
dim(peche_carroyage)

dim(donia_talassa)
dim(donia_carroyage)

dim(plongee_talassa)
dim(plongee_carroyage)


# Agrégation par activités (au sein de chaque jeu de données) ----
survolus_carroyage <- formula_ref %>%
  filter(source == "survol_usage") %>%
  select(talassa_code, formule) %>%
  left_join(
    x = survolus_carroyage,
    y = .,
    by = join_by(talassa_code)
  )

peche_carroyage <- formula_ref %>%
  filter(source == "peche") %>%
  select(talassa_code, formule) %>%
  left_join(
    x = peche_carroyage,
    y = .,
    by = join_by(talassa_code)
  )

donia_carroyage <- formula_ref %>%
  filter(source == "donia") %>%
  select(talassa_code, formule) %>%
  left_join(
    x = donia_carroyage,
    y = .,
    by = join_by(talassa_code)
  )

plongee_carroyage <- formula_ref %>%
  filter(source == "plongee") %>%
  select(talassa_code, formule) %>%
  left_join(
    x = plongee_carroyage,
    y = .,
    by = join_by(talassa_code)
  )

test <- survolus_carroyage
unique(test$formule)


# Fonction de calcul des intensités d'activités par maille et par activité
intensite_computation <- function(
  data,
  id_carroyage = "id_hex",
  scale_min = 0.01,
  scale_max = 1
) {
  # Computing weights per entity based on formula and available variables
  data_new <- data %>%
    rowwise() %>%
    mutate(
      cweight = ifelse(
        is.na(formule),
        NA,
        eval(parse(text = formule))
      )
    )

  names(data_new)

  # Computing intensity based on weights
  data_new <- data_new %>%
    # Sum per cell and activity
    group_by(.data[[id_carroyage]], talassa_code) %>%
    summarize(intensite = sum(cweight)) %>%
    # Rescaling by activity
    group_by(talassa_code) %>%
    mutate(intensite = scales::rescale(intensite, to = c(scale_min, scale_max))) %>%
    arrange(talassa_code, intensite)
}

test <- intensite_computation(data = survolus_carroyage)




# Agregation d'intensité d'activité entre jeux de données ----

# Jointure spatiale ID activités - hexagones ----

# Agrégation intervalles de confiance ----

# Jointure Intensités et IC ----

# Exports ----

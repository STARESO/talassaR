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
#' Note : ic == indice de confiance
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
  select(talassa_code, formule, ic) %>%
  left_join(
    x = survolus_carroyage,
    y = .,
    by = join_by(talassa_code)
  )

peche_carroyage <- formula_ref %>%
  filter(source == "peche") %>%
  select(talassa_code, formule, ic) %>%
  left_join(
    x = peche_carroyage,
    y = .,
    by = join_by(talassa_code)
  )

donia_carroyage <- formula_ref %>%
  filter(source == "donia") %>%
  select(talassa_code, formule, ic) %>%
  left_join(
    x = donia_carroyage,
    y = .,
    by = join_by(talassa_code)
  )

plongee_carroyage <- formula_ref %>%
  filter(source == "plongee") %>%
  select(talassa_code, formule, ic) %>%
  left_join(
    x = plongee_carroyage,
    y = .,
    by = join_by(talassa_code)
  )

# Fonction de calcul des intensités d'activités par maille et par activité
intensite_computation <- function(
  data,
  type,
  id_carroyage = "id_hex",
  scale_min = 0.01,
  scale_max = 1
) {
  # Calcul des poids par entités sur la base de formules et variables à disposition
  data_new <- data %>%
    rowwise() %>%
    mutate(
      cweight = ifelse(
        is.na(formule),
        1, # A verifier si ok dans toutes les situations
        eval(parse(text = formule))
      )
    )

  names(data_new)

  # Calcul du niveau d'intensité basé sur les poids
  data_new <- data_new %>%
    # Somme par cellule et par activité
    group_by(.data[[id_carroyage]], talassa_code, talassa_intitule) %>%
    summarize(
      intensite = sum(cweight),
      ic = unique(ic) # Ici ou autre endroit plus adapté ?
    ) %>%
    # Réechelonnage par activité entre valeurs scale_min et scale_max
    group_by(talassa_code, talassa_intitule) %>%
    mutate(intensite = scales::rescale(intensite, to = c(scale_min, scale_max))) %>%
    arrange(talassa_code, intensite)

  # Ajout de la colonne type pour les étapes prochaines
  data_new <- data_new %>%
    mutate(type = type)
}

# Passage des temps de pêche depuis format caractères HMS vers difftime en secondes vers numérique
peche_carroyage <- peche_carroyage %>%
  mutate(across(
    .cols = c(temps_pech, temps_pe_1),
    .fns = function(x) {
      as.numeric(as.difftime(x, units = "secs"))
    }
  ))

survolus_carroyage2 <- intensite_computation(data = survolus_carroyage, type = "survolus")
peche_carroyage2 <- intensite_computation(data = peche_carroyage, type = "peche")
donia_carroyage2 <- intensite_computation(data = donia_carroyage, type = "donia")
plongee_carroyage2 <- intensite_computation(data = plongee_carroyage, type = "plongee")

# Agregation d'intensité d'activité entre jeux de données ----
agregation_carroyage <- rbind(
  survolus_carroyage2,
  peche_carroyage2,
  donia_carroyage2,
  plongee_carroyage2
)

# Check du nombre d'occurences avec 1 ou plus de 1 jeux de données pour une combinaison
# activité-id_carroyage
nombre_agregations <- agregation_carroyage %>%
  count(id_hex, talassa_code, name = "nombre_jeux_données") %>%
  ungroup() %>%
  count(nombre_jeux_données)

View(nombre_agregations) # total de 188 hexagones avec combinaison de sources de données

# Calcul moyenne intensité d'activité
agregation_carroyage <- agregation_carroyage %>%
  group_by(id_hex, talassa_code, talassa_intitule) %>%
  summarize(
    intensite = mean(intensite),
    ic = min(ic) # IC pris comme le minimum de l'IC
  )

# Verification nombre agregations
nombre_agregations <- agregation_carroyage %>%
  count(id_hex, talassa_code, name = "nombre_jeux_données") %>%
  ungroup() %>%
  count(nombre_jeux_données)

View(nombre_agregations) # Ok : 1 valeur uniquement par combinaison activité-id_carroyage

# Organisation par codes talassa
agregation_carroyage <- agregation_carroyage %>%
  arrange(talassa_code, id_hex) %>%
  ungroup()


## Version noms de colonnes -> codes talassa ----

# Passage au format large (colonnes par code activité)
agregation_longer <- agregation_carroyage %>%
  select(-talassa_intitule) %>%
  pivot_longer(cols = c(intensite, ic)) %>%
  mutate(name = case_when(
    name == "intensite" ~ talassa_code, # code_talassa
    TRUE ~ paste0(talassa_code, "_iq") # code_talassa + ic
  )) %>%
  select(-talassa_code) %>%
  arrange(name, id_hex)

agregation_wider <- agregation_longer %>%
  pivot_wider(
    names_from = name,
    values_from = value,
    values_fill = 0
  )

# Jointure spatiale ID activités - hexagones ----

# Jointure
agregation_hex <- left_join(
  x = carroyage_hex,
  y = agregation_wider,
  by = join_by(id_hex)
)

# Transfo format final
agregation_hex <- agregation_hex %>%
  select(-c(left, top, right, bottom)) %>%
  mutate(across(-c(geometry, id_hex), ~ coalesce(., 0))) %>% # Attention voir si remplacement NA pertinent
  st_as_sf()

# Changement nom de la colonne d'id vers id2 (nomenclature modèle)
agregation_hex <- agregation_hex %>%
  rename_with(~ "id2", starts_with("id"))


## Version noms de colonnes -> intituleés talassa ----

# Passage au format large (colonnes par code activité)
agregation_longer2 <- agregation_carroyage %>%
  select(-talassa_code) %>%
  mutate(talassa_intitule = str_replace_all(talassa_intitule, " ", "_")) %>%
  pivot_longer(cols = c(intensite, ic)) %>%
  mutate(name = case_when(
    name == "intensite" ~ talassa_intitule, # code_talassa
    TRUE ~ paste0(talassa_intitule, "_iq") # code_talassa + ic
  )) %>%
  select(-talassa_intitule) %>%
  arrange(name, id_hex)


agregation_wider2 <- agregation_longer2 %>%
  pivot_wider(
    names_from = name,
    values_from = value,
    values_fill = 0
  )

# Jointure spatiale ID activités - hexagones ----

# Jointure
agregation_hex2 <- left_join(
  x = carroyage_hex,
  y = agregation_wider2,
  by = join_by(id_hex)
)

# Transfo format final
agregation_hex2 <- agregation_hex2 %>%
  select(-c(left, top, right, bottom)) %>%
  mutate(across(-c(geometry, id_hex), ~ coalesce(., 0))) %>% # Attention voir si remplacement NA pertinent
  st_as_sf()

# Changement nom de la colonne d'id vers id2 (nomenclature modèle)
agregation_hex2 <- agregation_hex2 %>%
  rename_with(~ "id2", starts_with("id"))

# Exports ----
st_write(
  obj = agregation_hex,
  dsn = paths$processed_hex_activites,
  driver = "gpkg",
  append = FALSE
)

st_write(
  obj = agregation_hex2,
  dsn = paths$processed_hex_activites_intitule,
  driver = "gpkg",
  append = FALSE
)

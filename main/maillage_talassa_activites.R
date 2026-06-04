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
paths <- yaml::read_yaml("config/paths.yml")
source("r/fct_jointure_id.R")
source("r/fct_intensite_computation.R")

## Choix du type de carroyage pour faire tourner le script ----
choice_carroyage <- "arp" # Choix voir condition if ci-dessous

## Import des données -----

# Activités (données ponctuelles format talassa)
survolus_talassa <- st_read(paths$processed$talassa_survolusage)
peche_talassa <- st_read(paths$processed$talassa_peche)
donia_talassa <- st_read(paths$processed$talassa_donia)
plongee_talassa <- st_read(paths$processed$talassa_plongee)

# Paramètres spécifique au maillage
if (choice_carroyage == "hex5") { # Carroyage hexagonal cinquième de mile nautique
  carroyage <- st_read(paths$raw$carroyage_hexcinquieme) %>% rename(id2 = "id_hex") # Carroyage
  ais_talassa <- st_read(paths$processed$talassa_ais_grid_hex) # AIS
  path_final <- paths$processed$hex_activites # Export activités version codes talassa
  path_final_wider <- paths$processed$hex_activites_intitule # Export activités version intitules talassa

} else if (choice_carroyage == "arp") { # Carroyage carré 1 mile nautique (modèle arp)
  carroyage <- st_read(paths$raw$carroyage_arp)
  ais_talassa <- st_read(paths$processed$talassa_ais_grid_arp)
  path_final <- paths$processed$arp_activites # Export activités version codes talassa
  path_final_wider <- paths$processed$arp_activites_intitule # Export activités version intitules talassa

} else {
  stop("Mauvais type de carroyage choisi. Choisir parmis ceux disponibles.")
}


# Precheck ----
if ("geom" %in% names(carroyage)) {
  carroyage <- carroyage %>%
    rename(geometry = geom)
}

# Transformation crs carroyage vers 4326
carroyage <- carroyage %>% 
  st_transform(crs = 4326) %>%
  select(id2, geometry)

names(survolus_talassa)
names(peche_talassa)
names(donia_talassa)
names(plongee_talassa)
names(ais_talassa)

write_codes_activites <- FALSE

if (write_codes_activites) {
  # Liste des jeux de données et leurs noms
  data_list <- list(
    list(df = survolus_talassa, source = "survol_usage"),
    list(df = peche_talassa, source = "peche"),
    list(df = donia_talassa, source = "donia"),
    list(df = plongee_talassa, source = "plongee"), 
    list(df = ais_talassa, source = "ais")
  )

  # Comparaison codes et variables à disposition
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
      file = paths$raw$devcarroyage_activites,
      sheetName = "ref"
    )

  # Permet de compléter manuellement l'excel avec les formules de calcul
  # d'agrégation et les intervalles de confiance pour la suite des étapes.
  # Reprendre la suite après complétion de la référence puis renomer selon
  # le chemin paths$raw$refcarroyage_activites
}


# Lecture formula ref à jour
formula_ref <- read.xlsx(
  xlsxFile = paths$raw$refcarroyage_activites,
  sheet = "ref"
)

# Jointure ID maillage ----

# Jointure avec fonction personnalisée jointure_id (cf fct_jointure_id.R)
resultats_carroyage <- map(
  .x = list(survolus_talassa, peche_talassa, donia_talassa, plongee_talassa),
  .f = ~ jointure_id(carroyage = carroyage, data = .x, id_name = "id2")
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

# Passage des temps de pêche depuis format caractères HMS vers difftime en secondes vers numérique
peche_carroyage <- peche_carroyage %>%
  mutate(across(
    .cols = c(temps_pech, temps_pe_1),
    .fns = function(x) {
      as.numeric(as.difftime(x, units = "secs"))
    }
  ))


# Finalisation préparation données AIS pour utilisation intensite_computation
# Intégration données AIS déjà maillées dans le script maillage_talassa_ais.R
ais_carroyage <- ais_talassa %>%
  st_drop_geometry() %>%
  select(id2, talassa_code, talassa_intitule, activity_value)

# Liste ref données ais (formume et ic)
ais_formula_ref <- formula_ref %>%
  filter(source == "ais") %>%
  select(talassa_code, formule, ic)

# Jointure des ic et formule
ais_carroyage <- left_join(
  x = ais_carroyage,
  y = ais_formula_ref,
  by = "talassa_code"
)

# Calculs intensité par maille pour les principaux datasets
survolus_carroyage2 <- intensite_computation(data = survolus_carroyage, type = "survolus")
peche_carroyage2 <- intensite_computation(data = peche_carroyage, type = "peche")
donia_carroyage2 <- intensite_computation(data = donia_carroyage, type = "donia")
plongee_carroyage2 <- intensite_computation(data = plongee_carroyage, type = "plongee")
ais_carroyage2 <- intensite_computation(data = ais_carroyage, type = "ais")

# Agregation d'intensité d'activité entre jeux de données ----
agregation_carroyage <- rbind(
  survolus_carroyage2,
  peche_carroyage2,
  donia_carroyage2,
  plongee_carroyage2, 
  ais_carroyage2
)

# Check du nombre d'occurences avec 1 ou plus de 1 jeux de données pour une combinaison
# activité-id_carroyage
nombre_agregations <- agregation_carroyage %>%
  count(id2, talassa_code, name = "nombre_jeux_données") %>%
  ungroup() %>%
  count(nombre_jeux_données)

nombre_agregations # 792 hexagones avec 2 sources et 178 avec 3 sources

# Calcul moyenne intensité d'activité
agregation_carroyage <- agregation_carroyage %>%
  group_by(id2, talassa_code, talassa_intitule) %>%
  summarize(
    intensite = mean(intensite),
    ic = min(ic) # IC pris comme le minimum de l'IC
  )

# Verification nombre agregations
nombre_agregations <- agregation_carroyage %>%
  count(id2, talassa_code, name = "nombre_jeux_données") %>%
  ungroup() %>%
  count(nombre_jeux_données)

nombre_agregations # Ok : 1 valeur uniquement par combinaison activité-id_carroyage

# Organisation par codes talassa
agregation_carroyage <- agregation_carroyage %>%
  arrange(talassa_code, id2) %>%
  ungroup()


# Version noms de colonnes -> codes talassa ----

# Passage au format large (colonnes par code activité)
agregation_longer <- agregation_carroyage %>%
  select(-talassa_intitule) %>%
  pivot_longer(cols = c(intensite, ic)) %>%
  mutate(name = case_when(
    name == "intensite" ~ talassa_code, # code_talassa
    TRUE ~ paste0(talassa_code, "_iq") # code_talassa + ic
  )) %>%
  select(-talassa_code) %>%
  arrange(name, id2)

agregation_wider <- agregation_longer %>%
  pivot_wider(
    names_from = name,
    values_from = value,
    values_fill = 0
  )

# Jointure spatiale ID activités - hexagones 
agregation_final_codes <- left_join(
  x = carroyage,
  y = agregation_wider,
  by = join_by(id2)
)

# Transfo format final
agregation_final_codes <- agregation_final_codes %>%
  mutate(across(-c(geometry, id2), ~ coalesce(., 0))) %>% # Attention voir si remplacement NA pertinent
  st_as_sf()


# Version noms de colonnes -> intituleés talassa ----

# Passage au format large (colonnes par code activité)
agregation_longer_intitule <- agregation_carroyage %>%
  select(-talassa_code) %>%
  mutate(talassa_intitule = str_replace_all(talassa_intitule, " ", "_")) %>%
  pivot_longer(cols = c(intensite, ic)) %>%
  mutate(name = case_when(
    name == "intensite" ~ talassa_intitule, # code_talassa
    TRUE ~ paste0(talassa_intitule, "_iq") # code_talassa + ic
  )) %>%
  select(-talassa_intitule) %>%
  arrange(name, id2)


agregation_wider_intitule <- agregation_longer_intitule %>%
  pivot_wider(
    names_from = name,
    values_from = value,
    values_fill = 0
  )

# Jointure spatiale ID activités - hexagones 

# Jointure
agregation_finale_intitules <- left_join(
  x = carroyage,
  y = agregation_wider_intitule,
  by = join_by(id2)
)

# Transfo format final
agregation_final_intitules <- agregation_finale_intitules %>%
  mutate(across(-c(geometry, id2), ~ coalesce(., 0))) %>% # Attention voir si remplacement NA pertinent
  st_as_sf()


# Liste combinaisons ----

# Liste des combinaisons liens talassa_code et talassa_intitule final post-transfos
# pour un export simple permettant de renseigner les infos du fichier de paramétrage
# du modèle TALASSA

export_combinaisons <- FALSE

if (export_combinaisons) {

  liste_combinaisons <- agregation_carroyage %>%
    select(talassa_code, talassa_intitule) %>%
    distinct()

  write.xlsx(
    x = liste_combinaisons, 
    file = paths$processed$params_combinaisons
  )

}


# Exports ----
st_write(
  obj = agregation_final_codes,
  dsn = path_final,
  driver = "gpkg",
  append = FALSE
)

st_write(
  obj = agregation_final_intitules,
  dsn = path_final_wider,
  driver = "gpkg",
  append = FALSE
)



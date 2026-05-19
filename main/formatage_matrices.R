#' ---
#' title : "talassaR - formatage_matrices"
#' author : Aubin Woehrel
#' creation date : 2026-05-11
#' ---
#'
#' =============================================================================
#'
#' talassaR : Formatage des matrices de liens activités-pressions et sensibilité 
#' aux pressions
#'
#' Description :
#' Script permettant de formater les matrices de liens activité-pressions et 
#' sensibilité aux pressions au bon format utilisable pour le modèle ou 
#' intégrable dans les jeux de données considérés
#' 
#' Permet par ailleurs l'intégration des codes talassa de pression dans les 
#' deux matrices
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

## Ressources locales ----
source("r/paths.R")

## Import des données ----

# Matrice de sensibilité
matrice_sensibilite <- read.xlsx(
  xlsxFile = paths$raw_mat_sensibilite,
  sheet = "sensibilite",
  fillMergedCells = TRUE
)

# Codes habitats
codes_habitats <- read.xlsx(
  xlsxFile = paths$raw_codes_habitats,
  sheet = "codes",
  fillMergedCells = TRUE
)

skimr::skim(matrice_sensibilite)

# Valeurs uniques
unique(matrice_sensibilite$sensibilite)
unique(matrice_sensibilite$ic)
unique(matrice_sensibilite$pression)
unique(matrice_sensibilite$categorie)
unique(matrice_sensibilite$talassa_intitule)


# Codes de référence des pressions
codes_pressions <- read.xlsx(
  xlsxFile = paths$raw_codes_pressions, 
  sheet = "codes_pressions",
  fillMergedCells = TRUE
)

# Modifications ----

# Réencodage valeur sensibilité entre 1 et 5
matrice_sensibilite <- matrice_sensibilite %>%
  mutate(sensibilite = sensibilite %>% recode_values(
    "TF" ~ 1,
    "F" ~ 2, 
    "M" ~ 3, 
    "H" ~ 4, 
    "TH" ~ 5, 
    "V" ~ 3, # Hypothèse "moyenne", mais à confirmer selon stratégie
    NA  ~ NA
  ))

# Check des intitules et codes de pression de la référence de codes pressions
unique(codes_pressions$pression_intitule)
unique(codes_pressions$pression_code)

# Pressions dans la matrice activités-pressions sans équivalence dans la matrice Marie Larivière
codes_pressions$pression_intitule[!unique(codes_pressions$pression_intitule) %in% unique(matrice_sensibilite$pression)]


# Changement format code pression
codes_pressions <- codes_pressions %>%
  mutate(pression_code = factor(pression_code, levels = unique(pression_code)))


# Jointure codes à la matrice de sensibilité
sensibilite_better <- left_join(
  x = matrice_sensibilite, 
  y = codes_pressions, 
  by = join_by(pression_intitule)
)

# Pressions dont la documentation est réalisée dans la matrice de Larivière 
# mais dont les activités de Talassa ne sont pas associées à celle-ci
sensibilite_better %>%
  filter(is.na(pression_code)) %>%
  select(pression_intitule) %>%
  distinct()

# Simplification de la matrice
sensibilite_better <- sensibilite_better %>%
  filter(!is.na(pression_code)) %>%
  select(talassa_code, talassa_intitule, pression_code, pression_intitule, sensibilite, ic) %>%
  arrange(pression_code)

# Transfo matrice vers format final talassa wider
sensibilite_wider <- sensibilite_better %>%
  rename(pre = sensibilite, icas = ic) %>% # renommer sensibilité et ic par suffixes modélisation
  pivot_longer(cols = c(pre, icas)) %>% # allongement en une colonne par accolement valeurs sensi et indice de confiance valeur sensi
  mutate(name = paste0(pression_code, "_", name)) %>% # noms de colonnes finaux
  select(-c(pression_intitule, pression_code)) %>% 
  pivot_wider(names_from = name, values_from = value) # Pivot large pour avoir 1 ligne = 1 habitat

# Check colonnes où toutes les valeurs sont absentes
sapply(sensibilite_wider[,-c(1, 2)], \(x) {all(is.na(x), na.rm = TRUE)})

# Elimination colonnes pressions sans données
sensibilite_wider <- sensibilite_wider %>%
  select_if(~ !all(is.na(.)))

# Remplacement des NA par 99
sensibilite_wider <- sensibilite_wider %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 99, .)))

# Exports ----
saveRDS(object = sensibilite_wider, file = paths$processed_mat_sensibilites)

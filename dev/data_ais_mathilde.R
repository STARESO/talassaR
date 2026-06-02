# --------------------------------------------------------------------------- #
# Titre : Analyse des données AIS Marine Traffic 2025
# Sous-titre : Mise en forme des fichiers JSON et analyse données AIs
# Auteur : Mathilde PATERNOTTE
# Date de creation : AVRIL 2026
# --------------------------------------------------------------------------- #

#### EXTRACTION DES DONNEES JSON ####

# Packages
if (!require(jsonlite)) install.packages("jsonlite")
library(jsonlite)
library(dplyr)

# Chemin des fichiers
folder_path <- "C:/Users/mathilde.paternotte/Documents/M_PATERNOTTE/USAGES/Données/Plaisance et mouillage/AIS/2025_ais_marine_traffic_pnmcca/pnmcca"

# Chargement des fichiers
files <- list.files(folder_path, pattern = "\\.json$", full.names = TRUE)

# Fonction pour télécharger les fichiers au bon format
list_data <- lapply(files, function(f) {
  tryCatch({
    json <- fromJSON(f)
    df <- json$DATA
    df$source_file <- basename(f)
    return(df)
  }, error = function(e) {
    message(paste("Erreur fichier :", f))
    return(NULL)
  })
})

# Combinaison de tous les fichiers
data_all <- bind_rows(list_data)

# Téléchargement de la donnée au formation csv
write.csv(
  data_all,
  "C:/Users/mathilde.paternotte/Documents/M_PATERNOTTE/USAGES/Données/Plaisance et mouillage/AIS/2025_ais_marine_traffic_pnmcca/data_all_ais_2025.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

#### ANALYSES STATISTIQUES ####

# ==========================================
# Script d'analyse exploratoire AIS 
# ==========================================

# Packages
if(!require(dplyr)) install.packages("dplyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggplot2)) install.packages("ggplot2")
library(dplyr)
library(lubridate)
library(ggplot2)

# Import CSV
# Données extraites pour le mont sous-marin de l'Agriate
file_path <- "C:/Users/mathilde.paternotte/Documents/M_PATERNOTTE/USAGES/Données/Plaisance et mouillage/AIS/2025_ais_marine_traffic_pnmcca/data_ais_mt_sous_marin.csv"
data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Préparation
data <- data_all %>%
  mutate(
    datetime = ymd_hms(TIMESTAMP),
    date = as.Date(datetime),
    mois = format(date, "%Y-%m")
  ) %>%
  arrange(MMSI, datetime)

# Passage par bateau et jour (prise en compte des multiples passages)
data <- data %>%
  group_by(MMSI, date) %>%
  mutate(diff_h = as.numeric(difftime(datetime, lag(datetime), units = "hours")),
         nouveau_passage = ifelse(is.na(diff_h) | diff_h > 2, 1, 0)) %>%
  ungroup()

passages <- data %>%
  group_by(MMSI, date) %>%
  summarise(nb_passages_jour = sum(nouveau_passage),
            .groups = "drop")

# Chiffres clés
# Total passages
total_passages <- sum(passages$nb_passages_jour)

# Bateaux uniques
bateaux_uniques <- n_distinct(data$MMSI)

# Moyenne journalière de passages
moy_jour <- passages %>%
  group_by(date) %>%
  summarise(total_passages = sum(nb_passages_jour)) %>%
  summarise(moyenne_journaliere = mean(total_passages))

# Passage par mois
passage_mois <- passages %>%
  mutate(mois = format(date, "%Y-%m")) %>%
  group_by(mois) %>%
  summarise(passages = sum(nb_passages_jour))

# Catégories de bateaux
types_bateaux <- data %>%
  distinct(MMSI, TYPE_NAME) %>%
  count(TYPE_NAME) %>%
  arrange(desc(n))

# Statistiques de vitesse
vitesse_stats <- data %>%
  summarise(
    moyenne = mean(SPEED, na.rm = TRUE),
    mediane = median(SPEED, na.rm = TRUE),
    max = max(SPEED, na.rm = TRUE),
    min = min(SPEED, na.rm = TRUE),
    sd = sd(SPEED, na.rm = TRUE)
  )

vitesse_par_type <- data %>%
  group_by(TYPE_NAME) %>%
  summarise(
    vitesse_moyenne = mean(SPEED, na.rm = TRUE),
    vitesse_max = max(SPEED, na.rm = TRUE)
  )

# Graphiques
# Histogramme passages par mois
ggplot(passage_mois, aes(x = mois, y = passages)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Nombre de passages par mois", y = "Nombre de passages", x = "Mois")

# Histogramme types de bateaux
ggplot(types_bateaux, aes(x = reorder(TYPE_NAME, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Nombre de bateaux par catégorie", y = "Nombre de MMSI", x = "Type de bateau")

# Histogramme vitesse
ggplot(data, aes(x = SPEED)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Distribution de la vitesse des bateaux (knots)", x = "Vitesse", y = "Nombre d'observations")

# Comptage des bateaux par pays (SHIP_COUNTRY)
bateaux_pays <- data %>%
  distinct(MMSI, SHIP_COUNTRY) %>%
  count(SHIP_COUNTRY) %>%
  arrange(desc(n))

# Graphique
ggplot(bateaux_pays, aes(x = reorder(SHIP_COUNTRY, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(
    title = "Nombre de bateaux par pays",
    x = "Pays",
    y = "Nombre de MMSI uniques"
  )

# Résumé
cat("===== Chiffres clés AIS =====\n")
cat("Total passages :", total_passages, "\n")
cat("Bateaux uniques :", bateaux_uniques, "\n")
cat("Moyenne journalière passages :", round(moy_jour$moyenne_journaliere, 2), "\n")
cat("\n===== Statistiques de vitesse =====\n")
print(vitesse_stats)
cat("\n===== Nombre de bateaux par type =====\n")
print(types_bateaux)
cat("\n===== Vitesse moyenne par type =====\n")
print(vitesse_par_type)

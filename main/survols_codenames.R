#' ---
#' title : "talassaR - codename issues"
#' author : Aubin Woehrel
#' creation date : 2025-09-16
#' last modification : 2025-10-20
#' ---
#'
#' =============================================================================
#' 
#' talassaR : 
#' Code name issues
#' 
#' Description : 
#' Small script to check lines where code of activity does not correspond to the
#' right activity name
#' 
#' =============================================================================

# Initialization ----

## Clean up and working directory ----
rm(list = ls())

## Library imports ----

# Data import and tidying
library("dplyr")
library("tidyr")

# Spatial
library("sf")

## Sourcing local resources ----
source("r/paths.R")

## Importing data ----
survols_usages <- readRDS(paths$raw_survols_usages)

# Code vs intitule verifications ----

## Checks
code_vs_nom <- survols_usages %>%
  select(cod_act, act) %>%
  group_by(cod_act, act) %>%
  summarize(n = n()) %>%
  arrange(cod_act)

code_vs_nom_byyear <- survols_usages %>%
  select(cod_act, act, annee) %>%
  group_by(cod_act, act, annee) %>%
  summarize(erreur_code_n = n()) %>%
  arrange(cod_act, annee)

## Exports of name and code linking

### Output for referencing codes manually
output_ref <- TRUE
if (output_ref) {
  write.csv2(code_vs_nom, paths$processed_survols_code_vs_nom)
} 

### Input of manual completed code linking
input_ref <- TRUE
if (input_ref) {
  survols_resoblo <- read.csv(paths$processed_survols_code_vs_nom_complet, sep = ";")
}

## Merging main dataset with error descriptions
survols_usages_fusion <- left_join(survols_usages, survols_resoblo, by = join_by(act, cod_act))

t1 <- survols_usages_fusion %>%
  filter(erreur_code == "invalide") %>%
  group_by(annee, mois, cod_act, act, erreur_code, erreur_code_description, erreur_code_suggestion) %>%
  summarize(erreur_code_n = n()) %>%
  relocate(erreur_code_n, .after = erreur_code) %>%
  mutate(mois = case_when(
    mois == "June" ~ "Juin",
    mois == "July" ~ "Juillet",
    mois == "August" ~ "Aout", 
    mois == "September" ~ "Septembre"
  )) %>% 
  mutate(mois = factor(mois, levels = c("Juin", "Juillet", "Aout", "Septembre"))) %>%
  arrange(annee, mois, cod_act, act)

errorref_simple <- survols_usages_fusion %>%
  filter(erreur_code == "invalide") %>%
  select(id_acti, date, annee, mois, nom_acti, act, cod_act, erreur_code, erreur_code_description, 
         erreur_code_suggestion, lon_x, lat_y)

# Exporting data of errors per date ----

# Ensure the date column is in character format
errorref_simple$date <- as.character(errorref_simple$date)
errorref_simple$annee <- as.character(errorref_simple$annee)  # Ensure 'annee' is character

# Convert to spatial object
spatial_data <- st_as_sf(
  errorref_simple,
  coords = c("lon_x", "lat_y"),
  crs = 4326
)

# Get unique years to create year folders
unique_years <- unique(errorref_simple$annee)

# Loop through each year
for (year in unique_years) {
  
  # Create a folder for the year if it doesn't exist
  year_folder <- file.path(paths$processed_survols_erreurs, year)
  
  if (!dir.exists(year_folder)) {
    dir.create(year_folder)
  }
  
  # Get all dates for the current year
  dates_in_year <- unique(errorref_simple$date[errorref_simple$annee == year])
  
  # Loop through each date in the year
  for (i in seq_along(dates_in_year)) {
    date_wanted <- as.Date(dates_in_year[i])
    print(date_wanted)
    
    # Filter the spatial data for the current date
    subdata_spatial <- spatial_data %>%
      filter(date == as.character(date_wanted)) %>%
      rename(valide = erreur_code,
             description = erreur_code_description,
             suggestion = erreur_code_suggestion)
    
    # Create a filename with the survey number and date (e.g., 01_2024-08-30)
    survey_number <- sprintf("%02d", i)  # Formats as two digits (01, 02, etc.)
    subdata_filename <- file.path(
      year_folder,
      paste0("erreurs_survol_", survey_number, "_", format(date_wanted, "%Y-%m-%d"), ".gpkg")
    )
    
    # Export the subdataset as spatial data
    # st_write(subdata_spatial, shp_filename, driver = "ESRI Shapefile", append = FALSE)
    st_write(subdata_spatial, subdata_filename, driver = "GPKG", append = FALSE)
    
    # Print confirmation
    message(paste("Exported Shapefile:", shp_filename))
  }
}
  
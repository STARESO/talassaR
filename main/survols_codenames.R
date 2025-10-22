#' ---
#' title : "talassaR - codename issues"
#' author : Aubin Woehrel
#' creation date : 2025-09-16
#' last modification : 2025-10-22
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

## Data precheck ----
skimr::skim(survols_usages)

## Checks ----
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

## Exports of name and code linking ----

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


# Merging main dataset with error descriptions ----
survols_usages_fusion <- left_join(survols_usages, survols_resoblo, by = join_by(act, cod_act))


## Checks ----

# General structure
skimr::skim(survols_usages_fusion)

# Number of errors per season
t1 <- survols_usages_fusion %>%
  filter(erreur_code == "invalide") %>%
  group_by(
    annee, 
    mois, 
    cod_act, 
    act, 
    erreur_code, 
    erreur_code_description, 
    erreur_code_suggestion,
    suggestion_resoblo_code,
    suggestion_resoblo_intitule
  ) %>%
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

t1

# Changing names 
survols_usages_fusion <- survols_usages_fusion %>%
  mutate(
    resoblo_code = suggestion_resoblo_code,
    resoblo_intitule = suggestion_resoblo_intitule
  )

# Exporting to spatial data ----

# # Ensure the date column is in character format
# survols_usages_errors$date <- as.character(survols_usages_errors$date)
# survols_usages_errors$annee <- as.character(survols_usages_errors$annee)  # Ensure 'annee' is character

## Spatial transfo ----
spatial_usages <- st_as_sf(
  survols_usages_fusion,
  coords = c("lon_x", "lat_y"),
  crs = 4326
)

## Export files per flight date ----

# Get unique years to create year folders
unique_years <- unique(spatial_usages$annee)

# Looping over the years
for (year in unique_years) {
  
  # Folders for the year
  folder_toverify <- file.path(paths$processed_survols_toverify, year, fsep = "")
  folder_errors <- file.path(paths$processed_survols_errors, year, fsep = "")
  
  if (!dir.exists(folder_toverify)) { dir.create(folder_toverify) }
  if (!dir.exists(folder_errors)) { dir.create(folder_errors) }
  
  # Get all dates for the current year
  dates_in_year <- unique(spatial_usages$date[spatial_usages$annee == year]) %>% sort()
  
  # Loop through each date position of the year
  for (i in seq_along(dates_in_year)) {
    
    # Date of the loop
    date_wanted <- dates_in_year[i]
    
    # Filter the spatial data for the current date
    spatial_subdata <- spatial_usages %>%
      filter(date == date_wanted) %>%
      rename(
        valide = erreur_code,
        description = erreur_code_description,
        suggestion = erreur_code_suggestion
      ) %>%
      mutate(across(c(d_heur_sur, f_heur_sur), ~as.character(hms::as_hms(.))))
    
    # Getting only mistakes
    errors_subdata <- spatial_subdata %>%
      filter(valide == "invalide") %>%
      select(
        id_acti, 
        date, 
        annee, 
        mois, 
        nom_acti, 
        act, 
        cod_act, 
        valide,
        description,
        suggestion,
        resoblo_code,
        resoblo_intitule
      )
    
    # Filenames with the survey date 
    subdata_filename <- file.path(
      folder_toverify,
      paste0("survol_usages_toverify_", format(date_wanted, "%Y-%m-%d"), ".gpkg")
    )
    
    errors_filename <- file.path(
      folder_errors,
      paste0("survol_usages_errors_", format(date_wanted, "%Y-%m-%d"), ".gpkg")
    )
    
    # Export the subdataset as spatial data
    st_write(spatial_subdata, subdata_filename, driver = "GPKG", append = FALSE)
    
    # In case of mistakes
    if (dim(errors_subdata)[1] > 0) {
      st_write(errors_subdata, errors_filename, driver = "GPKG", append = FALSE)
    }
    
    # Print confirmation
    message(paste("Exported spatial data to verify:", subdata_filename))
    message(paste("Exported error data to verify:", errors_filename))
  }
}

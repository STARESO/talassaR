#' ---
#' title : "talassaR - donia verification"
#' author : Aubin Woehrel
#' creation date : 2025-09-15
#' last modification : 2025-10-14
#' ---
#'
#' =============================================================================
#' 
#' talassaR : 
#' Donia verification
#' 
#' Description : 
#' Small script to investigate and prepare data of Donia for the talassa project
#' in the PNMCCA. Idea is to verify how data is structure and figure out how 
#' to correct and preprocess the data for further integration in a grid
#' 
#' =============================================================================


# Initialization ----

## Clean up and working directory ----
rm(list = ls())

## Library imports ----

# Data import and tidying
library("readr")
library("dplyr")
library("tidyr")
library("stringr")

# Spatial 
library("sf")
library("leaflet")
library("rlang")

## Sourcing paths and constants ----
# source("R/paths.R")

## Importing data ----

donia <- read_delim(
  "data/raw/mouillage_donia/donia.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE
)

pnm_borders <- sf::st_read("data/raw/pnm/N_ENP_PNM_S_000.shp") %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(NOM_SITE == "cap Corse et Agriate")

# Preprocess ----

## First investigations ----
spec(donia)
skimr::skim(donia)
names(donia)

## Small transformations ----
donia <- donia %>%
  separate_wider_delim(
    col = date_mouillage_central_european_time,
    delim = " ",
    names = c("date", "time")
  ) %>%
  mutate(
    date = as.Date(date, format = "%d/%m/%Y"),
    classe_probabilite_impact = factor(
      classe_probabilite_impact, 
      levels = c("tres_faible", "faible", "moyen", "fort", "tres_fort")
    ),
    taille = as.numeric(taille)
  ) %>%
  mutate(across(where(is.character), ~ na_if(., "/"))) %>%
  rename(
    lon_x = longitude_position_degres_decimaux,
    lat_y = latitude_position_degres_decimaux
  )

## Structure verifications ----
t1 <- donia %>%
  group_by(nom) %>%
  summarize(n = n()) %>%
  arrange(nom, desc(n))

t1sum <- t1 %>%
  mutate(type = case_when(
    is.na(nom) ~ "unnamed",
    TRUE ~ "named"
  )) %>%
  group_by(type) %>%
  summarize(n = sum(n, na.rm = FALSE))

t1sum

t2 <- donia %>%
  group_by(nom, date) %>%
  summarize(n = n()) %>%
  arrange(nom, desc(n), date)

type_navire <- donia %>%
  select(type_navire, type_navire_brut) %>%
  group_by_all() %>%
  summarize(n = n())

type_na_vs_value <-   type_navire %>%
  mutate(type_navire_brut = case_when(
    is.na(type_navire_brut) ~ "NA",
    TRUE ~ "type_existe"
  )) %>%
  group_by(type_navire_brut) %>%
  summarize(n = sum(n, na.rm = FALSE))

unique(donia$nom_masse_eau)

## Modifications post-investigation

donia <- donia %>%
  mutate(across(c(type_navire_brut, type_navire, nom_masse_eau, region), 
                str_to_lower))

type_navire <- donia %>%
  select(type_navire, type_navire_brut) %>%
  group_by_all() %>%
  summarize(n = n())


# Spatialisation ----
donia_spatial <- st_as_sf(donia, coords = c("lon_x", "lat_y"), crs = 4326)


## Variable distinction map ----

category_map <- function(focus_var, compressor = 1) {
  # Unique values of variable of interest (excluding NA)
  unique_values <- donia_spatial %>%
    pull({{ focus_var }}) %>%
    na.omit() %>%
    unique()
  
  # Choose palette based on the type and number of unique values
  if (is.numeric(donia_spatial %>% pull({{ focus_var }}))) { 
    all_focused <- donia_spatial[[focus_var]]
    all_focused <- log(1 + compressor * all_focused)
    # For numeric variables, use a gradient palette
    pal <- colorNumeric(
      palette = "Greys",  # or "plasma", "inferno", etc.
      domain = range(all_focused[!is.na(all_focused)]),
      reverse = TRUE
    )
    
    # Precomputing colors
    colors <- ifelse(
      is.na(all_focused),
      "red",  # Color for NA values
      pal(all_focused)  # Color for non-NA values
    )
    
  } else {
    # For categorical variables
    if (length(unique_values) <= 12) {
      pal <- colorFactor(palette = "Set3", domain = unique_values)
    } else {
      # For more than 12 unique values, use a qualitative palette
      pal <- colorFactor(
        palette = colorspace::qualitative_hcl(length(unique_values), "Dark2"),
        domain = unique_values
      )
    }
    
    # Precomputing colors
    colors <- ifelse(
      is.na(donia_spatial[[focus_var]]),
      "red",  # Color for NA values
      pal(donia_spatial[[focus_var]])  # Color for non-NA values
    )
  }
  
  # Map
  map_optimized <- leaflet(donia_spatial) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addPolygons(data = pnm_borders, color = "lightblue", weight = 10) %>%
    addCircleMarkers(
      radius = 4,
      stroke = FALSE,
      fillOpacity = 1,
      color = colors,  # Use the precomputed colors (red for NA)
      popup = ~paste(
        "Nom :", nom, "<br>",
        "Type :", type_navire, "<br>",
        "Sous-type :", type_navire_brut, "<br>",
        "Taille :", taille, "m", "<br>",
        "Classe de taille :", classe_taille, "<br>",
        "Date :", date, "<br>",
        "Heure :", time, "<br>",
        "Région :", region, "<br>",
        "Masse d'eau :", nom_masse_eau, "<br>",
        "Habitat :", habitat, "<br>",
        "Duree mouillage", duree_mouillage, "<br>",
        "Probabilite impact :", probabilite_impact
      )
    )
  
  return(map_optimized)
}

# Example usage
map_region <- category_map("region")
map_region

map_type <- category_map("type_navire")
map_type

map_sous_type <- category_map("type_navire_brut")
map_sous_type  

map_annee <- category_map("annee_mouillage")
map_annee

map_masse_eau <- category_map("code_masse_eau")
map_masse_eau  

map_proba_impact <- category_map("probabilite_impact", compressor = 10)
map_proba_impact

map_duree_mouillage <- category_map("duree_mouillage", compressor = 100)
map_duree_mouillage

map_duree_mouillage <- category_map("taille", compressor = 1)
map_duree_mouillage

#' ---
#' title : "talassaR - peche verification"
#' author : Aubin Woehrel
#' creation date : 2026-01-20
#' ---
#'
#' =============================================================================
#'
#' talassaR : Peche verification
#'
#' Description :
#' Script takes care of checking the overview of the leisure fishing data used
#' in the talassa project. General verification process, but dataset should
#' be already clean thanks to the previous use of the dataset by Jonathan
#' Richir in another contract with the PNMCCA
#'
#' =============================================================================
#'
#' # Initialization ----
#'
#' ## Clean up and working directory ----
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

## Sourcing local resources ----
source("r/paths.R")
source("r/fct_category_map.R") # Custom data spatial map function


# Importing data ----

# Peche de loisir imports (different versions of raw data)
peche_loisir <- sf::st_read(paths$raw_peche_shp) %>% sf::st_transform(crs = 4326)
peche_loisir_gpkg <- sf::st_read(paths$raw_peche_gpkg) %>% sf::st_transform(crs = 4326)
peche_loisir_quentin <- sf::st_read(paths$raw_peche_quentin) %>% sf::st_transform(crs = 4326)

# The dataset cleaned by hand from peche loisir for the Talassa project
peche_loisir_clean <- sf::st_read(paths$raw_peche_clean) %>% sf::st_transform(crs = 4326)

# Carroyage data
hex_cinquieme <- st_read(paste0(paths$raw_carroyage, "grille_talassa_2025_cotier_hexagone_cinquiemedemile.shp"))

# PNMCCA Border import
pnm_borders <- sf::st_read(paths$raw_pnmcca_borders) %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(NOM_SITE == "cap Corse et Agriate")

# Resoblo reference
peche_resoblo <- read.csv2(paths$raw_peche_resoblo)

# Fast structure check
str(peche_loisir)
str(peche_loisir_gpkg)
str(peche_loisir_clean)
str(peche_loisir_quentin) # Not exactly all the same variable names

# Few column names changes for quentin's dataset
peche_loisir_quentin <- peche_loisir_quentin %>%
  rename_with(stringr::str_to_lower) %>%
  rename_with(function(x) {
    str_replace_all(x, "\\.", "_")
  }) %>%
  rename(mod_pech = mod_peche)

# Map of peche function
map_peche <- function(data_peche, type_peche = "all") {
  # Case when only a type of fishing is selected
  if (type_peche != "all") {
    data_peche <- data_peche %>%
      filter(mod_pech == type_peche)
  }

  map_peche_selected <- data_peche %>%
    leaflet(.) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addPolygons(data = pnm_borders, color = "lightblue", weight = 10)

  if ("bd" %in% names(data_peche)) { # quentin treated data
    map_peche_selected <- map_peche_selected %>%
      addCircleMarkers(
        radius = 4,
        stroke = FALSE,
        fillOpacity = 1,
        color = colors, # Use the precomputed colors (red for NA)
        popup = ~ paste(
          "Source Données:", bd, "<br>",
          "ID sortie :", id_obs, "<br>",
          "Type de pêche", mod_pech, "<br>"
        )
      )
  } else { # PNMCCA og data
    map_peche_selected <- map_peche_selected %>%
      addCircleMarkers(
        radius = 4,
        stroke = FALSE,
        fillOpacity = 1,
        color = colors, # Use the precomputed colors (red for NA)
        popup = ~ paste(
          "ID sortie :", id_obs, "<br>",
          "Type de pêche", mod_pech, "<br>"
        )
      )
  }
  map_peche_selected
}

# All data
map_peche(peche_loisir)
map_peche(peche_loisir_gpkg)
map_peche(peche_loisir_quentin)
map_peche(peche_loisir_clean)

# Map of og fishing dataset by type
map_peche(data_peche = peche_loisir, type_peche = "po")
map_peche(data_peche = peche_loisir, type_peche = "pe")
map_peche(data_peche = peche_loisir, type_peche = "pdb")
map_peche(data_peche = peche_loisir, type_peche = "csm")

# Map of corrected dataset (quentin 2025) per type
map_peche(data_peche = peche_loisir_quentin, type_peche = "po")
map_peche(data_peche = peche_loisir_quentin, type_peche = "pe")
map_peche(data_peche = peche_loisir_quentin, type_peche = "pdb")
map_peche(data_peche = peche_loisir_quentin, type_peche = "csm")

# General variable investigation ----
View(peche_loisir)

sort(unique(peche_loisir$id_obs))
unique(peche_loisir$mod_pech) # All levels are ok

# Testing unicity of identifiers : which are specific to each survey ?
test <- peche_loisir_quentin %>%
  as.data.frame() %>%
  select(fiche_n, id_sortie, id_obs, bd) %>%
  distinct() %>%
  arrange(bd, fiche_n)

View(test)

test2 <- test %>%
  group_by(id_obs) %>%
  summarize(n = n())

View(test2)

# Unicity of each survey is the combination of fiche_n
# and the origin dataset (column bd)

# Cheking total amount of enquêtes for mod_pech = pdb before elimination
count_pdb <- peche_loisir %>%
  as.data.frame() %>%
  filter(mod_pech == "pdb") %>%
  select(id_obs, saisie) %>%
  distinct() %>%
  group_by(saisie) %>%
  mutate(n = n())
# View(count_pdb)

# Preparing for grid aggregation ----
peche_loisir_simpler <- peche_loisir_clean %>%
  select(id_obs, id_sortie, date, mod_pech, prof_m, nb_pecheur, temps_pech) %>%
  distinct()

View(peche_loisir_simpler)

check_unicity <- peche_loisir_simpler %>%
  group_by(id_obs) %>%
  summarize(n = n())

map_peche(peche_loisir_simpler)

## Bring Peche de loisir points to the grid crs, i.e. Lambert-93 ----
st_crs(peche_loisir_simpler) # WGS 84
st_crs(hex_cinquieme) # Lambert-93

pts <- peche_loisir_simpler %>%
  st_transform(st_crs(hex_cinquieme))

st_crs(pts) # Now in Lambert-93

## Spatial join to attach grid id to points ----
pts_j <- st_join(
  x = pts,
  y = hex_cinquieme,
  join = st_intersects
)

# Testing view of join map to check intersections with hex id label
join_map <- pts_j %>%
  st_transform("EPSG:4326") %>%
  leaflet(.) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(data = pnm_borders, color = "lightblue", weight = 10) %>%
  addPolygons(data = st_transform(hex_cinquieme, crs = "EPSG:4326"), color = "#103441", weight = 3) %>%
  addCircleMarkers(
    radius = 4,
    stroke = FALSE,
    fillOpacity = 1,
    color = colors, # Use the precomputed colors (red for NA)
    popup = ~ paste(
      "ID sortie :", id_obs, "<br>",
      "Id hex :", id_hex, "<br>",
      "Date :", date, "<br>",
      "Type de pêche :", mod_pech, "<br>"
    )
  )
# join_map

# Number of non usable points due to absence of hexagons on placement of points
pts_lost <- pts_j %>%
  filter(is.na(id_hex)) %>%
  dim(.)
pts_lost[1] # Eleven points total

dim(pts_j)[1] - pts_lost[1] # 195 usable

## Grouping points per hex as fishermen surveyed ----

### For all fishing
agg_tot <- pts_j %>%
  st_drop_geometry() %>%
  filter(!is.na(id_hex)) %>%
  group_by(id_hex) %>% # TODO : implement resoblo code instead of mod_pech
  summarise(
    peche_loisir_total = sum(nb_pecheur, na.rm = TRUE)
  )

### Per category of fishingc:\Users\Public\Desktop\QGIS 3.44.6\QGIS Desktop 3.44.6.lnk
agg_cat <- pts_j %>%
  st_drop_geometry() %>%
  filter(!is.na(id_hex)) %>%
  group_by(id_hex, mod_pech) %>% # TODO : implement resoblo code instead of mod_pech
  summarise(
    nb_pecheur = sum(nb_pecheur, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = mod_pech,
    values_from = nb_pecheur,
    values_fill = 0
  ) %>%
  ungroup()

agg <- agg_cat %>%
  left_join(., agg_tot, by = "id_hex") %>%
  relocate(peche_loisir_total, .after = all_of("id_hex"))

grid_out <- hex_cinquieme %>%
  left_join(., agg, by = "id_hex") %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  relocate(left, top, right, bottom, .after = everything())

st_write(
  obj = grid_out,
  dsn = paste0(paths$processed_peche_hex, "us_med_pnmcca_observatoire_pecheloisir_hexcinquieme_ofb_pol.gpkg"),
  layer = "hex_cinquieme_peche_all",
  delete_dsn = TRUE
)

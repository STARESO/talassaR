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

# Data representations
library("ggplot2")
library("ggbeeswarm")
library("ggExtra")
library("ggpubr")

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
  summarize(n = n()) %>%
  arrange(desc(type_navire), desc(n))

write.csv2(type_navire, "data/processed/donia_type_navire.csv")

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

category_map <- function(data_spatial, focus_var, compressor = 1) {
  # Compressor argument : strength of log scaling for continuous gray scale representation. 
  # (Only visual, no effect on real data on popup)
  
  # Unique values of variable of interest (excluding NA)
  unique_values <- data_spatial %>%
    pull({{ focus_var }}) %>%
    na.omit() %>%
    unique()
  
  # Choosing palette based on the type and number of unique values
  if (is.numeric(data_spatial %>% pull({{ focus_var }}))) { 
    all_focused <- data_spatial[[focus_var]]
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
      is.na(data_spatial[[focus_var]]),
      "red",  # Color for NA values
      pal(data_spatial[[focus_var]])  # Color for non-NA values
    )
  }
  
  # Leaflet Map
  map_optimized <- leaflet(data_spatial) %>%
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

# Variable per variable
map_region <- category_map(donia_spatial, "region")
map_region

map_type <- category_map(donia_spatial, "type_navire")
map_type

map_sous_type <- category_map(donia_spatial, "type_navire_brut")
map_sous_type  

map_annee <- category_map(donia_spatial, "annee_mouillage")
map_annee

map_masse_eau <- category_map(donia_spatial, "code_masse_eau")
map_masse_eau  

map_proba_impact <- category_map(donia_spatial, "probabilite_impact", compressor = 10)
map_proba_impact

map_duree_mouillage <- category_map(donia_spatial, "duree_mouillage", compressor = 100)
map_duree_mouillage

map_taille <- category_map(donia_spatial, "taille", compressor = 1)
map_taille


# Checking per type : 

# Cargo ships
donia_spatial %>%
  filter(type_navire_brut %in% c(
    "cargo ship", "general cargo ship"
  )) %>%
  category_map(., "type_navire_brut")

# Passenger ships
donia_spatial %>%
  filter(type_navire_brut %in% c(
    "passenger ship", 
    "passenger (cruise) ship", 
    "passenger/ro-ro cargo ship"
  )) %>%
  category_map(., "type_navire_brut")

# Diving ops
donia_spatial %>%
  filter(type_navire_brut %in% c(
    "diving ops"
  )) %>%
  category_map(., "type_navire_brut")

# Training ship
donia_spatial %>%
  filter(type_navire_brut %in% c(
    "training ship"
  )) %>%
  category_map(., "type_navire_brut")

# Yacht
donia_spatial %>%
  filter(type_navire_brut %in% c(
    "yacht"
  )) %>%
  category_map(., "taille")


# Resoblo format ----

## Joining datasets ----

# Import of donia-resoblo connections reference manually completed
donia_resoblo <- read.csv2("data/processed/donia_resoblo.csv") %>%
  select(-type_navire)

# Adding generalist columns of code and intitule depending on level 
donia_resoblo<- donia_resoblo %>%
  mutate(
    resoblo_intitule = case_when(
      !is.na(resoblo_intitule_n1) ~ resoblo_intitule_n1,
      TRUE ~ resoblo_intitule_n2
    ), 
    resoblo_code = case_when(
      !is.na(resoblo_code_n1) ~ resoblo_code_n1,
      TRUE ~ resoblo_code_n2
    ),
    resoblo_niveau = case_when(
      !is.na(resoblo_code_n1) ~ 1,
      TRUE ~ 2
    )
  )

# New version of donia with resoblo info by left join
donia <- donia %>%
  filter(!is.na(type_navire_brut)) %>% # All non specified type_navire_brut are removed
  left_join(., donia_resoblo, by = join_by("type_navire_brut"))

# Checking amount of observations per resoblo level 2
t1 <- donia %>%
  select(resoblo_code_n2, resoblo_intitule_n2) %>%
  group_by_all() %>%
  summarize(n = n())

# All NA for resoblo at level 2
t2 <- donia %>%
  filter(is.na(resoblo_code_n2)) %>%
  select(
    resoblo_intitule_n2, 
    type_navire_brut, 
    type_navire) %>%
  group_by_all() %>%
  summarize(n = n()) %>%
  arrange(desc(n))


# Filter out all lines without resoblo correspondance from dataset
donia <- donia %>%
  filter(!is.na(resoblo_intitule_n2))

# Checking left over 
t3 <- donia %>%
  select(
    type_navire,
    type_navire_brut,
    resoblo_intitule_n2, 
    resoblo_intitule_n1,
    resoblo_code_n2,
    resoblo_code_n1
  ) %>%
  group_by_all() %>%
  summarize(n = n()) %>%
  select(n, everything()) %>%
  arrange(desc(n)) %>%
  ungroup()

skimr::skim(donia) # A few boats without any size


# Therefore, trying to see which categories do not have size :
t4 <- donia %>%
  filter(is.na(taille)) %>%
  select(
    type_navire_brut,
    resoblo_intitule_n2, 
    resoblo_intitule_n1,
  ) %>%
  group_by_all() %>%
  summarize(n_na = n()) %>%
  arrange(desc(n_na)) %>%
  ungroup()

# And then computing completion rate :
t4 <- t3 %>% 
  filter(type_navire_brut %in% t4$type_navire_brut) %>%
  select(type_navire_brut, n) %>%
  left_join(., t4) %>%
  mutate(
    resoblo_intitule = case_when(
      !is.na(resoblo_intitule_n1) ~ resoblo_intitule_n1,
      TRUE ~ resoblo_intitule_n2
    ),
    taux_completion_taille = round(((n - n_na)*100)/n, 1)
  ) %>%
  select(type_navire_brut, resoblo_intitule, n, n_na, taux_completion_taille) %>%
  rename(na_taille = n_na)


# Graphical representations of size of boats and time anchored for transfo thinking process
ggplot(donia, aes(x = taille, y = "")) +
  geom_beeswarm(method = 'center') + 
  theme_pubr() + 
  labs(y = "")

ggplot(donia, aes(x = duree_mouillage, y = "")) +
  geom_beeswarm(method = 'center') + 
  theme_classic() + 
  labs(y = "")

g1 <- ggplot(donia, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_y_reverse()
ggMarginal(g1, type = "histogram", size = 3)

g2 <- ggplot(donia, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_y_continuous(transform = scales::compose_trans("log10", "reverse"))
ggMarginal(g2, type = "histogram", size = 3)

g3 <- ggplot(donia, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_x_continuous(transform = "log10") +
  scale_y_continuous(transform = scales::compose_trans("log10", "reverse"))
ggMarginal(g3, type = "histogram", size = 3)

g4 <- ggplot(donia, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_x_continuous(transform = "log10", limits = c(10, 200)) +
  scale_y_continuous(transform = scales::compose_trans("log10", "reverse"))
ggMarginal(g4, type = "histogram", size = 3)


# Spatialisation post-fusion check ----

donia_spatial <- st_as_sf(donia, coords = c("lon_x", "lat_y"), crs = 4326)

donia_folder <- file.path("data/processed/donia_folder/")
if (!dir.exists(donia_folder)) {
  dir.create(donia_folder)
}

st_write(donia_spatial, paste0(donia_folder, "/donia_talassa.shp"), driver = "ESRI Shapefile")

map_taille <- category_map(donia_spatial, "taille", compressor = 1)
map_taille

map_region <- category_map(donia_spatial, "region")
map_region 





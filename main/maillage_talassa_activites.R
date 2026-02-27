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
    file = paths$raw_devcarroyage_activites
  )

# Permet de compléter manuellement l'excel avec les formules de calcul
# d'agrégation et les intervalles de confiance pour la suite des étapes.
# Reprendre la suite après complétion de la référence puis renomer selon
# le chemin paths$raw_refcarroyage_activites
paths$raw_refcarroyage_activites


# Jointure ID maillage ----

resultats_carroyage <- map(
  .x = list(survolus_talassa, peche_talassa, donia_talassa, plongee_talassa),
  .f = ~ jointure_id(carroyage = carroyage_hex, data = .x, id_name = "id_hex")
)

survolus_carroyage <- resultats_carroyage[[1]]
peche_carroyage <- resultats_carroyage[[2]]
donia_carroyage <- resultats_carroyage[[3]]
plongee_carroyage <- resultats_carroyage[[4]]


dim(survolus_talassa)
dim(survolus_carroyage)

dim(peche_talassa)
dim(peche_carroyage)

dim(donia_talassa)
dim(donia_carroyage)

dim(plongee_talassa)
dim(plongee_carroyage)




## Réflexion transfos donia ----
ggplot(donia_obs, aes(x = taille, y = "")) +
  geom_beeswarm(method = "center") +
  theme_pubr() +
  labs(y = "")

ggplot(donia_obs, aes(x = duree_mouillage, y = "")) +
  geom_beeswarm(method = "center") +
  theme_classic() +
  labs(y = "")

g1 <- ggplot(donia_obs, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_y_reverse()
ggMarginal(g1, type = "histogram", size = 3)

g2 <- ggplot(donia_obs, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_y_continuous(transform = scales::compose_trans("log10", "reverse"))
ggMarginal(g2, type = "histogram", size = 3)

g3 <- ggplot(donia_obs, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_x_continuous(transform = "log10") +
  scale_y_continuous(transform = scales::compose_trans("log10", "reverse"))
ggMarginal(g3, type = "histogram", size = 3)

g4 <- ggplot(donia_obs, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_x_continuous(transform = "log10", limits = c(10, 200)) +
  scale_y_continuous(transform = scales::compose_trans("log10", "reverse"))
ggMarginal(g4, type = "histogram", size = 3)


# Pêche de loisir ----

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
      "Type de pêche :", mod_pech, "<br>",
      "Resoblo code :", resoblo_code_n3, "<br>",
      "Resoblo intitulé :", resoblo_intitule_n3, "<br>"
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
  group_by(id_hex) %>%
  summarise(
    peche_loisir_total = sum(nb_pecheur, na.rm = TRUE)
  )

### Per category of fishingc:\Users\Public\Desktop\QGIS 3.44.6\QGIS Desktop 3.44.6.lnk
agg_cat <- pts_j %>%
  st_drop_geometry() %>%
  filter(!is.na(id_hex)) %>%
  group_by(id_hex, mod_pech, mod_pech_intitule, resoblo_intitule_n3, resoblo_code_n3) %>%
  summarise(
    nb_pecheur = sum(nb_pecheur, na.rm = TRUE)
  ) %>%
  ungroup()

# Format Talassa avec entêtes Resoblo snake-type
agg_cat_wider <- agg_cat %>%
  select(id_hex, resoblo_code_n3, nb_pecheur) %>%
  mutate(resoblo_code_n3 = str_to_snake(resoblo_code_n3)) %>% # converting to better talassa format for model
  pivot_wider(
    names_from = resoblo_code_n3,
    values_from = nb_pecheur,
    values_fill = 0
  ) %>%
  ungroup()

# Export données format Talassa
agg <- agg_cat_wider %>%
  left_join(., agg_tot, by = "id_hex") %>%
  relocate(peche_loisir_total, .after = all_of("id_hex"))

grid_out <- hex_cinquieme %>%
  left_join(., agg, by = "id_hex") %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  relocate(left, top, right, bottom, .after = everything())

st_write(
  obj = grid_out,
  dsn = paste0(paths$processed_peche_hex, "us_med_pnmcca_talassa_pecheloisir_hexcinquieme_ofb_pol.gpkg"),
  layer = "hex_cinquieme_peche_all",
  delete_dsn = TRUE
)

# Export données format observatoire classique (entête plus compréhensible)
agg2 <- agg_cat %>%
  left_join(., agg_tot, by = "id_hex") %>%
  relocate(peche_loisir_total, .after = all_of("id_hex"))

grid_out2 <- hex_cinquieme %>%
  left_join(., agg2, by = "id_hex") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  relocate(left, top, right, bottom, .after = everything())

st_write(
  obj = grid_out2,
  dsn = paste0(paths$processed_peche_hex, "us_med_pnmcca_observatoire_pecheloisir_hexcinquieme_ofb_pol.gpkg"),
  layer = "hex_cinquieme_peche_all",
  delete_dsn = TRUE
)

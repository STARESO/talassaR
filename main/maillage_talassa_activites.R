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

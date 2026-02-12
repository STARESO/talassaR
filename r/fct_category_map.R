#' ---
#' title : "talassaR - fct_category_map"
#' author : Aubin Woehrel
#' creation date : 2025-10-20
#' last modification : 2025-10-21
#' ---
#'
#' =============================================================================
#'
#' talassaR :
#' Fct category map
#'
#' Description :
#' Function to map donia data for one defined category, here named focus_var
#'
#' =============================================================================

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
      palette = "Greys", # or "plasma", "inferno", etc.
      domain = range(all_focused[!is.na(all_focused)]),
      reverse = TRUE
    )

    # Precomputing colors
    colors <- ifelse(
      is.na(all_focused),
      "red", # Color for NA values
      pal(all_focused) # Color for non-NA values
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
      "red", # Color for NA values
      pal(data_spatial[[focus_var]]) # Color for non-NA values
    )
  }

  # Leaflet Map
  map_optimized <- leaflet(data_spatial) %>%
    # addProviderTiles(providers$Esri.WorldImagery) %>%
    addPolygons(data = pnm_borders, color = "lightblue", weight = 10) %>%
    addCircleMarkers(
      radius = 4,
      stroke = FALSE,
      fillOpacity = 1,
      color = colors, # Use the precomputed colors (red for NA)
      popup = ~ paste(
        "Nom :", nom_bateau, "<br>",
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

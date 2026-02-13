#' ---
#' title : "talassaR - fct_map_peche
#' author : Aubin Woehrel
#' creation date : 2026-02-13
#' ---
#'
#' =============================================================================
#'
#' talassaR :
#' Fct map peche
#'
#' Description :
#' Petite fonction pour afficher les données d'enquête de pêche sous forme
#' de carte interactive.
#'
#' =============================================================================


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

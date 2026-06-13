#' ---
#' title : "talassaR - carroyage_talassa_habitats"
#' author : Aubin Woehrel
#' creation date : 2026-03-18
#' ---
#'
#' =============================================================================
#'
#' talassaR : carroyage des données d'habitat
#'
#' Description :
#' Script permettant de passer des données d'habitats au format TALASSA polygone
#' ou ponctuel vers un format au carroyage TALASSA choisi.
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

# Spatial
library("sf")

## Ressources locales ----
paths <- yaml::read_yaml("config/paths.yml")

## Import des données -----

# Habitats
habitats_talassa_hex <- st_read(paths$processed$talassa_habitats_hexcinquieme) # Version découpée sur mailles via QGIS
habitats_talassa_arp <- st_read(paths$processed$talassa_habitats_arp)

# Carroyages ----
carroyage_hexcinquieme <- st_read(paths$raw$carroyage_hexcinquieme)
carroyage_arp <- st_read(paths$raw$carroyage_arp)

# Côte Corse
cote_corse <- st_read(paths$raw$corsica_borders)

# Sensibilités aux pressions
sensibilite <- readRDS(paths$processed$mat_sensibilite)


# Pré-check ----
if ("geom" %in% names(cote_corse)) {
  cote_corse <- cote_corse %>%
    rename(geometry = geom)
}


# Check validité délimitation Corse
# Si invalide --> corriger source Qgis (pour vérifications autre que R)
st_is_valid(cote_corse, reason = TRUE)[which(st_is_valid(cote_corse) == FALSE)]
which(st_is_valid(cote_corse) == FALSE)


# Fonction de traitement ----
#' Traitement des habitats sur grille carroyée
#'
#' @param grid sf object - La grille (carroyage) à utiliser
#' @param habitats_talassa sf object - Habitats déjà découpés selon le carroyage via Qgis
#' @param grid_id_col character - Nom de la colonne ID dans la grille d'entrée (ex: "id_hex", "id_square")
#'
#' @return list avec deux éléments:
#'   - carroyage: grille avec surfaces (colonnes: id2, srm, surf_cel, surf_ter, surfmer, zone, geometry)
#'   - habitats: grille avec habitats au format long (colonnes: id2, srm, surf_cel, surf_ter, surfmer, surfhab_cel, surfhab_pcel, hab_lib, talassa_code, hab_iq, ...)
#'
process_habitats_grid <- function(grid, habitats_talassa, grid_id_col) {

  if (!grid_id_col %in% names(grid)) {
    stop(paste0("Colonne d'identifiant introuvable dans la grille : ", grid_id_col))
  }

  # Renommer la colonne d'ID en "id2" pour uniformité interne
  grid <- grid %>%
    rename(id2 = .data[[grid_id_col]]) %>%
    mutate(id2 = as.numeric(id2)) %>%
    st_transform(., crs = 4326)

  habitats_talassa <- habitats_talassa %>%
    rename(id2 = .data[[grid_id_col]]) %>%
    mutate(id2 = as.numeric(id2)) 

  # Renommer la géométrie si nécessaire pour grille
  if ("geom" %in% names(grid)) {
    grid <- grid %>%
      rename(geometry = geom)
  }

  # Renommer la géométrie si nécessaire pour habitats
  if ("geom" %in% names(habitats_talassa)) {
  habitats_talassa <- habitats_talassa %>%
    rename(geometry = geom)
}

  # Calculs des surfaces carroyage ----
  print("Calcul surfaces carroyage")
  carroyage_grid <- grid %>%
    select(id2) %>%
    st_transform(., crs = 2154) %>%
    mutate(aire_maille = round(st_area(geometry))) %>%
    relocate(aire_maille, .after = id2) %>%
    st_transform(., crs = 4326)

  carroyage_cote <- st_difference(carroyage_grid, cote_corse) %>%
    st_transform(., crs = 2154) %>%
    mutate(
      aire_mer = round(st_area(geometry)),
      aire_terre = aire_maille - aire_mer
    ) %>%
    relocate(geometry, .after = last_col()) %>%
    st_transform(., crs = 4326)

  carroyage_final <- carroyage_cote %>%
    st_drop_geometry() %>%
    select(-aire_maille) %>%
    left_join(
      x = carroyage_grid,
      y = .,
      by = "id2"
    ) %>%
    mutate(srm = "MED") %>%
    relocate(srm, .after = id2)


  # Calcul surface habitats ----
  print("Calcul surfaces habitats")

  # Calcul aire habitat par polygone
  habitats_temp <- habitats_talassa %>%
    mutate(aire_habitat = as.numeric(st_area(geometry)))

  #  Somme des aires par maille
  habitats_sum <- habitats_temp %>%
    st_drop_geometry(.) %>%
    group_by(id2, talassa_code, talassa_intitule, hab_iq) %>%
    summarise(
      aire_habitat = round(sum(aire_habitat, na.rm = TRUE)),
      .groups = "drop"
    )

  # Calcul des pourcentages d'habitats, de terre et de mer
  carroyage_habitats <- left_join(
    x = habitats_sum,
    y = carroyage_final,
    by = "id2"
  ) %>%
    mutate(
      pourcentage_habitat = as.numeric(round(aire_habitat / aire_mer * 100, 2)),
      pourcentage_terre = as.numeric(round(aire_terre / aire_maille * 100, 2)),
      pourcentage_mer = 100 - pourcentage_terre
    ) %>%
    relocate(srm, .after = id2) %>%
    st_drop_geometry() %>%
    select(-geometry)


  # Jointure des données habitats au carroyage final
  carroyage_habitats <- left_join(
    x = carroyage_habitats,
    y = carroyage_final %>% select(id2, geometry),
    by = "id2"
  )

  # Matrice de sensibilite
  sensibilite_temp <- sensibilite %>%
    rename(talassa_intitule_check = talassa_intitule)

  # Jointure de la sensibilité au carroyage + données habitats pour chaque type d'habitat
  carroyage_habitats <- left_join(
    x = carroyage_habitats,
    y = sensibilite_temp,
    by = "talassa_code"
  )

  # Finalisation données format final ----
  print("Formatage final")
  carroyage_habitats_final <- carroyage_habitats %>%
    rename(
      surf_cel = aire_maille,
      surf_ter = aire_terre,
      surfmer = aire_mer,
      surfhab_cel = aire_habitat,
      surfhab_pcel = pourcentage_habitat,
      hab_lib = talassa_intitule
    ) %>%
    select(-c(talassa_intitule_check, pourcentage_mer, pourcentage_terre)) %>%
    select(id2, srm, surf_cel, surf_ter, surfmer, surfhab_cel, surfhab_pcel, hab_lib, talassa_code, hab_iq, everything())

  carroyage_final <- carroyage_final %>%
    rename(
      surf_cel = aire_maille,
      surf_ter = aire_terre,
      surfmer = aire_mer
    ) %>%
    relocate(surf_ter, .after = surf_cel)

  carroyage_habitats_final <- carroyage_habitats_final %>%
    mutate(across(c(surf_cel, surf_ter, surfmer), \(x) {as.numeric(x) * 10^-6})) %>%
    st_as_sf(.)

  carroyage_final <- carroyage_final %>%
    mutate(across(c(surf_cel, surf_ter, surfmer), \(x) {as.numeric(x) * 10^-6})) %>%
    mutate(zone = NA) %>%
    relocate(geometry, .after = last_col())

  return(list(
    carroyage = carroyage_final,
    habitats = carroyage_habitats_final
  ))
}


# carroyage habitats hex5
final_hex5 <- process_habitats_grid(
  grid = carroyage_hexcinquieme, 
  habitats_talassa = habitats_talassa_hex,
  grid_id_col = "id_hex"
)

# carroyage habitats arp
final_arp <- process_habitats_grid(
  grid = carroyage_arp, 
  habitats_talassa = habitats_talassa_arp,
  grid_id_col = "id2"
)


# Exports ----

## Carroyage hexagonal un cinquième de mile ----

# Carroyage final hex5
st_write(
  obj = final_hex5$carroyage,
  dsn = paths$processed$hex_carroyage,
  driver = "gpkg",
  append = FALSE
)

# Carroyage habitats hex5
st_write(
  obj = final_hex5$habitats,
  dsn = paths$processed$hex_habitats,
  driver = "gpkg",
  append = FALSE
)

## Carroyage carré 1 mile nautique type arp (analyse risque peche) ----

# Carroyage final arp
st_write(
  obj = final_arp$carroyage, 
  dsn = paths$processed$arp_carroyage, 
  driver = "gpkg",
  append = FALSE
)

# Carroyage habitats arp
st_write(
  obj = final_arp$habitats,
  dsn = paths$processed$arp_habitats,
  driver = "gpkg",
  append = FALSE
)





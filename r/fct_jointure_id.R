#' ---
#' title : "talassaR - fct_jointure_id"
#' author : Aubin Woehrel
#' creation date : 2026-02-27
#' ---
#'
#' =============================================================================
#'
#' talassaR : Jointure des identifiants de maillage
#'
#' Description :
#' Fonction permettant de joindre les identifiants des mailles du carroyage
#' choisi aux jeux de données des activités ou d'habitats.
#' Permet par la suite de réaliser les agrégations des données d'activités
#' ou d'habitats par mailles dans le processus de carroyage
#'
#' =============================================================================


jointure_id <- function(
  carroyage = NULL,
  data = NULL,
  id_name = "id_hex"
) {
  # Check présence données-arguments fct
  if (is.null(carroyage) | is.null(data)) {
    stop("Il manque certaines valeurs d'arguments")
  }

  # Check crs carroyage et transfo si besoin
  if (st_crs(carroyage)$epsg != 4326) {
    carroyage <- st_transform(carroyage, crs = 4326)
    warning("Mauvaise projection. Reprojection du carroyage vers crs 4326.")
  }

  # Check crs données activites ou habitats et transfo si besoin
  if (st_crs(data)$epsg != 4326) {
    data <- st_transform(data, crs = 4326)
    warning("Mauvaise projection. Reprojection des données vers crs 4326.")
  }

  # Jointure spatiale par intersection
  data_joined <- st_join(
    x = data,
    y = carroyage,
    join = st_intersects
  )

  # Selection des colonnes du jeu de données + ID du carroyage uniquement
  data_joined <- data_joined %>%
    select(id_name, c(names(data)))

  # Nombre d'entités non jointes (car pas en intersection avec la maille)
  nb_out <- sum(is.na(data_joined[id_name]))

  # En cas d'entités non jointes, élimination de ces entités
  if (nb_out > 0) {
    data_joined <- data_joined %>%
      filter(!is.na(data_joined[[id_name]]))
    warning(paste("Points éliminés car hors de la zone de carroyage :", nb_out))
  }

  # Elimination géométrie (sera remise en place lors de la jointure post-agrégation)
  data_joined <- data_joined %>%
    st_drop_geometry() %>%
    rename(id2 = id_name) # Nouveau nom vers id2 pour harmonisation maillage
}

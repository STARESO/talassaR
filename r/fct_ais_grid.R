#' ---
#' title : "talassaR - fct_ais_grid"
#' author : Aubin Woehrel
#' creation date : 2026-06-03
#' ---
#'
#' =============================================================================
#'
#' talassaR : Fonctions d'agrégation AIS vers maillage
#'
#' Description :
#' Fonctions pour calculer l'état de mouvement des points AIS
#' et agréger les données par maille de carroyage.
#'
#' Workflow:
#' 1) Charger AIS formaté (points spatialisés, talassa_code/intitule assignés)
#' 2) Calculer mouvement (transit vs stationnaire)
#' 3) Coder activité basée sur mouvement: 
#'    - transit -> garder talassa_code du jeu formaté
#'    - stationnaire -> forcer "recz_00_f00_a03" (ancrage)
#' 4) Agréger par maille et activité
#'
#' =============================================================================

library("dplyr")
library("sf")
library("lubridate")

#' Calculer l'état de mouvement pour chaque ping AIS
#'
#' Basé sur speed et/ou distance par rapport au ping précédent du même MMSI.
#' N'utilise PAS le champ status (non fiable).
#'
compute_ais_movement_state <- function(
  ais,
  speed_field = "speed",
  time_field = "timestamp",
  speed_threshold = 2,
  distance_threshold = 200
) {
  stopifnot(inherits(ais, "sf"))
  
  if (!speed_field %in% names(ais)) {
    stop("Champ de vitesse introuvable : ", speed_field)
  }
  if (!time_field %in% names(ais)) {
    stop("Champ de temps introuvable : ", time_field)
  }
  if (!"mmsi" %in% names(ais)) {
    stop("La colonne mmsi est requise.")
  }

  ais <- ais %>%
    mutate(
      speed = as.numeric(.data[[speed_field]]),
      timestamp = lubridate::ymd_hms(as.character(.data[[time_field]]), tz = "UTC", quiet = TRUE)
    )

  if (any(is.na(ais$timestamp))) {
    warning("Certaines dates AIS n'ont pas pu être parsées et seront traitées comme NA.")
  }

  ais <- ais %>%
    mutate(.geom = st_geometry(.)) %>%
    arrange(mmsi, timestamp) %>%
    group_by(mmsi) %>%
    mutate(
      prev_geom = lag(.geom),
      speed_status = case_when(
        is.na(speed) ~ NA_character_,
        speed <= speed_threshold ~ "stationary",
        TRUE ~ "transit"
      ),
      dist_status = case_when(
        is.na(prev_geom) ~ NA_character_,
        as.numeric(st_distance(.geom, prev_geom, by_element = TRUE)) <= distance_threshold ~ "stationary",
        TRUE ~ "transit"
      ),
      movement_state = case_when(
        is.na(prev_geom) ~ "unknown",
        speed_status == "stationary" ~ "stationary",
        dist_status == "stationary" ~ "stationary",
        TRUE ~ "transit"
      )
    ) %>%
    ungroup() %>%
    select(-.geom, -prev_geom, -speed_status, -dist_status)

  ais
}

#' Recoder l'activité basée sur l'état de mouvement
#'
#' Logique:
#' - Si mouvement = transit: garder talassa_code original
#' - Si mouvement = stationnaire: forcer talassa_code = "recz_00_f00_a03" (ancrage)
#'
recode_ais_activity_by_movement <- function(
  ais,
  ancrage_code = "recz_00_f00_a03",
  ancrage_intitule = "ancrage"
) {
  stopifnot(inherits(ais, "sf"))
  
  if (!"movement_state" %in% names(ais)) {
    stop("Le jeu de données doit contenir movement_state.")
  }
  if (!"talassa_code" %in% names(ais) || !"talassa_intitule" %in% names(ais)) {
    stop("Le jeu de données doit contenir talassa_code et talassa_intitule.")
  }

  ais <- ais %>%
    mutate(
      talassa_code_final = case_when(
        movement_state == "stationary" ~ ancrage_code,
        TRUE ~ talassa_code
      ),
      talassa_intitule_final = case_when(
        movement_state == "stationary" ~ ancrage_intitule,
        TRUE ~ talassa_intitule
      )
    )

  ais
}

#' Agréger AIS par maille et par code d'activité
#'
aggregate_ais_to_grid <- function(
  ais,
  carroyage,
  id_name,
  activity_code_field,
  activity_intitule_field,
  value_method = c("unique_vessels", "pings")
) {
  stopifnot(inherits(ais, "sf"))
  stopifnot(inherits(carroyage, "sf"))
  value_method <- match.arg(value_method)

  if (!activity_code_field %in% names(ais)) {
    stop("Champ de code activité introuvable : ", activity_code_field)
  }
  if (!activity_intitule_field %in% names(ais)) {
    stop("Champ d'intitulé activité introuvable : ", activity_intitule_field)
  }

  ais_joined <- jointure_id(carroyage = carroyage, data = ais, id_name = id_name)

  if (value_method == "pings") {
    agg <- ais_joined %>%
      group_by(.data[[id_name]], .data[[activity_code_field]]) %>%
      summarize(
        activity_value = n(),
        n_distinct_mmsi = n_distinct(mmsi),
        .groups = "drop"
      )

  } else {
    # Pour l'instant se base que sur le nombre de bateaux mmsi par grille et activité
    # Sans prendre en compte le temps de résidence du bateau dans la maille
    # A améliorer dans le futur.
    agg <- ais_joined %>%
      group_by(.data[[id_name]], .data[[activity_code_field]], .data[[activity_intitule_field]]) %>%
      summarize(
        activity_value = n_distinct(mmsi),
        n_pings = n(),
        .groups = "drop"
      )
  }

  agg <- agg %>%
    rename(talassa_code = all_of(activity_code_field)) %>%
    rename(id2 = all_of(id_name)) %>%
    mutate(
      talassa_intitule = .data[[activity_intitule_field]]
    ) %>%
    select(id2, talassa_code, talassa_intitule, activity_value, everything())

  # Changement nom id carroyage pour jointure geometrie
  carroyage_geometry <- carroyage %>%
    select(all_of(id_name)) %>%
    rename(id2 = all_of(id_name))

  # Réintégration géométrie du carroyage pour pouvoir exporter en GeoPackage
  agg <- left_join(agg, carroyage_geometry, by = "id2")

  agg
}

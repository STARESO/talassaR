#' ---
#' title : "talassaR - fct_intensite_computation"
#' author : Aubin Woehrel
#' creation date : 2026-06-03
#' ---
#'
#' =============================================================================
#'
#' talassaR : fonction d'intensité de computation
#'
#' Description :
#' Fonctions utilitaires pour le script maillage_talassa_activites.R
#' =============================================================================


# Calcul intensité par maille et activités à partir d'un jeu de données à disposition. 
#' @data nom de la variable des données sources
#' @type type de jeu de données source

intensite_computation <- function(
  data,
  type,
  id_carroyage = "id2",
  scale_min = 0.01,
  scale_max = 1
) {
  # Calcul des poids par entités sur la base de formules et variables à disposition
  data_new <- data %>%
    rowwise() %>%
    mutate(
      cweight = ifelse(
        is.na(formule),
        1, # A verifier si ok dans toutes les situations
        eval(parse(text = formule))
      )
    )

  names(data_new)

  # Calcul du niveau d'intensité basé sur les poids
  data_new <- data_new %>%
    # Somme par cellule et par activité
    group_by(.data[[id_carroyage]], talassa_code, talassa_intitule) %>%
    summarize(
      intensite = sum(cweight),
      ic = unique(ic) # Ici ou autre endroit plus adapté ?
    ) %>%
    # Réechelonnage par activité entre valeurs scale_min et scale_max
    group_by(talassa_code, talassa_intitule) %>% # @awoehrel TODO: check si changement pour normalisation sans activité 
    mutate(intensite = scales::rescale(intensite, to = c(scale_min, scale_max))) %>%
    arrange(talassa_code, intensite)

  # Ajout de la colonne type pour les étapes prochaines
  data_new <- data_new %>%
    mutate(type = type)
}


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


#' ---
#' title : "talassaR - fct_ais_reading"
#' author : Aubin Woehrel
#' creation date : 2026-05-07
#' ---
#'
#' =============================================================================
#'
#' talassaR :
#' Fonctions lectures AIS
#'
#' Description :
#' Fonctions pour lire les données AIS de Marine Traffic à partir de la liste des 
#' fichiers json fournis. Permet aussi la concaténation des données ais 
#' au format dataframe ou alors en ajout progressif à la base de données
#'
#' =============================================================================


# Référence des colonnes à conserver ----
ref_columns <- c(
  "mmsi", "imo", "ship_id", "lat", "lon", "speed", "heading",
  "course", "status", "timestamp", "dsrc", "utc_seconds", "market", "shipname",
  "shiptype", "callsign", "flag", "length", "width", "grt", "dwt", "draught",
  "year_built", "ship_country", "ship_class", "rot", "type_name", "ais_type_summary",
  "destination", "eta", "l_fore", "w_left", "last_port", "last_port_time",
  "last_port_id", "last_port_unlocode", "last_port_country", "current_port",
  "current_port_id", "current_port_unlocode", "current_port_country", "next_port_id",
  "next_port_unlocode", "next_port_name", "next_port_country", "eta_calc",
  "eta_updated", "distance_to_go", "distance_travelled", "avg_speed", "max_speed", 
  "date_from", "date_to"
)

# Fonction permettant de lire un fichier unique (section temporelle) de données ais ----
read_section <- function(json_name) {
  path_json <- paste0(paths$raw_ais_marinetraffic, "/", json_name)
  data_json <- rjson::fromJSON(file = path_json)

  if(!is.null(data_json$errors)) {
    return("base_file_error")
  } 

  data_framed <- as.data.frame(do.call(rbind, data_json$DATA)) %>%
    mutate(
    date_from = data_json$METADATA$DATE_FROM, 
    date_to = data_json$METADATA$DATE_TO
  ) %>%
  rename_with(stringr::str_to_lower) 

  if (length(names(data_framed)) != length(ref_columns)) {
    return("different_column_amount")
  }
  
  if (sum(names(data_framed) != ref_columns) > 0) {
    return("wrong_column_names")
  } 

  data_framed <- data_framed %>%
    relocate(date_from, date_to, .before = mmsi)
}


# Fonction compilation ais au format dataframe ----
bind_ais <- function(json_names) {
  temp_list <- data.frame() # Initialisation jeu de données nul
  error_list <- NULL # Liste des erreurs à renvoyer

  for(i in 1:length(json_names)) { # Boucle sur les fichiers ais individuels
    print(paste(i, ":", json_names[i])) 

    # Extraction données
    data_temp <- read_section(json_names[i])

    # Rajout à liste erreur si présente, sinon concaténation lignes
    if (is.character(data_temp)) {
      print(data_temp)
      error_list <- c(error_list, paste(json_names[i], ":", data_temp))
    } else {
      temp_list <- rbind(temp_list, data_temp)
    }
  }

  return(list(data = temp_list, errors = error_list))
}
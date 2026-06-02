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

library("dplyr")
library("purrr")
library("furrr")
library("progressr")
library("tictoc")
library("stringr")


# Colonnes de référence
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

json_to_df <- function(json_name) {
  path_json <- file.path(paths$raw$ais_marinetraffic, json_name)

  data_json <- tryCatch(
    rjson::fromJSON(file = path_json),
    error = function(e) return(NULL)  # Return NULL if JSON reading fails
  )

  if (!is.null(data_json$errors)) {
    return(NULL)  # Return NULL if JSON has errors
  }

  if (is.null(data_json$DATA) || length(data_json$DATA) == 0) {
    return("no_data")
  }

  data_framed <- tryCatch(
    {
      as.data.frame(bind_rows(data_json$DATA)) %>%
        mutate(
          date_from = data_json$METADATA$DATE_FROM,
          date_to = data_json$METADATA$DATE_TO
        ) %>%
        rename_with(str_to_lower)
    },
    error = function(e) return(NULL)  # Return NULL if conversion fails
  )

  return(data_framed)  # Return the data frame (or NULL if failed)
}


read_section <- function(json_name) {
  data_framed <- json_to_df(json_name)

  # Initialize warnings as NULL (no warnings by default)
  warnings <- NULL

  # Skip if data_framed is NULL (error in json_to_df)
  if (is.null(data_framed)) {
    warnings <- paste(json_name,  ": failed to read or convert")
    return(list(data = NULL, warnings = warnings))
  }

  if (is.character(data_framed) && data_framed == "no_data") {
    warnings <- paste(json_name,  ": no existing data")
    return(list(data = NULL, warnings = warnings))
  }

  # Check for column mismatches and record warnings
  # if (length(names(data_framed)) != length(ref_columns)) {
  #   # warnings <- append(warnings, paste("Column count mismatch in", json_name, ":", length(names(data_framed)), "vs reference", length(ref_columns)))
  # }

  if (!all(names(data_framed) %in% ref_columns)) {
    missing_cols <- setdiff(ref_columns, names(data_framed))
    extra_cols <- setdiff(names(data_framed), ref_columns)
    if (length(missing_cols) > 0) {
      warnings <- append(warnings, paste(json_name, ": missing columns", paste(missing_cols, collapse = ", ")))
    }
    if (length(extra_cols) > 0) {
      warnings <- append(warnings, paste(json_name, ": extra columns", paste(extra_cols, collapse = ", ")))
    }
  }

  # Return the data frame and warnings
  return(list(data = data_framed, warnings = warnings))
}


bind_ais <- function(json_names) {
  tic()
  with_progress({
    p <- progressor(steps = length(json_names))

    # Process files sequentially
    results <- map(
      json_names,
      ~ {
        p()  # Update progress bar
        read_section(.x)  # Returns list(data = df, warnings = character_vector)
      }
    )

    # Separate data and warnings
    data_list <- map(results, ~ .x$data)  # Extract data frames
    warnings_list <- map(results, ~ .x$warnings)  # Extract warnings

    # Combine data (skip NULL entries)
    data <- bind_rows(compact(data_list))

    # Combine warnings (flatten and remove NULLs)
    all_warnings <- unlist(compact(warnings_list))
  })

  elapsed <- toc()

  return(list(
    data = data,
    warnings = all_warnings,  # Return warnings instead of errors
    computation_time = elapsed$callback_msg
  ))
}


bind_ais_parallel <- function(json_names, workers = 10) {
  tic()
  # Enable parallel processing
  plan(multisession, workers = workers)

  with_progress({
    p <- progressor(steps = length(json_names))

    # Process files in parallel
    results <- future_map(
      json_names,
      ~ {
        p()  # Update progress bar
        read_section(.x)  # Returns list(data = df, warnings = character_vector)
      }
    )

    # Separate data and warnings
    data_list <- map(results, ~ .x$data)  # Extract data frames
    warnings_list <- map(results, ~ .x$warnings)  # Extract warnings

    # Combine data (skip NULL entries)
    data <- bind_rows(compact(data_list))

    # Combine warnings (flatten and remove NULLs)
    all_warnings <- unlist(compact(warnings_list))
  })

  elapsed <- toc()

  return(list(
    data = data,
    warnings = all_warnings,  # Return warnings instead of errors
    computation_time = elapsed$callback_msg
  ))
}

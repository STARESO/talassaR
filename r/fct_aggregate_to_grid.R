#' ---
#' title : "talassaR - fct_aggregate_to_grid"
#' author : Aubin Woehrel
#' creation date : 2025-10-21
#' last modification : 2025-10-21
#' ---
#'
#' =============================================================================
#'
#' talassaR :
#' Function aggregate to grid
#'
#' Description :
#' Function to aggregate donia point data to grid
#'
#' =============================================================================


# Aggregating function ----
aggregate_to_grid <- function(
  grid_layer,
  transfo = NULL, # type of transformation
  id_field = "id", # id field name for layer joining
  output = NULL, # output file name
  layer_name = NULL, # output layer name
  overwrite = TRUE # To overwrite previous file
) {
  stopifnot(inherits(grid_layer, "sf"))
  if (is.na(st_crs(grid_layer))) stop("grid_layer has no CRS.")

  # 1) Bring DONIA points to the grid CRS
  pts <- donia_talassa %>%
    st_transform(st_crs(grid_layer))

  # 2) Spatial join to attach grid id to points
  pts_j <- pts %>%
    st_join(
      grid_layer %>%
        select(all_of(id_field)),
      join = st_intersects, left = TRUE
    )

  # 3) Weighted value per point
  if (transfo == "ln_mouillage") {
    base <- pts_j %>%
      st_drop_geometry() %>%
      mutate(
        w = if_else(
          !is.na(taille) & !is.na(duree_mouillage) & duree_mouillage > 0,
          taille * log(duree_mouillage),
          NA_real_
        )
      )
  } else if (transfo == "ln_all") {
    base <- pts_j %>%
      st_drop_geometry() %>%
      mutate(
        w = if_else(
          !is.na(taille) & !is.na(duree_mouillage) & duree_mouillage > 0,
          log(taille) * log(duree_mouillage),
          NA_real_
        )
      )
  } else if (transfo == "sqrt_ln") {
    base <- pts_j %>%
      st_drop_geometry() %>%
      mutate(
        w = if_else(
          !is.na(taille) & !is.na(duree_mouillage) & duree_mouillage > 0,
          sqrt(taille) * log(duree_mouillage),
          NA_real_
        )
      )
  } else if (transfo == "cubert_ln") {
    base <- pts_j %>%
      st_drop_geometry() %>%
      mutate(
        w = if_else(
          !is.na(taille) & !is.na(duree_mouillage) & duree_mouillage > 0,
          (taille^(1 / 3)) * log(duree_mouillage),
          NA_real_
        )
      )
  }

  # 4) Total per cell
  agg_tot <- base %>%
    group_by(.data[[id_field]]) %>%
    summarise(act_all = sum(w, na.rm = TRUE), .groups = "drop")

  # 5) Per-category per cell, pivot wide
  agg_wide <- base %>%
    group_by(.data[[id_field]], resoblo_intitule) %>%
    summarise(act_sum = sum(w, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from  = resoblo_intitule,
      values_from = act_sum,
      values_fill = 0
    )

  # 6) Merge totals, tidy
  agg <- agg_wide %>%
    left_join(agg_tot, by = id_field) %>%
    relocate(act_all, .after = all_of(id_field))

  # 7) Shorter names (applies only to columns that exist)
  rename_map <- c(
    ferry     = "transport de passagers ferry",
    moteur    = "plaisance a moteur",
    marchand  = "transport maritime de marchandises",
    ravitalr  = "transport par ravitailleur",
    voile     = "plaisance a voile",
    yacht     = "grande plaisance",
    depollu   = "activite de depollution",
    cargo     = "transport par cargo",
    croisr    = "croisere sur paquebot",
    tankr     = "transport par tanker",
    portur    = "circulation des navires professionnels dans les ports",
    navette   = "transport de passagers navette ou high speed craft",
    passagers = "transport maritime de passagers",
    svtage    = "recherche et sauvetage en mer",
    plongee   = "plongee avec assistance respiratoire",
    science   = "activite de recherche scientifique en mer",
    remorqg   = "activite de remorquage"
  )

  agg <- agg %>%
    rename(!!!rename_map)

  # 8) Join back to grid
  grid_out <- grid_layer %>%
    left_join(., agg, by = id_field) %>%
    mutate(across(everything(), ~ replace_na(., 0)))

  # 9) Optional write
  if (!is.null(output)) {
    st_write(grid_out, output, layer = layer_name, delete_dsn = overwrite)
  }

  return(grid_out)
}

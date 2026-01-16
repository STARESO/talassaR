#' ---
#' title : "talassaR - survols_gpkg_check"
#' author : Aubin Woehrel
#' creation date : 2025-11-18
#' ---
#'
#' =============================================================================
#'
#' talassaR :
#' Survols gpkg check
#'
#' Description :
#' Small script to check a second time the survols usages data after manual
#' correction in QGIS.
#'
#' =============================================================================

# Initialization ----

## Clean up and working directory ----
rm(list = ls())

## Library imports ----

# Data import and tidying
library("readr")
library("dplyr")
library("tidyr")
library("stringr")
library("purrr")

# Data exploration
library("skimr")

# Data representations
library("ggplot2")

# Spatial
library("sf")

## Sourcing local resources ----
source("r/paths.R")

## Importing data ----

file_names <- list.files(paths$processed_survols_corrected, full.names = TRUE)

survols_all <- file_names %>%
  map(st_read, quiet = TRUE) %>%
  bind_rows()


# General check ----
skim(survols_all)

resoblo_verif <- survols_all %>%
  st_drop_geometry() %>%
  select(resoblo_code, resoblo_intitule) %>%
  group_by(resoblo_code, resoblo_intitule) %>%
  summarise(n = n()) %>%
  ungroup()

# If mistakes, give line number of the object to check in source data
# with line numbers corresponding to resoblo_verif dataset
to_slice <- c(1, 3, 4, 6, 12, 14, 16, 20, 31, 39)
to_slice <- c(13)

resoblo_to_correct <- resoblo_verif %>%
  slice(to_slice) %>%
  mutate(key_resoblo = str_c(resoblo_code, resoblo_intitule, sep = "_"))

to_correct <- survols_all %>%
  st_drop_geometry() %>%
  mutate(key_resoblo = stringr::str_c(resoblo_code, resoblo_intitule, sep = "_")) %>%
  right_join(., resoblo_to_correct) %>%
  select(id_acti, date, resoblo_code, resoblo_intitule, key_resoblo)

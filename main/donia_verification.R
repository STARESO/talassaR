#' ---
#' title : "talassaR - Donia verification"
#' author : Aubin Woehrel
#' creation date : 2025-09-15
#' last modification : 2025-09-15
#' ---
#'
#' =============================================================================
#' 
#' talassaR : 
#' Donia verification
#' 
#' Description : 
#' Small script to investigate and prepare data of Donia for the talassa project
#' in the PNMCCA. Idea is to verify how data is structure and figure out how 
#' to correct and preprocess the data for further integration in a grid
#' 
#' =============================================================================


# Initialization ----

## Clean up and working directory ----
rm(list = ls())

## Library imports ----

# Data tidying
library("dplyr")
library("tidyr")

# Progress bar
library("progress")

## Sourcing paths and constants ----
source("R/paths.R")
#source("R/constants.R")
source("R/taxalist.R")


library("readr")
donia <- read_delim(
  "data/raw/mouillage_donia/donia.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE
)
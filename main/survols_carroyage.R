#' ---
#' title : "talassaR - survols_carroyage"
#' author : Aubin Woehrel
#' creation date : 2026-02-03
#' ---
#'
#' =============================================================================
#'
#' talassaR :
#' Survols carroyage
#'
#' Description :
#' Mise en carroyage des données ponctuelles de survols aériens. Utilisé
#' principalement pour le carroyage hexagonal d'1/5 de mile
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

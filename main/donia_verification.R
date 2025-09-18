#' ---
#' title : "talassaR - donia verification"
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

# Data import and tidying
library("readr")
library("dplyr")
library("tidyr")

## Sourcing paths and constants ----
# source("R/paths.R")

## Importing data ----

donia <- read_delim(
  "data/raw/mouillage_donia/donia.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE
)

# Preprocess ----

## First investigations ----
spec(donia)
skimr::skim(donia)
names(donia)

## Small transformations ----
donia_test <- donia %>%
  separate_wider_delim(
    col = date_mouillage_central_european_time,
    delim = " ",
    names = c("date", "time")
  ) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))

t1 <- donia_test %>%
  group_by(nom) %>%
  summarize(n = n()) %>%
  arrange(nom, desc(n))

t1sum <- t1 %>%
  mutate(type = case_when(
    nom == "/" ~ "unnamed",
    TRUE ~ "named"
  )) %>%
  group_by(type) %>%
  summarize(n = sum(n))

t1sum



t2 <- donia_test %>%
  group_by(nom, date) %>%
  summarize(n = n()) %>%
  arrange(nom, desc(n), date)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
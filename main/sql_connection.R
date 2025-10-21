#' ---
#' title : "talassaR - postgresql connection"
#' author : Aubin Woehrel
#' creation date : 2025-08-01
#' last modification : 2025-09-15
#' ---
#'
#' =============================================================================
#' 
#' talassaR : 
#' postgresql connection
#' 
#' Description : 
#' Connects to postgreSQL database on local machine. Enables the creation of 
#' datasets in the postgreDB using R, and updates them if needed.
#' =============================================================================


# Initialization ----

## Clean up and working directory ----
rm(list = ls())

# Libraries ----

# Tidyverse
library("dbplyr")
library("dplyr")

# Database connections
library("RPostgres")
library("odbc")
library("DBI")

# Sourcing local scripts ----

# Connection with RPostgres package ----

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "postgres",
  host     = "localhost",     # or IP, e.g., "192.168.1.100"
  port     = 5432,
  user     = "postgres",
  password = "stareso1972"
)

# Connection with odbc package (doesnt work) ----
if (FALSE) {
  con <- dbConnect(
    odbc::odbc(),
    driver = "PostgreSQL Driver",
    database = "test_db",
    uid    = Sys.getenv("DB_USER"),
    pwd    = Sys.getenv("DB_PASSWORD"),
    host   = "localhost",
    port   = 5432
  ) 
}

# Testing the different function of the package
dbListTables(con)

# Importing survols data
survols_plaba <- readRDS(paths$survols_plaba)
survols_usages <- readRDS(paths$survols_usages)

# Writing tables to database
dbWriteTable(con, "raw_survols_plaba", survols_plaba, overwrite = TRUE)
dbWriteTable(con, "raw_survols_usages", survols_usages, overwrite = TRUE)

dbDisconnect(con)


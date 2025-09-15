# Connection to PostgresSQL database with R

# Libraries
library("RPostgres")
library("odbc")
library("DBI")

library("dbplyr")
library("dplyr")

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
survols_plaba <- readRDS("data/raw/us_med_pnmcca_observatoire_survols_plaba.rds")
survols_usages <- readRDS("data/raw/us_med_pnmcca_observatoire_survols_usages.rds")

# Writing tables to database
dbWriteTable(con, "survols_usages", survols_usages, overwrite = TRUE)
dbWriteTable(con, "survols_plaba", survols_plaba, overwrite = TRUE)

dbDisconnect(con)


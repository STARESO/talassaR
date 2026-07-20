#' ---
#' title : "talassaR - zones_index"
#' author : Aubin Woehrel
#' creation date : 2026-06-14
#' ---
#'
#' =============================================================================
#'
#' talassaR : Zones index
#'
#' Description :
#' Script post-modélisation permettant de calculer quelques statistiques 
#' sur les zones d'intérêt choisies à la main.
#'
#' =============================================================================


# Initialisation ----

rm(list = ls())

# Import des librairies et ressources locales 

# Manipulations de données
library("dplyr")
library("tidyr")
library("stringr")

# Données spatiales
library("sf")

# Connections BD
library("RPostgres")
library("rpostgis")

# Config file import
library("yaml")


## Configurations export et chemins ----

# Fichiers yaml
paths <- yaml::read_yaml("config/paths.yml") # Chemins
config <- yaml.load_file("config/secrets.yml") # Secrets de config connection db


# Connection BD ----
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = config$db$dbname,
  host = config$db$host,
  port = config$db$port,
  user = config$db$user,
  password = config$db$password
)

# Import des données 
zones <- st_read(con, query = "SELECT * FROM export_complet_hex5_v5.zones")
mod_simple <- st_read(con, query = "SELECT * FROM export_complet_hex5_v5.index")
mod_mc <- st_read(con, query = "SELECT * FROM export_complet_hex5_v5.stat_simul_1000_hex5_v5_x1111102_groups")

names(mod_simple)
names(mod_mc)

mod_simple <- mod_simple %>% st_drop_geometry() %>% select(-c("geom", "zone"))
mod_mc <- mod_mc %>% st_drop_geometry()

# Jointure des données de modélisation sur les zones
zones <- left_join(zones, mod_simple, by = "id2")
zones <- left_join(zones, mod_mc, by = "id2")
zones <- zones %>% as.data.frame()

# Check nombre de polygones par zone
zones %>% 
  st_drop_geometry() %>%
  count(zone) %>%
  arrange(desc(n))

# Calculs moyenne, max et min de quelques paramètres importants
zones_stats <- zones %>%
  filter(!is.na(zone)) %>%
  group_by(zone) %>%
  summarize(
    polygones = n(),
    refc_simple_mean = mean(refc_e1), 
    refc_simple_min = min(refc_e1), 
    refc_simple_max = max(refc_e1), 
    refc_mc_mean = mean(REFC_moy),
    refc_mc_min = min(REFC_moy),
    refc_mc_max = max(REFC_moy), 
    top10_mean = mean(top10), 
    top10_min = min(top10), 
    top10_max = max(top10)
  )

zones_stats <- zones_stats %>%
  ungroup() %>%
  mutate(
    arg1 = scales::rescale(refc_simple_mean, to = c(0.1, 1)),
    arg2 = scales::rescale(refc_mc_mean, to = c(0.1, 1)),
    arg3 = scales::rescale(top10_mean, to = c(0.1, 1))
  ) %>%
  rowwise() %>%
  mutate(index_zone = weighted.mean(x = c(arg1, arg2, arg3), w = c(3, 3, 2))) %>%
  select(-c(arg1, arg2, arg3))

zones_stats <- zones %>%
  select(id2, zone, geom) %>%
  left_join(., zones_stats, by = "zone") %>%
  st_as_sf()

# Correction des erreurs de géométrie
zones_stats <- zones_stats %>%
  st_make_valid()

zones_stats_polys <- zones_stats %>%
  filter(!is.na(zone)) %>%
  select(-id2) %>%
  group_by(zone) %>%
  summarize(
    across(where(is.numeric), first), 
    do_union = TRUE
  ) 

dbWriteTable(
    conn = con,
    name = Id(schema = "export_complet_hex5_v5", table = "zones_stats"),
    value = zones_stats,
    overwrite = TRUE
  )

dbWriteTable(
    conn = con,
    name = Id(schema = "export_complet_hex5_v5", table = "zones_stats_polys"),
    value = zones_stats_polys,
    overwrite = TRUE
  )

RPostgres::dbDisconnect(con)

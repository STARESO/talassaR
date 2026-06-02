#' =============================================================================
#'
#' talassaR : rajout hab_iq
#'
#' Description :
#' Code dev rapide pour ajouter la colonne hab_iq au jeu de données 
#' habitats intermiédiaires de talassa. Celui-ci correspond au jeu de données 
#' des habitats d'andromèdes corrigés et mis au format talassa par le script 
#' correction_habitats.R puis découpés par un carroyage donné via qgis.
#' (Découpage via R trop lent et n'a pas fonctionné)
#' 
#' =============================================================================


habitats_interm <- st_read(paths$processed$talassa_habitats_intermediaire)

codes_habitats <- read.xlsx(
  xlsxFile = paths$raw$codes_habitats,
  sheet = "codes",
  fillMergedCells = TRUE
)

names(habitats_interm)
names(codes_habitats)

codes_habitats <- codes_habitats %>%
  select(talassa_code, hab_iq)

habitats_interm <- left_join(
  x = habitats_interm, 
  y = codes_habitats, 
  by = join_by(talassa_code)
)

habitats_interm %>%
  st_drop_geometry() %>%
  select(talassa_code, talassa_intitule, hab_iq) %>%
  distinct() %>%
  arrange(desc(hab_iq))


st_write(
  obj = habitats_interm, 
  dsn = paths$processed$talassa_habitats_intermediaire,
  driver = "gpkg",
  append = FALSE
)

# Super fast dev script for extracting id2 numbers of smaller amount of hex for test model

library("dplyr")

smaller_selection <- sf::st_read("data/raw/dev/test_hex_small_format.gpkg")
names(smaller_selection)

id_selection <- smaller_selection %>%
  pull(id2) %>%
  sort() %>%
  unique()

writeLines(
  unlist(lapply(mylist, paste, collapse=" ")), 
  "data/processed/dev/id_subselection.txt"
  )

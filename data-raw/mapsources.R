## code to prepare `DATASET` dataset goes here
library(tidyverse)

# TODO: Need to look into this as a more persistent source of map data
# ons = tibble(boundaryType = c("Administrative_Boundaries",
#                       "Census_Boundaries",
#                       "Electoral_Boundaries",
#                       "Eurostat_Boundaries",
#                       "Health_Boundaries",
#                       "Other_Boundaries",
#                       "Postcodes"))
# #
# ons = ons %>% mutate(services = purrr::map(boundaryType, function(.x) {
# #       tmp = jsonlite::read_json(paste0('https://ons-inspire.esriuk.com/arcgis/rest/services/',.x,'?f=pjson'))
#       tmp = jsonlite::read_json(paste0('https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/',.x,'?f=pjson'))
#       tmp = bind_rows(lapply(tmp$services, as_tibble)) %>%
#         filter(type == 'MapServer') %>%
#         mutate(boundary = stringr::str_extract(name, '(?<=\\/)(.+)'))
# }))
#
# ons = ons %>% unnest(cols = services) %>% rename(feature = name)
# ons %>% View()

here::i_am("data-raw/mapsources.R")
devtools::load_all()
options("arear.cache.dir" = here::here("data-raw/cache"))
fs::dir_create(here::here("data-raw/cache"))
readr::write_lines(x = "^cache$", file = here::here("data-raw/.gitignore"))


## Map sources ----
mapsources = yaml::read_yaml(here::here("data-raw/mapsources.yaml"))
usethis::use_data(mapsources, overwrite = TRUE)

board = pins::board_folder(here::here("pkgdown/assets/maps"))
items = pins::pin_list(board)
options(arear.skip_pins = TRUE)

for (x in names(mapsources)) {
  # x= "WD11"
  if (!x %in% items) {
    try({
      tmp = arear::getMap(x, sources = mapsources)
      pins::pin_write(board, x = tmp, name = x)
      message("Wrote: ", x)
    })
  } else {
    message("Skipped: ", x)
  }
}
pins::pin_write(board, mapsources, type = "json")
pins::write_board_manifest(board)

## UK Bridges and ferry data ----
tmp = yaml::read_yaml(here::here("data-raw/mapconnections.yaml"))
ukconnections = tibble(name = names(tmp), connect = tmp) %>%
  unnest_wider(col = connect) %>%
  unnest_wider(start) %>%
  rename(start.lat = lat, start.long = lng) %>%
  unnest_wider(end) %>%
  rename(end.lat = lat, end.long = lng)
usethis::use_data(ukconnections, overwrite = TRUE)

## London shapefile ----

londonShape = arear::getMap("NHSER20") %>% filter(name == "London")
usethis::use_data(londonShape, overwrite = TRUE)

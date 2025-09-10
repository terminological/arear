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

for (x in names(mapsources)) {
  # x= "WD11"
  if (!x %in% items) {
    try({
      tmp = list()
      tmp[[x]] = arear::getMap(x, sources = mapsources)
      ex = rlang::expr(usethis::use_data(!!as.symbol(x), overwrite = TRUE))
      suppressMessages(with(tmp, eval(ex)))
      message("Wrote: ", x)
    })
  } else {
    message("Skipped: ", x)
  }
}

## UK Bridges and ferry data ----
tmp = yaml::read_yaml(here::here("data-raw/mapconnections.yaml"))
ukconnections = tibble(name = names(tmp), connect = tmp) %>%
  unnest_wider(col = connect) %>%
  unnest_wider(start) %>%
  rename(start.lat = lat, start.long = lng) %>%
  unnest_wider(end) %>%
  rename(end.lat = lat, end.long = lng)
usethis::use_data(ukconnections, overwrite = TRUE)

## Estimates of NHS surge capacity ----
surgecapacity = readr::read_csv(here::here(
  "data-raw/NHSSurgeCapacityMarch2020.csv"
))
surgecapacity = sf::st_as_sf(
  surgecapacity,
  coords = c("long", "lat"),
  crs = 4326
)
usethis::use_data(surgecapacity, overwrite = TRUE)

## UK Demographics estimates ----

# this location is not checked into github
wd = here::here("data-raw/cache")
# England and wales:
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates
# https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt2mid2018lsoasyoaestimatesunformatted.zip
destfile = paste0(wd, "/demographicsUK.zip")
if (!file.exists(destfile)) {
  download.file(
    url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt2mid2018lsoasyoaestimatesunformatted.zip",
    destfile = destfile
  )
}
unzip(destfile, junkpaths = TRUE, exdir = wd, overwrite = TRUE)

# A zipped excel file
# sheets are: Mid-2018 Males A5:CP34758
# sheets are: Mid-2018 Females A5:CP34758
UKdemog = paste0(wd, "/SAPE21DT2-mid-2018-lsoa-syoa-estimates-unformatted.xlsx")

convert = function(demogByLSOA) {
  ageCols = colnames(demogByLSOA)[
    !is.na(as.integer(stringr::str_remove(colnames(demogByLSOA), "\\+")))
  ]

  tmp = demogByLSOA %>%
    dplyr::select(-`All Ages`) %>%
    tidyr::pivot_longer(
      cols = all_of(ageCols),
      names_to = "age",
      values_to = "count"
    ) #, names_ptypes = list(age=integer()))

  tmp = tmp %>%
    dplyr::rename(code = `Area Codes`, name = `Area Names`) %>% #, total=`All Ages`) %>%
    dplyr::mutate(
      age = as.integer(stringr::str_remove(age, "\\+")),
      codeType = "LSOA11"
    )
  return(tmp)
}

demogByLSOA_M <- readxl::read_excel(
  UKdemog,
  sheet = "Mid-2018 Males",
  skip = 4
) %>%
  convert() %>%
  dplyr::mutate(gender = "male")
demogByLSOA_F <- readxl::read_excel(
  UKdemog,
  sheet = "Mid-2018 Females",
  skip = 4
) %>%
  convert() %>%
  dplyr::mutate(gender = "female")

scotDemogM = paste0(wd, "/demographicsScot_M.xlsx")
scotDemogF = paste0(wd, "/demographicsScot_F.xlsx")

# Scotland:
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series#2018
# males - https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/males/sape-2018-males.xlsx
# sheet - Table 1b Males (2018) A6:CR6982
# females - https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/females/sape-2018-females.xlsx
# sheet - Table 1c Females (2018) A6:CR6982

if (!file.exists(scotDemogM)) {
  download.file(
    url = "https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/males/sape-2018-males.xlsx",
    destfile = scotDemogM
  )
}
if (!file.exists(scotDemogF)) {
  download.file(
    url = "https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/females/sape-2018-females.xlsx",
    destfile = scotDemogF
  )
}

convert2 = function(demogBySGDZ) {
  demogBySGDZ = demogBySGDZ %>% dplyr::select(-...4, -...5)
  ageCols = colnames(demogBySGDZ)[
    !is.na(as.integer(stringr::str_remove(colnames(demogBySGDZ), "\\.+")))
  ]
  demogBySGDZ = demogBySGDZ %>%
    tidyr::pivot_longer(
      cols = all_of(ageCols),
      names_to = "age",
      values_to = "count"
    )
  demogBySGDZ = demogBySGDZ %>%
    dplyr::mutate(
      age = as.integer(stringr::str_remove(age, "\\.+")) - 6,
      codeType = "SGDZ11"
    ) %>%
    dplyr::rename(code = DataZone2011Code, name = DataZone2011Name) %>%
    dplyr::select(-CouncilArea2018Name)
  return(demogBySGDZ)
}

demogBySGDZ_M = readxl::read_excel(
  scotDemogM,
  sheet = "Table 1b Males (2018)",
  range = "A6:CR6982"
) %>%
  convert2() %>%
  dplyr::mutate(gender = "male")
demogBySGDZ_F = readxl::read_excel(
  scotDemogF,
  sheet = "Table 1c Females (2018)",
  range = "A6:CR6982"
) %>%
  convert2() %>%
  dplyr::mutate(gender = "female")

demogNI = readr::read_csv(
  "https://www.opendatani.gov.uk/dataset/3333626e-b96e-4b90-82fb-474c6c03b868/resource/64bd8dc4-935f-4bdd-9232-90ff33f24732/download/mid-2018-based-population-projections-for-areas-within-northern-ireland-lgd14.csv"
)
demogNI = demogNI %>%
  dplyr::mutate(gender = stringr::str_to_lower(Gender), codeType = "LGD") %>%
  dplyr::filter(Mid_Year_Ending == 2020) %>%
  dplyr::select(
    code = Geo_Code,
    name = Geo_Name,
    age = Age,
    count = Population_Projection,
    gender,
    codeType
  ) %>%
  dplyr::filter(gender %in% c("male", "female"))

demographics = dplyr::bind_rows(
  demogByLSOA_M,
  demogByLSOA_F,
  demogBySGDZ_M,
  demogBySGDZ_F,
  demogNI
)

uk2019adultpopulation = demographics %>%
  filter(age > 18) %>%
  select(-name) %>%
  inner_join(demographicsMap %>% select(code, name), by = "code") %>%
  group_by(code, name, codeType) %>%
  summarise(population = sum(count)) %>%
  ungroup()
uk2019retiredpopulation = demographics %>%
  filter(age > 65) %>%
  select(-name) %>%
  inner_join(demographicsMap %>% select(code, name), by = "code") %>%
  group_by(code, name, codeType) %>%
  summarise(population = sum(count)) %>%
  ungroup()

usethis::use_data(uk2019adultpopulation, overwrite = TRUE)
usethis::use_data(uk2019retiredpopulation, overwrite = TRUE)

## London shapefile ----

londonShape = arear::getMap("NHSER20") %>% filter(name == "London")
usethis::use_data(londonShape, overwrite = TRUE)

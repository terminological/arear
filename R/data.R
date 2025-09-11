## mapsources ----

#' Locations of UK and international shapefiles relevant to COVID
#'
#' A list of URLs to get maps, and metadata about the maps in the shapefiles and
#' the column labelling.
#'
#' @concept datasets
#'
#' @format A list with:
#' \describe{
#'   \item{source}{the human readable location of the map}
#'   \item{url}{the web location of the downloadable map shapefile}
#'   \item{mapName}{the name of the map contained in the shapefile (which can contain multiple maps)}
#'   \item{codeCol}{the name of the shapefile column containing the code of the area}
#'   \item{nameCol}{the name of the shapefile column containing the name of the area}
#'   \item{altCodeCol}{the name of the shapefile column containing the an alternative code for the area}
#'   \item{simplify}{should the map be simplified when loaded?}
#'   \item{license}{license terms}
#' }
"mapsources"

## ukconnections ----

#' Locations of UK bridge or ferry route start and end points
#'
#' geographical location of bridges / ferries UK connections.
#' picked from https://developers.google.com/maps/documentation/javascript/examples/event-click-latlng#maps_event_click_latlng-html
#'
#' @concept datasets
#'
#' @format A dataframe with:
#' \describe{
#'   \item{name}{the connection}
#'   \item{start.lat}{the latitude of one end}
#'   \item{start.long}{the longditude of one end}
#'   \item{end.lat}{the latitude of the other end}
#'   \item{end.long}{the longditude of the other end}
#' }
"ukconnections"

## surge capacity ----

#' Hospital surge capacity in COVID
#'
#' Locations of UK general medical hospitals in mid march 2020 with estimates of
#' beds available and maximal surge capacity HDU beds. This was manually
#' assembled and curated from various sources in mid march 2020 as the NHS
#' geared up to provide additional capacity to cope with the surge in COVID
#' cases. It is not an up to date picture of NHS capacity. It does not include
#' mental health or community hospitals. The surge capacity seems to have been
#' calculated quite differently in Scotland.
#'
#' @concept datasets
#'
#' @format A sf geometry with:
#' \describe{
#'   \item{nation}{England, Wales, etc...}
#'   \item{hospitalId}{An id for the hospital}
#'   \item{sector}{NHS or independent}
#'   \item{hospitalName}{the hospital name}
#'   \item{pcds}{the UK postcode of the hospital}
#'   \item{trustId}{the NHS trust or local health board of the hospital}
#'   \item{trustName}{the NHS trust or local health board name}
#'   \item{tier1}{indicator of the role of the hospital as an acure provider}
#'   \item{hduBeds}{the number of hdu beds the hospital could have provided at maximum surge in March 2020}
#'   \item{acuteBeds}{the number of acute beds the hospital could have provided at maximum surge in March 2020}
#' }
"surgecapacity"

## london shape ----

#' An outline of the London NHSER region for use as a mask
#'
#' Data available under open government licence:
#' http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#'
#' @concept datasets
#'
#' @format A sf geometry with 1 row:
#' \describe{
#'   \item{codeType}{NHSER}
#'   \item{code}{E4000003 - the NHSER code for London}
#'   \item{name}{London}
#'   \item{altCode}{NA}
#'   \item{area}{the area}
#'   \item{geometry}{the outline}
#' }
"londonShape"

## test demographics ----

#' Mid year 2019 small area estimates for adult population of England, Wales, Scotland, and Northern Ireland
#'
#' Small area single digit estimates are aggregated to include only over 18s,
#' and combining gender
#'
#' Data available under open government licence:
#' http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#'
#' @concept datasets
#'
#' @format A 41740 line data frame:
#' \describe{
#'   \item{code}{the ONS code for the ares}
#'   \item{name}{The name for the area}
#'   \item{codeType}{the type of area (LSOA, DZ or LGD)}
#'   \item{population}{the size of the population}
#' }
#' @source
#' \describe{
#'   \item{England and Wales}{https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates}
#'   \item{Scotland}{https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series#2018}
#'   \item{Northern Ireland}{https://www.opendatani.gov.uk/dataset/3333626e-b96e-4b90-82fb-474c6c03b868/resource/64bd8dc4-935f-4bdd-9232-90ff33f24732/}
#' }
"uk2019adultpopulation"

#' Mid year 2019 small area estimates for adult population of England, Wales, Scotland, and Northern Ireland
#'
#' Small area single digit estimates are aggregated to include only over 65s,
#' and combining gender.
#'
#' Data available under open government licence:
#' http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#'
#' @concept datasets
#'
#' @format A 41730 line data frame:
#' \describe{
#'   \item{code}{the ONS code for the ares}
#'   \item{name}{The name for the area}
#'   \item{codeType}{the type of area (LSOA, DZ or LGD)}
#'   \item{population}{the size of the population}
#' }
#' @source
#' \describe{
#'   \item{England and Wales}{https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates}
#'   \item{Scotland}{https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series#2018}
#'   \item{Northern Ireland}{https://www.opendatani.gov.uk/dataset/3333626e-b96e-4b90-82fb-474c6c03b868/resource/64bd8dc4-935f-4bdd-9232-90ff33f24732/}
#' }
"uk2019retiredpopulation"

## demographics maps ----

#' Small area demographics map
#'
#' Shape file related to the mid year 2019 small area estimates for England,
#' Wales, Scotland, and Northern Ireland. see [uk2019demographics()]
#'
#' There are 10 regions (mostly in Scotland) where the demographics estimates
#' don't align with this map. This is a small number of people.
#'
#' Data available under open government licence:
#' http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#'
#' @concept datasets
#'
#' @source
#' \describe{
#'   \item{England and Wales}{https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bgc}
#'   \item{Scotland}{https://data.gov.uk/dataset/ab9f1f20-3b7f-4efa-9bd2-239acf63b540/data-zone-boundaries-2011}
#'   \item{Northern Ireland}{https://data.gov.uk/dataset/05f72866-b72b-476a-b6f3-57bd4a768674/osni-open-data-largescale-boundaries-local-government-districts-2012}
#' }
#'
#' @return A 41730 line data frame:
#' \describe{
#'   \item{code}{the ONS code for the ares}
#'   \item{name}{The name for the area}
#'   \item{codeType}{the type of area (LSOA, DZ or LGD)}
#'   \item{altCode}{NA}
#' }
#' @export
uk2019demographicsmap = function() {
  .cached(
    {
      rbind(
        arear::getMap("LSOA11"),
        arear::getMap("DZ11"),
        arear::getMap("LGD12")
      ) %>%
        rmapshaper::ms_simplify(keep = 0.1, keep_shapes = TRUE)
    },
    name = "demographics-map"
  )
}

## statistical reporting maps ----

#' COVID reporting map
#'
#' Shape file related to the detailed level for PHE coronavirus statistical
#' reporting in England, Wales, Scotland, and Northern Ireland on the
#' coronavirus.gov.uk tracker. The legacy is the level at which detailed data
#' was reported in the first wave, prior to the API and was aggregated by
#' https://github.com/tomwhite/covid-19-uk-data
#'
#' This map matches the data reported by the PHE coronavirus api when it is
#' downloading lower tier local authority regions e.g. via
#' https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv
#' The API has now been discontinued.
#'
#' There are 2 regions here for which no data is reported in this API - the city
#' of London & the Isles of Scilly.
#'
#' Data available under open government licence:
#' http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#'
#' @concept datasets
#'
#' @param legacy use the legacy version of the map from pre coronavirus api times.
#'
#' @source
#' \describe{
#'   \item{England, Wales, Scotland & NI - LAD19}{https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc}
#'   \item{England & Wales - LAD19}{https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc}
#'   \item{Scotland - CA19}{https://www.spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/1cd57ea6-8d6e-412b-a9dd-d1c89a80ad62}
#'   \item{Northern Ireland  -LGD12}{https://data.gov.uk/dataset/05f72866-b72b-476a-b6f3-57bd4a768674/osni-open-data-largescale-boundaries-local-government-districts-2012}
#' }
#'
#' @return A 382 line data frame (or a 335 line data frame for legacy):
#' \describe{
#'   \item{code}{the ONS code for the ares}
#'   \item{name}{The name for the area}
#'   \item{codeType}{the type of area (LAD19)}
#'   \item{altCode}{NA}
#' }
#' @export
ukcovidmap = function(legacy = FALSE) {
  .cached(
    {
      if (legacy) {
        rbind(
          arear::getMap("LAD19") %>%
            dplyr::filter(code %>% stringr::str_starts("E")),
          #arear::getMap("CA19"),
          arear::getMap("LHB19") %>%
            rmapshaper::ms_simplify(keep = 0.1, keep_shapes = TRUE),
          arear::getMap("LGD12") %>%
            rmapshaper::ms_simplify(keep = 0.1, keep_shapes = TRUE)
        ) %>%
          dplyr::ungroup() %>%
          return()
      } else {
        return(arear::getMap("LAD19"))
      }
    },
    name = "reporting-map",
    hash = legacy
  )
}


## test data ----

#' A list of regular lattice SF polygons used for testing purposes
#'
#' @concept datasets
#'
#' @format A list with:
#' \describe{
#'   \item{grid5x5}{5x5 grid centred on 0,0}
#'   \item{grid11x11}{11x11 grid centred on 0,0}
#'   \item{diamond5x5}{5x5 diagonal grid centred on 0,0}
#'   \item{diamond11x11}{11x11 diagonal grid centred on 0,0}
#'   \item{gridDemand}{an 11x11 grid with a demand parameter}
#'   \item{gridSupply}{a 3 point supply}
#'   \item{gridSupply2}{another 3 point supply with only 2 ids}
#'   \item{gridSupplyViolateConstraint}{a 4 point supply with 2 points in the same grid square}
#'   \item{gridSupplyDegenerate}{a 5 point supply with 2 points in the same grid square amd a different 2 points with the same id}
#' }
"testdata"


## uk demographics ----

#' UK small area single year of age population estimates from 2019
#'
#' These estimates are appropriate for the majority of the pandemic, and are
#' the highest geographical resolution estimates by single year of age that
#' I could find.
#'
#' @concept datasets
#' @return a dataframe with age, gender, codeType, code, name and count
#' @export
uk2019demographics = function() {
  demographics = .cached({
    wd = getOption(
      "arear.cache.dir",
      default = rappdirs::user_cache_dir("arear")
    )
    # England and wales:
    # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates
    # https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt2mid2018lsoasyoaestimatesunformatted.zip
    destfile = paste0(wd, "/demographicsUK.zip")
    if (!file.exists(destfile)) {
      utils::download.file(
        url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt2mid2018lsoasyoaestimatesunformatted.zip",
        destfile = destfile
      )
    }
    utils::unzip(destfile, junkpaths = TRUE, exdir = wd, overwrite = TRUE)

    # A zipped excel file
    # sheets are: Mid-2018 Males A5:CP34758
    # sheets are: Mid-2018 Females A5:CP34758
    UKdemog = paste0(
      wd,
      "/SAPE21DT2-mid-2018-lsoa-syoa-estimates-unformatted.xlsx"
    )

    convert = function(demogByLSOA) {
      suppressMessages(suppressWarnings({
        ageCols = colnames(demogByLSOA)[
          !is.na(as.integer(stringr::str_remove(colnames(demogByLSOA), "\\+")))
        ]

        tmp = demogByLSOA %>%
          dplyr::select(-`All Ages`) %>%
          tidyr::pivot_longer(
            cols = dplyr::all_of(ageCols),
            names_to = "age",
            values_to = "count"
          ) #, names_ptypes = list(age=integer()))

        tmp = tmp %>%
          dplyr::rename(code = `Area Codes`, name = `Area Names`) %>% #, total=`All Ages`) %>%
          dplyr::mutate(
            age = as.integer(stringr::str_remove(age, "\\+")),
            codeType = "LSOA11"
          )
      }))
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
      utils::download.file(
        url = "https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/males/sape-2018-males.xlsx",
        destfile = scotDemogM
      )
    }
    if (!file.exists(scotDemogF)) {
      utils::download.file(
        url = "https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/females/sape-2018-females.xlsx",
        destfile = scotDemogF
      )
    }

    convert2 = function(demogBySGDZ) {
      suppressMessages(suppressWarnings({
        demogBySGDZ = demogBySGDZ %>% dplyr::select(-...4, -...5)
        ageCols = colnames(demogBySGDZ)[
          !is.na(as.integer(stringr::str_remove(colnames(demogBySGDZ), "\\.+")))
        ]
        demogBySGDZ = demogBySGDZ %>%
          tidyr::pivot_longer(
            cols = tidyselect::all_of(ageCols),
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
      }))
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

    niDemog = paste0(wd, "/demographicsNI.xlsx")
    if (!file.exists(niDemog)) {
      utils::download.file(
        url = "https://www.nisra.gov.uk/system/files/statistics/MYE11-21_SYA-2021BASED.xlsx",
        niDemog
      )
    }
    demogNI = readxl::read_excel(niDemog, sheet = "Flat")
    demogNI = demogNI %>%
      dplyr::filter(area == "2. Local Government Districts (LGD2014)") %>%
      dplyr::mutate(
        gender = dplyr::case_when(
          sex == "Females" ~ "female",
          sex == "Males" ~ "male",
          TRUE ~ NA
        ),
        codeType = "LGD"
      ) %>%
      dplyr::filter(year == 2020) %>%
      dplyr::select(
        code = area_code,
        name = area_name,
        age = age,
        count = MYE,
        gender,
        codeType
      ) %>%
      dplyr::filter(gender %in% c("male", "female"))

    dplyr::bind_rows(
      demogByLSOA_M,
      demogByLSOA_F,
      demogBySGDZ_M,
      demogBySGDZ_F,
      demogNI
    )
  })

  return(demographics)
}

## api NHS Trust data ----

#' NHS trust admissions data from the legacy COVID API from 2021
#'
#' @concept datasets
#'
#' @format A 72928 line data frame:
#' \describe{
#'   \item{date}{the ONS code for the ares}
#'   \item{area_name}{The name for the area}
#'   \item{area_type}{the type of area (LSOA, DZ or LGD)}
#'   \item{area_code}{the NHS ODS code}
#'   \item{metric}{hospital cases}
#'   \item{metric_name}{hospital cases}
#'   \item{value}{the count}
#' }
"apiTrusts"

#' Gets maps for which the metadata is known.
#'
#' If a map needs to be downloaded as a shapefile then it is stored temporarily.
#' The location of this download directory can be set as
#' option("arear.download.dir" = "~/.)
#'
#' @param mapId either: a name of a default map
#' @param sources or: a named list of map sources. Each map source entry
#' must be a named list containing the following information:
#'
#' - `url`: the download URL of the map
#' - `mapName`: either `geojson` or for shapes the layer name containing the map.
#'   if not given the first shape file layer will be used.
#' - `simplify` (opt: boolean) do you want the map to be simplified
#' - `source`: (opt) the URL that best represents the source of the map
#' - `license`: (opt) the license of the map
#' - `nameCol`: (opt) the column containing the name
#' - `codeCol`: (opt) the column containing the id code
#' - `altCodeCol`: (opt) alternative code columns
#'
#' see `arear::mapsources` for example
#' @inheritDotParams standardiseMap -sf
#' @inheritDotParams .cached .nocache .stale
#'
#' @return a standard sf map
#' @export
#' @concept io
#'
#' @examples
#' if (interactive()) {
#'   map = getMap("NHSER20")
#'   map %>% dplyr::glimpse()
#' }
getMap = function(mapId = names(sources)[1], sources = arear::mapsources, ...) {
  # prebuilt maps
  if (!getOption("arear.skip_pins", FALSE)) {
    pinnedIds = pins::pin_list(pin_board())
    if (mapId %in% pinnedIds) {
      return(pins::pin_read(pin_board(), mapId))
    }
  }

  if (!(mapId %in% names(sources))) {
    stop("Unknown map: ", mapId)
  }

  loader = sources[[mapId]]

  if (loader$mapName == "geojson") {
    map = downloadGeojson(
      geojsonUrl = loader$url,
      simplify = loader$simplify,
      ...
    )
  } else {
    map = downloadMap(
      zipUrl = loader$url,
      mapName = loader$mapName,
      simplify = isTRUE(loader$simplify),
      ...
    )
  }

  map = map %>%
    standardiseMap(
      codeCol = loader$codeCol,
      nameCol = loader$nameCol,
      altCodeCol = loader$altCodeCol,
      codeType = mapId
    )

  return(
    structure(
      map,
      source = source,
      license = license
    )
  )
}

#' List the standard maps available to download
#'
#' @return a vector of map names
#' @export
#' @concept io
#' @examples
#' # example code
#' listStandardMaps()
listStandardMaps = function() {
  names(arear::mapsources)
}

#' Download a geojson url, optionally simplifies and cache the result
#'
#' @param geojsonUrl the URL of the geojson resource or ESRI feature service layer
#' @param simplify do you want to simplify the map
#' @inheritDotParams featureServerLayerQuery -url
#' @inheritDotParams .cached .nocache .stale
#'
#' @concept io
#' @return the `sf` object for this geoJson
#' @export
#' @examples
#' # The ONS UK country files:
#' if (interactive()) {
#'   svc = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services"
#'   fs = sprintf("%s/%s",svc,"Countries_December_2024_Boundaries_UK_BUC/FeatureServer")
#'   layer = sprintf("%s/%s",fs,"0")
#'   map = downloadGeojson(layer)
#'   map %>% dplyr::glimpse()
#' }
downloadGeojson = function(
  geojsonUrl,
  simplify = FALSE,
  ...
) {
  out = .cached(
    {
      if (.isFeatureServerUrl(geojsonUrl)) {
        map = featureServerLayerQuery(geojsonUrl, ...)
      } else {
        shape = httr::GET(geojsonUrl)
        content = httr::content(shape, type = 'text', encoding = 'UTF-8')
        map = sf::read_sf(content) %>% sf::st_transform(crs = 4326)
      }

      if (simplify) {
        map = suppressWarnings(map %>% rmapshaper::ms_simplify(keep = 0.1))
      }

      map %>% dplyr::ungroup() %>% sf::st_as_sf()
    },
    hash = list(
      geojsonUrl,
      simplify
    ),
    ...
  )
  return(out)
}

#' Download a shape file map
#'
#' This function downloads and caches a zipped shape file map, unpacks it, finds
#' the correct shape file layer optionally simplifies it and converts it to `sf`.
#'
#' @param zipUrl the URL of the zipped shape file
#' @param mapName (optional) the layer name or map name - this is `"xyz"` if a
#'   zip file contains `"xyz.shp"`. Usually there is one `.shp` file in a  zip
#'   file, and by default it will be picked if this is not set.
#' @param simplify do you want to simplify the map?
#' @inheritDotParams .cached .nocache .stale
#'
#' @concept io
#' @return a `sf` object containing the map
#' @export
#'
#' @examples
#' \dontrun{
#' downloadMap(
#'   zipUrl="https://bit.ly/3A9TnR1"
#' )
#' }
downloadMap = function(
  zipUrl,
  mapName = NULL,
  simplify = FALSE,
  ...
) {
  pattern = if (is.null(mapName)) "*.shp" else sprintf("*/%s.shp", mapName)

  if (!is.null(mapName)) {
    id = fs::path_sanitize(mapName)
  } else {
    id = zipUrl %>% fs::path_file() %>% fs::path_ext_remove()
  }

  out = .cached(
    {
      onsZip = .cache_download(zipUrl, .extn = ".zip")
      unzipDir = fs::path(tempdir(), id)
      fs::dir_create(unzipDir)

      paths = utils::unzip(onsZip, exdir = unzipDir, junkpaths = TRUE)
      if (length(paths) < 1) {
        stop("Could not extract files from shapefile zip: ", onsZip)
      }

      # should be using fs::dir_ls with a glob to do an ends with match.
      mapFile = fs::dir_ls(
        fs::path_abs(unzipDir),
        recurse = TRUE,
        glob = "*.shp"
      )
      if (length(mapFile) == 0) {
        stop("No shape file found in zip")
      }

      if (length(mapFile) > 1) {
        mapFile = fs::dir_ls(
          fs::path_abs(unzipDir),
          recurse = TRUE,
          glob = pattern
        )
        if (length(mapFile) != 1) {
          stop(
            "No uniquely matching shape file for `",
            pattern,
            "` has been found in zip: ",
            onsZip
          )
        }
      }

      map = sf::st_read(mapFile) %>% sf::st_transform(crs = 4326)

      if (simplify) {
        map = suppressWarnings(map %>% rmapshaper::ms_simplify(keep = 0.1))
      }

      map %>% dplyr::ungroup() %>% sf::st_as_sf()
    },
    name = id,
    hash = list(
      zipUrl,
      mapName,
      simplify
    ),
    ...
  )
  return(out)
}


#' Standardise maps
#'
#' This function renames a `sf` with a minimal set of attributes with consistent
#' naming with `code`, `name` and `codeType` columns and an optional `altCode`
#' column. Renames all columns to be lower case, and makes sure the `area` column
#' is calculated. It also loses any `Z` or `M` layers.
#'
#' @param sf a non standard map
#' @param codeCol the name of the column containing the id or code
#' @param nameCol the name of the column containing the label (optional -
#'   defaults to the same as codeCol)
#' @param altCodeCol an optional column name containing another code type
#' @param codeType the "type" of the code - optional. defaults to NA
#' @param ... not used.
#'
#' @concept io
#' @return a standardised map with exactly the following columns: `code`, `codeType`,
#'   `name`, `altCode`, `geometry` and `area`
#' @export
#' @examples
#' # example code
#' if (interactive()) {
#'   svc = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services"
#'   fs = sprintf("%s/%s",svc,"Countries_December_2024_Boundaries_UK_BUC/FeatureServer")
#'   layer = sprintf("%s/%s",fs,"0")
#'   map = downloadGeojson(layer)
#'   map %>% dplyr::glimpse()
#'   map2 = map %>% standardiseMap(codeCol = CTRY24CD, nameCol = CTRY24NM)
#'   map2 %>% dplyr::glimpse()
#' }
standardiseMap = function(
  sf,
  codeCol = "code",
  nameCol = "name",
  altCodeCol = NULL,
  codeType = NA_character_,
  ...
) {
  if (
    missing(codeCol) &&
      missing(nameCol) &&
      missing(altCodeCol) &&
      missing(codeType)
  ) {
    if (!all(c("code", "name") %in% colnames(sf))) {
      message(
        "Map columns not standardised: " + paste0(colnames(sf), collapse = ", ")
      )
    }
  } else {
    codeCol = rlang::ensym(codeCol)
    if (is.na(codeType)) {
      codeType = tolower(rlang::as_label(codeCol))
    }
    nameCol = tryCatch(rlang::ensym(nameCol), error = function(e) NULL)
    altCodeCol = tryCatch(rlang::ensym(altCodeCol), error = function(e) NULL)

    if (!as.character(codeCol) %in% colnames(sf)) {
      stop(
        "the codeCol column is not present in sf should be one of: ",
        paste(colnames(sf), collapse = ",")
      )
    }
    sf = sf %>%
      dplyr::mutate(tmp_code = as.character(!!codeCol))
    if (!identical(nameCol, NULL)) {
      if (!as.character(nameCol) %in% colnames(sf)) {
        stop(
          "the nameCol column is not present in sf should be one of: ",
          paste(colnames(sf), collapse = ",")
        )
      }
      sf = sf %>% dplyr::mutate(tmp_name = as.character(!!nameCol))
      sf = sf %>% dplyr::select(-!!nameCol)
    } else {
      sf = sf %>% dplyr::mutate(tmp_name = as.character(!!codeCol))
    }
    sf = sf %>%
      dplyr::select(-!!codeCol) %>%
      dplyr::rename(code = tmp_code, name = tmp_name)

    if (!identical(altCodeCol, NULL)) {
      if (!as.character(altCodeCol) %in% colnames(sf)) {
        stop(
          "the altCodeCol column is not present in sf should be one of: ",
          paste(colnames(sf), collapse = ",")
        )
      }
      sf = sf %>%
        dplyr::mutate(tmpAltCode = !!altCodeCol) %>%
        dplyr::select(-!!altCodeCol) %>%
        dplyr::rename(altCode = tmpAltCode) %>%
        dplyr::mutate(altCode = as.character(altCode))
    } else {
      sf = sf %>% dplyr::mutate(altCode = as.character(NA))
    }
    sf = sf %>%
      dplyr::mutate(codeType = codeType) %>%
      dplyr::select(codeType, code, name, altCode)
  }

  .forceGeos({
    sf$area = sf %>% sf::st_area() %>% as.numeric()
    sf %>% sf::st_zm()
  })

  sf = sf %>% dplyr::rename_with(.cols = -c(codeType, altCode), .fn = tolower)
  return(sf)
}

#' Save a `sf` as a shapefile to disk
#'
#' @param shape the sf shape
#' @param mapId a mapId - will become the zip filename, and the filename of
#'   the zipped `.shp` file
#' @param dir the directory (defaults to current working directory)
#' @param overwrite the save function will not write over existing files
#'   unless this is set to true
#'
#' @concept io
#' @return a the filename of the zipped shapefile
#' @export
#' @examples
#' if (interactive()) {
#'   svc = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services"
#'   fs = sprintf("%s/%s",svc,"Countries_December_2024_Boundaries_UK_BUC/FeatureServer")
#'   layer = sprintf("%s/%s",fs,"0")
#'   map = downloadGeojson(layer)
#'   map %>% dplyr::glimpse()
#'   map2 = map %>% standardiseMap(codeCol = CTRY24CD, nameCol = CTRY24NM)
#'   map2 %>% dplyr::glimpse()
#'   saveShapefile(map2, "ctry24", dir = tempdir())
#' }
saveShapefile = function(shape, mapId, dir = getwd(), overwrite = FALSE) {
  if (!dir %>% stringr::str_ends("/")) {
    dir = paste0(dir, "/")
  }
  zipDir = paste0(dir, mapId)
  if (dir.exists(zipDir) & !overwrite & length(list.files(zipDir)) > 0) {
    stop(
      "Directory ",
      zipDir,
      " exists and is not empty. use overwrite=TRUE to force update"
    )
  }
  if (dir.exists(zipDir)) {
    unlink(zipDir, recursive = TRUE)
  }
  dir.create(zipDir, recursive = TRUE)
  suppressWarnings(sf::st_write(
    shape,
    paste0(zipDir, "/", mapId, ".shp"),
    driver = "ESRI Shapefile"
  ))
  wd = getwd()
  setwd(dir) # the parent directory
  utils::zip(zipfile = paste0(zipDir, ".zip"), files = mapId) #zip the directory
  setwd(wd)
  unlink(zipDir, recursive = TRUE)
  return(paste0(zipDir, ".zip"))
}

pin_board = .memoise(
  ~ pins::board_url("https://terminological.github.io/arear/maps/")
)

#' Gets maps for which the metadata is known.
#'
#' If a map needs to be downloaded as a shapefile then it is stored temporarily. The location of this download directory can be set as option("arear.download.dir" = "~/.)
#'
#' @param mapId - a name of a map
#' @param sources - a list of map sources - see `getOption("arear.mapsources",arear::mapsources)``
#' @param codeType - defaults to mapId, the codeType of the map
#' @param ... - passed to .cache, param `nocache=TRUE` to disable caching
#'
#' @return a standard sf map
#' @export
#'
#' @examples
#' \dontrun{
#' map = getMap("NHSER20")
#' }
getMap = function(mapId, sources = .loadSources(...), codeType = mapId, ...) {
  pinnedIds = pins::pin_list(pin_board())
  if (mapId %in% pinnedIds) {
    return(pins::pin_read(pin_board(), mapId))
  }

  if (!(mapId %in% names(sources))) {
    stop("Unknown map: ", mapId)
  }

  loader = sources[[mapId]]
  codeCol = as.symbol(tolower(loader$codeCol))
  nameCol = tryCatch(as.symbol(tolower(loader$nameCol)), error = function(e) {
    NULL
  })
  altCodeCol = tryCatch(
    as.symbol(tolower(loader$altCodeCol)),
    error = function(e) NULL
  )
  if (loader$mapName == "geojson") {
    map = downloadGeojson(
      geojsonUrl = loader$url,
      codeCol = !!codeCol,
      nameCol = !!nameCol,
      altCodeCol = !!altCodeCol,
      codeType = codeType,
      id = mapId,
      license = loader$license,
      simplify = loader$simplify,
      ...
    )
  } else {
    map = downloadMap(
      zipUrl = loader$url,
      mapName = loader$mapName,
      codeCol = !!codeCol,
      nameCol = !!nameCol,
      altCodeCol = !!altCodeCol,
      codeType = codeType,
      id = mapId,
      license = loader$license,
      simplify = loader$simplify,
      ...
    )
  }
  return(map)
}

.loadSources = function(nocache = FALSE, ...) {
  mapsources = getOption("arear.mapsources", arear::mapsources)
  cache = getOption(
    "arear.cache.dir",
    default = rappdirs::user_cache_dir("arear")
  )
  cachedVersion = fs::path(cache, "sources.json")
  if (nocache || !fs::file_exists(cachedVersion)) {
    version = mapsources
    if (fs::file_exists(cachedVersion)) unlink(cachedVersion)
  } else {
    version = c(
      mapsources,
      jsonlite::read_json(cachedVersion, simplifyVector = TRUE)
    )
  }
  return(version)
}

.addSource = function(
  name,
  source = url,
  url,
  codeCol,
  nameCol,
  altCodeCol,
  simplify = FALSE,
  license = "unknown",
  ...
) {
  cache = getOption(
    "arear.cache.dir",
    default = rappdirs::user_cache_dir("arear")
  )
  cachedVersion = fs::path(cache, "sources.json")

  if (!(name %in% names(arear::mapsources))) {
    if (fs::file_exists(cachedVersion)) {
      source = jsonlite::read_json(cachedVersion, simplifyVector = TRUE)
    } else {
      source = list()
    }

    source[[name]] = list(
      name = name,
      source = source,
      url = url,
      codeCol = codeCol,
      nameCol = nameCol,
      altCodeCol = altCodeCol,
      simplify = simplify,
      license = license
    )

    jsonlite::write_json(source, cachedVersion)
  }
}

#' List the standard maps available to download
#'
#' @return a vector of map names
#' @export
listStandardMaps = function() {
  names(arear::mapsources)
}

#' Download a geojson url, standardise it and cache the result
#'
#' @param geojsonUrl the URL of the geojson resource
#' @param codeCol - the name of the column containing the id or code
#' @param nameCol - the name of the column containing the label (optional - defaults to the same as codeCol)
#' @param altCodeCol - an optional column name containing another code type
#' @param codeType - the "type" of the code - optional. defaults to NA
#' @param simplify - do you want to simplify the map
#' @param id - an id for the map that can be used to retrieve it in the future (through getMap()).
#' @param license - an optional license string
#' @param ... - passed to .cache, param nocache=TRUE to disable caching
#'
#' @return the sf object for this geoJson
#' @export
downloadGeojson = function(
  geojsonUrl,
  codeCol = "code",
  nameCol = "name",
  altCodeCol = NULL,
  codeType = NA_character_,
  simplify = FALSE,
  id,
  license = "unknown",
  ...
) {
  codeCol = rlang::ensym(codeCol)
  nameCol = tryCatch(rlang::ensym(nameCol), error = function(e) NULL)
  altCodeCol = tryCatch(rlang::ensym(altCodeCol), error = function(e) NULL)

  .cached(
    {
      complete = FALSE
      row = 0
      map = NULL
      while (!complete) {
        if (stringr::str_ends(geojsonUrl, "query")) {
          geojsonUrl2 = sprintf(
            "%s?outSR=4326&outFields=*&f=pgeojson&where=1%%3D1&resultOffset=%d",
            geojsonUrl,
            row
          )
        } else {
          geojsonUrl2 = geojsonUrl
        }

        content = .cached(
          {
            shape = httr::GET(geojsonUrl2)
            httr::content(shape, type = 'text', encoding = 'UTF-8')
          },
          hash = list(geojsonUrl2)
        )

        tmp = jsonlite::fromJSON(content)

        map = dplyr::bind_rows(
          map,
          sf::read_sf(content) %>% sf::st_transform(crs = 4326)
        )
        complete = TRUE

        if (
          isTRUE(tmp$exceededTransferLimit) ||
            isTRUE(tmp$properties$exceededTransferLimit)
        ) {
          row = nrow(map)
          message("rows: ", row)
          complete = FALSE
        }
      }

      map = standardiseMap(map, !!codeCol, !!nameCol, !!altCodeCol, codeType)
      if (simplify) {
        map = suppressWarnings(map %>% rmapshaper::ms_simplify(keep = 0.1))
      }

      .addSource(
        name = id,
        url = geojsonUrl,
        codeCol = rlang::as_label(codeCol),
        nameCol = rlang::as_label(nameCol),
        altCodeCol = rlang::as_label(altCodeCol),
        simplify = simplify,
        license = license
      )

      map %>% dplyr::ungroup() %>% sf::st_as_sf()
    },
    name = id,
    hash = list(
      geojsonUrl,
      codeCol,
      nameCol,
      altCodeCol,
      codeType,
      simplify,
      license
    ),
    ...
  )
}

#' Download a map, unpack it, and rename columns.
#'
#' to standard code, name, altCode and codeType columns
#'
#' @param zipUrl - the URL of the zipped shapefile
#' @param mapName - the layer name or map name - this is the "xyz" of a zip file containing "xyz.shp". If you are getting multiple layers it is OK to repeatedly call this within the same session as the download is stored, see wd option.
#' @param codeCol - the name of the column containing the id or code
#' @param nameCol - the name of the column containing the label (optional - defaults to the same as codeCol)
#' @param altCodeCol - an optional column name containing another code type
#' @param codeType - the "type" of the code - optional. defaults to NA
#' @param simplify - do you want to simplify the map
#' @param wd - an optional working directory (defaults to `getOption("arear.download.dir", tempdir())`)
#' @param id - an optional id for the map that can be used to retrieve it later (through getMap()) - defaults to either the mapName or if not present the name of the zip file.
#' @param license - an optional license string
#' @param ... - passed to .cache, param nocache=TRUE to disable caching
#'
#' @return a sf object containing the map
#' @export
#'
#' @examples
#' \dontrun{
#' downloadMap(
#'   zipUrl="https://bit.ly/3A9TnR1",
#'   mapName="NHS_England_Regions__April_2020__Boundaries_EN_BGC",
#'   codeCol="nhser20cd",
#'   nameCol="nhser20nm"
#' )
#' }
downloadMap = function(
  zipUrl,
  mapName = NULL,
  codeCol = "code",
  nameCol = "name",
  altCodeCol = NULL,
  codeType = NA_character_,
  simplify = FALSE,
  wd = getOption("arear.download.dir", tempdir()),
  id = NULL,
  license = "unknown",
  ...
) {
  codeCol = rlang::ensym(codeCol)
  nameCol = tryCatch(rlang::ensym(nameCol), error = function(e) NULL)
  altCodeCol = tryCatch(rlang::ensym(altCodeCol), error = function(e) NULL)

  if (!stringr::str_ends(wd, "/")) {
    wd = paste0(wd, "/")
  }
  try(fs::dir_create(wd))

  pattern = if (is.null(mapName)) "*.shp" else sprintf("*/%s.shp", mapName)

  if (is.null(id)) {
    if (!is.null(mapName)) {
      id = fs::path_sanitize(mapName)
    } else {
      id = zipUrl %>% fs::path_file() %>% fs::path_ext_remove()
    }
  }

  .cached(
    {
      onsZip = fs::path(wd, id) %>% fs::path_ext_set("zip")
      unzipDir = fs::path(wd, id)
      if (!file.exists(onsZip)) {
        status = utils::download.file(zipUrl, destfile = onsZip)
        if (status != 0) stop("Problem downloading map: ", zipUrl)
      }
      suppressWarnings(dir.create(unzipDir, recursive = TRUE))

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
            "No uniquely matching shape file for ",
            pattern,
            " has been found in zip: ",
            onsZip
          )
        }
      }

      map = sf::st_read(mapFile) %>% sf::st_transform(crs = 4326)

      map = standardiseMap(map, !!codeCol, !!nameCol, !!altCodeCol, codeType)
      if (simplify) {
        map = suppressWarnings(map %>% rmapshaper::ms_simplify(keep = 0.1))
      }

      .addSource(
        name = id,
        url = zipUrl,
        codeCol = rlang::as_label(codeCol),
        nameCol = rlang::as_label(nameCol),
        altCodeCol = rlang::as_label(altCodeCol),
        simplify = simplify,
        license = license
      )

      map %>% dplyr::ungroup() %>% sf::st_as_sf()
    },
    name = id,
    hash = list(
      zipUrl,
      mapName,
      codeCol,
      nameCol,
      altCodeCol,
      codeType,
      simplify,
      license
    ),
    ...
  )
}


#' Standardise maps to a minimal set of attributes with consistent naming with code, name and codeType columns and an optional altCode column
#'
#' @param sf - a non standard map
#' @param codeCol - a column name containing the unique code or id for the map
#' @param nameCol - the name column
#' @param altCodeCol - an alternative code column
#' @param codeType - a fixed value for the codeType
#'
#' @return a standardised map
#' @export
standardiseMap = function(sf, codeCol, nameCol, altCodeCol, codeType) {
  codeCol = rlang::ensym(codeCol)
  nameCol = tryCatch(rlang::ensym(nameCol), error = function(e) NULL)
  altCodeCol = tryCatch(rlang::ensym(altCodeCol), error = function(e) NULL)
  sf = sf %>%
    dplyr::rename_with(.fn = tolower)

  .forceGeos({
    #TODO: catch missing columns and throw helpful error
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

    sf$area = sf %>% sf::st_area() %>% as.numeric()
    return(sf %>% sf::st_zm())
  })
}

#' save a shapefile to disk in the current working directory
#'
#' @param shape - the sf shape
#' @param mapId - a mapId - will become the zip filename, and the filename of the zipped .shp file
#' @param dir - the directory (defaults to current working directory)
#' @param overwrite - the save function will not write over existing files unless this is set to true
#'
#' @return a the filename of the zipped shapefile
#' @export
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

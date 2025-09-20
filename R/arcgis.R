.isFeatureServerUrl = function(url) {
  return(grepl("^.*/(Feature|Map)Server", url))
}

.featureServerUrl = function(url) {
  out = gsub("^(.*/(Feature|Map)Server).*$", "\\1", url)
  if (!grepl("^.*/(Feature|Map)Server$", out)) {
    stop("Feature server could not be identified from url: ", url)
  }
  return(out)
}

# looks up the first feature service layer
# url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Countries_December_2019_GCB_GB_2022/FeatureServer"
# grep("^(.*/(Feature|Map)Server)",url1,value=TRUE)
# .defaultLayerUrl(url)
.defaultLayerUrl = function(url) {
  type = NULL
  if (!grepl("/(Feature|Map)Server+$", url)) {
    url = .featureServerUrl(url)
  }
  fs_info = featureServerInfo(url)
  # get the feature server URL
  fs_info = fs_info %>%
    dplyr::filter(
      type == "Feature Layer"
    ) %>%
    dplyr::filter(
      dplyr::row_number() == 1
    )

  id = fs_info$id[[1]]
  return(sprintf("%s/%d", url, id))
}

# truncates URL to layer. If it doesn't match expected pattern then
# looks to get a default layer.
.layerUrl = function(url) {
  out = gsub("^(.*/(Feature|Map)Server/[0-9]+).*$", "\\1", url)
  if (!grepl("^.*/(Feature|Map)Server/[0-9]+$", out)) {
    url = .defaultLayerUrl(url)
  }
  return(url)
}

#' Get feature service information from an arcgis rest service
#'
#' @param url the base rest service url
#' @inheritDotParams .cache_download
#'
#' @returns a dataframe of available feature and map services
#' @export
#'
#' @concept arcgis
#'
#' @examples
#' url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services"
#' arcgisServiceInfo(url)
arcgisServiceInfo = function(url, ...) {
  url = gsub("^(.*)/[^/]*/(Feature|Map)Server.*$", "\\1", url)
  url = strsplit(url, "?", fixed = TRUE)[[1]]
  if (!is.na(url[2])) {
    comps = strsplit(url[2], "&", fixed = TRUE)[[1]]
    comps = unique(c(comps, "f=json"))
    gets = paste0(comps, collapse = "&")
  } else {
    gets = "f=json"
  }
  url = sprintf("%s?%s", url[1], gets)
  fs_info = .cache_download(url, ...)
  tmp = jsonlite::fromJSON(fs_info)
  return(dplyr::as_tibble(tmp$services))
}

#' Get layer information from a feature service
#'
#' @param url the feature service url
#' @inheritDotParams .cache_download
#'
#' @returns a dataframe of layers
#'
#' @concept arcgis
#' @export
#'
#' @examples
#' svc = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services"
#' fs = sprintf("%s/%s",svc,"Countries_December_2024_Boundaries_UK_BUC/FeatureServer")
#' url = sprintf("%s/%s",fs,"0")
#' featureServerInfo(fs)
featureServerInfo = function(url, ...) {
  url = .featureServerUrl(url)
  url = sprintf("%s?%s", url, "f=json")
  fs_info = .cache_download(url, ...)
  tmp = jsonlite::fromJSON(fs_info)
  return(dplyr::as_tibble(tmp$layers))
}

#' Get field information from a feature service layer.
#'
#' @param url the feature service layer url. If no layer is specified the first
#'   one is selected.
#' @inheritDotParams .cache_download
#'
#' @returns a dataframe of field names for the selected layer
#' @export
#'
#' @concept arcgis
#'
#' @examples
#' svc = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services"
#' fs = sprintf("%s/%s",svc,"Countries_December_2024_Boundaries_UK_BUC/FeatureServer")
#' layer = sprintf("%s/%s",fs,"0")
#' featureServerLayerInfo(layer)
featureServerLayerInfo = function(url, ...) {
  url = .layerUrl(url)
  url = sprintf("%s?%s", url, "f=json")
  fs_layer_info = .cache_download(url, ...)
  tmp = jsonlite::fromJSON(fs_layer_info)
  return(dplyr::as_tibble(tmp$fields))
}


#' Execute and cache a query on a Feature Server
#'
#' Allow basic filtering queries only at the moment. Will cache the results and
#' handles multipart transfers.
#'
#' @param url the URL of the feature service layer (if layer is not known the
#'   first feature layer will be used)
#' @param where an SQL query (defaults to whole map)
#' @param select the columns to return (see featureServerLayerInfo())
#' @inheritDotParams .cache_post
#' @param limit maximum rows to return
#' @param crs the coordinate reference system
#' @param queryParams additional query parameters passed to the request to the
#'   feature server
#'
#' @returns a `sf` of the query result
#' @export
#' @concept arcgis
#'
#' @examples
#' svc = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services"
#' fs = sprintf("%s/%s",svc,"Countries_December_2024_Boundaries_UK_BUC/FeatureServer")
#' layer = sprintf("%s/%s",fs,"0")
#'
#' # default all 4 nations:
#' sf = featureServerLayerQuery(layer)
#' cat(sf$CTRY24CD)
#'
#' # england only
#' sf2 = featureServerLayerQuery(layer, where="CTRY24CD LIKE 'E%'")
#' cat(sf2$CTRY24CD)
#'
#' if (interactive()) {
#'   # Download the first 4000 LSOAs. There are lots of LSOAs and this
#'   # query has to be paginated. This also shows the limit feature
#'   # and the verbose output of the cache.
#'   lsoa = sprintf("%s/%s",svc,"LSOA_DEC_2021_EW_NC_v3/FeatureServer/0")
#'   withr::with_options(list(cache.verbose=TRUE), {
#'     sf_lsoa = featureServerLayerQuery(lsoa, limit = 4000)
#'   })
#'
#'   cat(nrow(sf_lsoa))
#' }
featureServerLayerQuery = function(
  url,
  where = "1 = 1",
  select = "*",
  ...,
  limit = NULL,
  crs = 4326,
  queryParams = list()
) {
  if (!.isFeatureServerUrl(url)) {
    stop("not a feature server URL: ", url)
  }

  url = sprintf("%s/query", .layerUrl(url))
  body = queryParams
  body = body[names(body) != ""]
  body$where = where
  body$f = "pgeojson"
  body$outFields = select
  body$outSR = crs

  row = 0
  complete = FALSE
  map = NULL
  while (!complete) {
    body$resultOffset = row

    if (!is.null(limit)) {
      body$resultRecordCount = limit - row
    }

    content = .cache_post(
      url,
      body = body,
      as = "text",
      ...
    )

    tmp = jsonlite::fromJSON(content)

    map = dplyr::bind_rows(
      map,
      sf::read_sf(content) %>% sf::st_transform(crs = crs)
    )

    if (
      isTRUE(tmp$exceededTransferLimit) ||
        isTRUE(tmp$properties$exceededTransferLimit)
    ) {
      row = nrow(map)
      message("rows: ", row)
      complete = (row >= limit)
    } else {
      complete = TRUE
    }
  }

  return(map)
}

# https://www.doogal.co.uk/files/postcodes.zip

# https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/ONSPD_LATEST_UK/FeatureServer/1/query?
# where=1%3D1&objectIds=&geometry=&geometryType=esriGeometryEnvelope&
# inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&
# units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=PCDS&returnGeometry=true&
# featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&
# geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&
# returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&
# returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&
# cacheHint=false&collation=&orderByFields=&groupByFieldsForStatistics=&
# outStatistics=&having=&resultOffset=&resultRecordCount=1&returnZ=false&returnM=false&returnTrueCurves=false&returnExceededLimitFeatures=true&
# quantizationParameters=&sqlFormat=none&f=pjson&token=

# https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/ONSPD_LATEST_UK/FeatureServer/1/query?where=PCDS%3D%22TW13DG%22&outFields=PCDS&f=json
# objectIds=
#   geometry=
#   geometryType=esriGeometryEnvelope
# inSR=&
#   spatialRel=esriSpatialRelIntersects&
#   resultType=none&
#   distance=0.0&
#   units=esriSRUnit_Meter&
#   relationParam=&
#   returnGeodetic=false&
#   &returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&collation=&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnTrueCurves=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=html&token=

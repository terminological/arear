
#' Gets maps for which the metadata is known
#'
#' @param mapId - a name of a map
#' @param sources - a list of map sources - see arear::mapsources
#' @param codeType - defaults to mapId, the codeType of the map
#' @param wd - optional working directory
#'
#' @return a standard sf map
#' @export
#'
#' @examples
#' \dontrun{
#' map = getMap("NHSER20")
#' }
getMap = function(mapId, sources = arear::mapsources, codeType = mapId, wd = getOption("arear.cache.dir",default=tempdir())) {
  if (!(mapId %in% names(sources))) stop("Unknown map: ",mapId)
  loader = sources[[mapId]]
  codeCol = as.symbol(loader$codeCol)
  nameCol = tryCatch(as.symbol(loader$nameCol), error = function(e) NULL)
  altCodeCol = tryCatch(as.symbol(loader$altCodeCol), error = function(e) NULL)
  map = downloadMap(zipUrl = loader$url, mapName = loader$mapName, codeCol = !!codeCol, nameCol = !!nameCol, altCodeCol = !!altCodeCol, codeType=codeType, id=mapId, wd = wd)
  return(map)
}


#' List the standard maps available to download
#'
#' @return a vector of map names
#' @export
listStandardMaps = function() {
  names(arear::mapsources)
}

#' Download a map, unpack it, and rename columns
#'
#' to standard code, name, altCode and codeType columns
#'
#' @param zipUrl - the URL of the zipped shapefile
#' @param mapName - the layer name or map name - this is the "xyz" of a zip file containing "xyz.shp"
#' @param codeCol - the name of the column containing the id or code
#' @param nameCol - the name of the column containing the label (optional - defaults to the same as codeCol)
#' @param altCodeCol - an optional column name containing another code type
#' @param codeType - the "type" of the code - optional. defaults to NA
#' @param simplify - do you want to simplify the map
#' @param wd - an optional working directory (defaults to tempdir)
#' @param id - an optional id for the map - defaults to either the mapName or if not present the name of the zip file.
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
downloadMap = function(zipUrl, mapName=NULL, codeCol="code", nameCol="name", altCodeCol=NULL, codeType=NA_character_, simplify=FALSE, wd = tempdir(), id=NULL) {

  codeCol = rlang::ensym(codeCol)
  nameCol = tryCatch(rlang::ensym(nameCol), error = function(e) NULL)
  altCodeCol = tryCatch(rlang::ensym(altCodeCol), error = function(e) NULL)


  pattern = paste0(ifelse(is.null(mapName),"",mapName),"\\.shp$")

  if(is.null(id)) {
    if(!is.null(mapName)) {
      id = fs::path_sanitize(mapName)
    } else {
      id = zipUrl %>% fs::path_file() %>% fs::path_ext_remove()
    }
  }

  .cached({

    onsZip = paste0(wd,"/",id,".zip")
    unzipDir = paste0(wd,"/",id)
    if(!file.exists(onsZip)) {
      status = utils::download.file(zipUrl, destfile = onsZip)
      if (status != 0) stop("Problem downloading map: ",zipUrl)
    }
    suppressWarnings(dir.create(unzipDir,recursive = TRUE))

    paths = utils::unzip(onsZip, exdir=unzipDir, junkpaths = TRUE)
    if(length(paths) < 1) stop("Could not extract files from shapefile zip: ",onsZip)

    mapFile = paste0(unzipDir,"/",list.files(unzipDir,recursive = TRUE,pattern = pattern))
    if(length(mapFile)!=1) stop("More than one matching map has been found: ",mapFile)


    map = sf::st_read(mapFile) %>% sf::st_transform(crs=4326)

    map = standardiseMap(map, !!codeCol, !!nameCol, !!altCodeCol, codeType)
    if(simplify) map = suppressWarnings(map %>% rmapshaper::ms_simplify(keep=0.1))

    map %>% dplyr::ungroup() %>% sf::st_as_sf()

  }, name=id, hash=list(zipUrl,mapName,codeCol,nameCol,altCodeCol,codeType,simplify))

}


#' Standardise maps to a minimal set of attributes with consistent naming with code, name and codeType columns and an optional altCode column
#'
#' @param sf - a non standard map
#' @param codeCol - a column name containing the unique code or id for the map
#' @param nameCol - the name column
#' @param altCodeCol - an alternative code column
#' @param codeType - a fixed vlue for the codeType
#'
#' @return a standardised map
#' @export
standardiseMap = function(sf, codeCol, nameCol, altCodeCol, codeType) {
  codeCol = rlang::ensym(codeCol)
  nameCol = tryCatch(rlang::ensym(nameCol), error = function(e) NULL)
  altCodeCol = tryCatch(rlang::ensym(altCodeCol), error = function(e) NULL)
  sf = sf %>% dplyr::mutate(tmp_code = as.character(!!codeCol))

  #TODO: catch missing columns and throw helpful error
  if(!as.character(codeCol) %in% colnames(sf)) stop("the codeCol column is not present in sf should be one of: ",paste(colnames(sf),collapse = ","))

  if(!identical(nameCol,NULL)) {
    if(!as.character(nameCol) %in% colnames(sf)) stop("the nameCol column is not present in sf should be one of: ",paste(colnames(sf),collapse = ","))
    sf = sf %>% dplyr::mutate(tmp_name = as.character(!!nameCol))
    sf = sf %>% dplyr::select(-!!nameCol)
  } else {
    sf = sf %>% dplyr::mutate(tmp_name = as.character(!!codeCol))
  }
  sf = sf %>% dplyr::select(-!!codeCol) %>% dplyr::rename(code = tmp_code,name = tmp_name)

  if(!identical(altCodeCol,NULL)) {
    if(!as.character(altCodeCol) %in% colnames(sf)) stop("the altCodeCol column is not present in sf should be one of: ",paste(colnames(sf),collapse = ","))
    sf = sf %>% dplyr::rename(altCode = !!altCodeCol) %>% dplyr::mutate(altCode = as.character(altCode))
  } else {
    sf = sf %>% dplyr::mutate(altCode = as.character(NA))
  }
  sf = sf %>% dplyr::mutate(codeType = codeType) %>% dplyr::select(codeType, code, name, altCode)
  sf$area = sf %>% sf::st_area() %>% as.numeric()
  return(sf %>% sf::st_zm())
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
saveShapefile = function(shape, mapId, dir=getwd(), overwrite=FALSE) {
  if (!dir %>% stringr::str_ends("/")) dir = paste0(dir,"/")
  zipDir = paste0(dir,mapId)
  if (dir.exists(zipDir) & !overwrite & length(list.files(zipDir))>0) stop("Directory ",zipDir," exists and is not empty. use overwrite=TRUE to force update")
  if (dir.exists(zipDir)) unlink(zipDir, recursive=TRUE)
  dir.create(zipDir, recursive = TRUE)
  suppressWarnings(sf::st_write(shape, paste0(zipDir,"/",mapId, ".shp"), driver="ESRI Shapefile"))
  wd = getwd()
  setwd(dir) # the parent directory
  utils::zip(zipfile = paste0(zipDir,".zip"),files=mapId) #zip the directory
  setwd(wd)
  unlink(zipDir, recursive=TRUE)
  return(paste0(zipDir,".zip"))
}

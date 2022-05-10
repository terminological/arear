#
.md5obj = function(obj) {
  as.character(digest::digest(obj, algo="md5"))
}

#.arear.cache <- new.env(parent=emptyenv())

# executed expr and saves the output as an RDS file indexed by code in expr and the hash variable (which should contain any varying inputs)
.cached = function (
  expr = {},
  hash = NULL,
  name = "cache",
  nocache = FALSE,
  cache = getOption("arear.cache.dir", default=rappdirs::user_cache_dir("arear")),
  stale = getOption("arear.cache.stale", default=Inf),
  ...)
{

  code = deparse(substitute(expr))
  md5code = .md5obj(code)

  if(!stringr::str_ends(cache,"/"))
    cache = paste0(cache,"/")

  dir.create(cache, recursive = TRUE, showWarnings = FALSE)

  md5params = NULL
  if (!is.null(hash))
    md5params = .md5obj(hash)

  path = paste0(cache,paste(name,md5code,md5params,sep = "-"),".rda")

  if (nocache) unlink(path)
  if (file.exists(path)) {
    mtime = as.Date(file.info(path)$mtime)
    if (mtime < Sys.Date()-stale+1) unlink(path)
  }

  if (file.exists(path)) {
    message("using cached item: ",path)
    readRDS(path)
    #assign(path, obj, envir=.arear.cache)
  } else {
    message("caching item: ",path)
    obj = expr
    saveRDS(obj, path)
    #assign(path, obj, envir=.arear.cache)
    obj
  }
}


#' Clear data from the passthrough cache for complex or long running operations
#'
#' @param cache the location of the cache as a directory. May get its value from options("arear.cache.dir") or the default value of rappdirs::user_cache_dir("arear")
#' @param interactive if FALSE will delete the files without warning. Defaults to TRUE
#'
#' @return nothing
#' @export
cache_clear = function (
  cache = getOption("arear.cache.dir", default=rappdirs::user_cache_dir("arear")),
  interactive = TRUE
) {
  paths = NULL # for r cmd check
  if (!fs::dir_exists(cache)) {
    warning("cache does not exist (yet)")
  } else {
    files = tibble::tibble(
      paths = fs::dir_ls(cache,recurse=TRUE)
    ) %>% dplyr::mutate( filename = fs::path_file(paths))

    if(!interactive) {
      lapply(files$paths, unlink)
    } else {
      message("About to delete ",nrow(files)," cached files.")
      sure = utils::menu(c("Yes", "No"), title="Are you sure?")
      if(sure==1 | interactive==FALSE) lapply(files$paths, unlink)
      else message("operation aborted by the user")
    }
  }
  invisible(NULL)
}

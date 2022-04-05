#
.md5obj = function(obj) {
  as.character(digest::digest(obj, algo="md5"))
}

#.arear.cache <- new.env(parent=emptyenv())

.cached = function (
  expr = {},
  hash = NULL,
  name = "cache",
  nocache = FALSE,
  cache = getOption("arear.cache.dir", default=tempdir()),
  stale = getOption("arear.cache.stale", default=Inf),
  ...)
{

  code = deparse(substitute(expr))
  md5code = .md5obj(code)

  if(!stringr::str_ends(cache,"/"))
    cache = paste0(cache,"/")

  dir.create(cache, recursive = TRUE, showWarnings = FALSE)

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



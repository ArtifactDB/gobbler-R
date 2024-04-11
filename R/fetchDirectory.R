#' Fetch a directory from the registry
#'
#' Obtain a path to a subdirectory in the registry,
#' possibly creating a local copy of the subdirectory's contents if the caller is not on the same filesystem as the registry.
#'
#' @param path String containing the relative path to a subdirectory within the registry.
#' This usually takes the form of \code{PROJECT/ASSET/VERSION/*}.
#' @param registry String containing a path to the registry.
#' @param url String containing the URL to the Gobbler REST API.
#' @param cache String containing a path to a cache directory.
#' If \code{NULL}, an appropriate cache is automatically chosen.
#' @param forceRemote Logical scalar indicating whether to force remote access.
#' This will download all files in the \code{path} via the REST API and cache them locally.
#' @param overwrite Logical scalar indicating whether to overwrite the existing cache.
#'
#' @return Path to the subdirectory on the caller's filesystem.
#' This is either a path to the registry if it is accessible,
#' or a path to a local cache of the registry's contents otherwise.
#'
#' @author Aaron Lun
#' 
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up an upload. 
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' dir.create(file.path(src, "whee"))
#' write(file=file.path(src, "whee", "blah"), "stuff")
#' write(file=file.path(src, "whee2"), "more-stuff")
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#'
#' # Now fetching the directory.
#' dir <- fetchDirectory("test/simple/v1", registry=info$registry, url=info$url)
#' dir
#' list.files(dir, recursive=TRUE)
#' 
#' # Or, forcing remote access:
#' cache <- tempfile()
#' dir1 <- fetchDirectory("test/simple/v1", 
#'     registry=info$registry, 
#'     url=info$url, 
#'     cache=cache, 
#'     forceRemote=TRUE
#' )
#' dir1
#' list.files(dir1, recursive=TRUE)
#'
#' @export
#' @import httr2
fetchDirectory <- function(path, registry, url, cache=NULL, forceRemote=FALSE, overwrite=FALSE) {
    if (!forceRemote && file.exists(registry)) {
        return(file.path(registry, path))
    }

    cache <- local_registry(cache, url)
    final <- file.path(cache, "REGISTRY", path)
    ok <- file.path(cache, "SUCCESS", path, "....OK")
    if (!overwrite && file.exists(ok) && file.exists(final)) {
        return(final)
    }

    req <- request(paste0(url, "/list?path=", URLencode(path), "&recursive=true"))
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)
    res <- req_perform(req)
    listing <- resp_body_json(res)
    for (l in listing) {
        acquire_file(cache, path, l, url=url, overwrite=overwrite)
    }

    # We use a directory-level OK file to avoid having to scan through all 
    # the directory contents to indicate that it's complete.
    dir.create(dirname(ok), showWarnings=FALSE, recursive=TRUE)
    write(file=ok, character(0))
    final
}

#' @importFrom utils download.file
acquire_file <- function(cache, path, name, url, overwrite) {
    target <- file.path(cache, "REGISTRY", path, name)

    if (overwrite || !file.exists(target)) {
        tempdir <- file.path(cache, "TEMP")
        dir.create(tempdir, recursive=TRUE, showWarnings=FALSE)
        tempf <- tempfile(tmpdir=tempdir)
        on.exit(unlink(tempf), add=TRUE, after=FALSE)

        if (download.file(paste0(url, "/fetch/", path, "/", name), tempf)) {
            stop("failed to download '", name, "' from the registry")
        }
        dir.create(dirname(target), recursive=TRUE, showWarnings=FALSE)
        file.rename(tempf, target) # this should be more or less atomic, so no need for locks.
    }

    target
}

#' @importFrom utils URLencode
local_registry <- function(cache, url) {
    if (is.null(cache)) {
        cache <- tools::R_user_dir("gobbler", "data")
    }
    file.path(cache, URLencode(url, reserved=TRUE))
}

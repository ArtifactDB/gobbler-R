#' Fetch a single file
#'
#' Fetch a single file from the registry.
#' This will call the REST API if the caller is not on the same filesystem as the registry.
#'
#' @param path String containing the relative path to a file within the registry.
#' This usually takes the form of \code{PROJECT/ASSET/VERSION/*}, where path components should be separated by \code{/}.
#' @inheritParams fetchDirectory
#'
#' @author Aaron Lun
#' 
#' @return String containing the path to the file on the caller's filesystem.
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
#' uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#'
#' fetchFile("test/simple/v1/foo", registry=info$registry, url=info$url)
#' fetchFile("test/simple/v1/whee/blah", registry=info$registry, url=info$url, forceRemote=TRUE)
#' 
#' @export
fetchFile <- function(path, registry, url, cache=NULL, forceRemote=FALSE, overwrite=FALSE) {
    if (!forceRemote && file.exists(registry)) {
        file.path(registry, path)
    } else {
        cache <- local_registry(cache, url)
        acquire_file_raw(cache, path, url=url, overwrite=overwrite)
    }
}

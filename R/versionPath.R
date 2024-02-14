#' Path to a versioned asset
#'
#' Obtain the path to the directory containing a versioned asset in the registry.
#'
#' @inheritParams fetchManifest
#'
#' @author Aaron Lun
#' 
#' @return String containing a path to a versioned asset in the registry.
#' 
#' @examples
#' # Mocking up an upload. 
#' info <- startGobbler()
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)
#' stopGobbler(info, keep.dir=TRUE)
#'
#' # Obtaining the path.
#' versionPath("test", "simple", "v1", registry=info$registry)
#' 
#' @export
versionPath <- function(project, asset, version, registry) {
    file.path(registry, project, asset, version)
}

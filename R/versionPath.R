#' Path to a versioned asset
#'
#' Obtain the path to the directory containing a versioned asset in the registry.
#'
#' @inheritParams fetchManifest
#' @param ... Further arguments to pass to \code{\link{fetchDirectory}}.
#'
#' @author Aaron Lun
#' 
#' @return String containing a path to a versioned asset in the registry.
#' 
#' @examples
#' info <- startGobbler()
#' versionPath("test", "simple", "v1", registry=info$registry, url=info$url)
#' 
#' @export
versionPath <- function(project, asset, version, ...) {
    fetchDirectory(paste(project, asset, version, sep="/"), ...)
}

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
#' versionPath("test-R", "basic", "v1")
#' 
#' @export
versionPath <- function(project, asset, version, registry) {
    file.path(registry, project, asset, version)
}

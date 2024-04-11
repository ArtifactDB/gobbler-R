#' List asset versions
#'
#' List all versions of a project asset.
#' This will call the REST API if the caller is not on the same filesystem as the registry.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @inheritParams listProjects
#'
#' @author Aaron Lun
#'
#' @return Character vector of versions.
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start from a clean slate.
#' createProject("test", info$staging, url=info$url) 
#'
#' # Mocking up a few uploads.
#' src <- allocateUploadDirectory(info$staging)
#' for (v in c("v1", "v2")) {
#'     uploadDirectory("test", "simple", v, src, staging=info$staging, url=info$url)
#' }
#'
#' # Listing the versions of the asset:
#' listVersions("test", "simple", registry=info$registry, url=info$url)
#' 
#' # Force remote listing:
#' listVersions("test", "simple", registry=info$registry, url=info$url, forceRemote=TRUE)
#' @export
listVersions <- function(project, asset, registry, url, forceRemote=FALSE) {
    list_registry_directories(paste0(project, "/", asset), registry, url, forceRemote)
}

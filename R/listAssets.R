#' List assets
#'
#' List all assets in a project.
#' This will call the REST API if the caller is not on the same filesystem as the registry.
#'
#' @param project String containing the project name.
#' @inheritParams listProjects
#'
#' @author Aaron Lun
#'
#' @return Character vector of asset names.
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up a few uploads.
#' src <- allocateUploadDirectory(info$staging)
#' for (ass in c("simple", "more-simple", "even-more-simple")) {
#'     uploadDirectory("test", ass, "v1", src, staging=info$staging, url=info$url)
#' }
#'
#' # Listing available assets:
#' listAssets("test", registry=info$registry, url=info$url)
#'
#' # Force remote listing:
#' listAssets("test", registry=info$registry, url=info$url, forceRemote=TRUE)
#'
#' @export
listAssets <- function(project, registry, url, forceRemote=FALSE) {
    list_registry_directories(project, registry, url, forceRemote)
}

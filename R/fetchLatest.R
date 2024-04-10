#' Fetch the latest version
#'
#' Fetch the latest version of a project's asset.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param registry String containing a path to the registry.
#'
#' @return String containing the latest version of the asset. 
#' This may also be \code{NULL} if the asset has no (non-probational) versions.
#'
#' @author Aaron Lun
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url) 
#'
#' # Mocking up a few uploads.
#' src <- allocateUploadDirectory(info$staging)
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#' res <- uploadDirectory("test", "simple", "v2", src, staging=info$staging, url=info$url)
#'
#' # Obtaining the latest version of this asset.
#' fetchLatest("test", "simple", registry=info$registry)
#'
#' @seealso
#' \code{\link{refreshLatest}}, to refresh the latest version.
#'
#' @export
#' @importFrom jsonlite fromJSON
fetchLatest <- function(project, asset, registry) {
    proposed <- file.path(registry, project, asset, "..latest")
    if (!file.exists(proposed)) {
        NULL
    } else {
        vers <- fromJSON(proposed, simplifyVector=FALSE)
        vers$version
    }
}

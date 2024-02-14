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
#' # Mocking up an upload. 
#' info <- startGobbler()
#' src <- allocateUploadDirectory(info$staging)
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)
#' res <- uploadDirectory("test", "simple", "v2", src, staging=info$staging)
#' stopGobbler(info, keep.dir=TRUE)
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

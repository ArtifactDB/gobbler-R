#' Refresh the latest version
#'
#' Recompute the latest version of a project's asset.
#' This is useful on rare occasions where multiple simultaneous uploads cause the latest version to be slightly out of sync.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param staging String containing a path to the staging directory.
#'
#' @return String containing the latest version of the project, or \code{NULL} if there are no non-probational versions.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{fetchLatest}}, to get the latest version without recomputing it.
#' 
#' @examples
#' info <- startGobbler()
#'
#' # Mocking up a few uploads.
#' src <- allocateUploadDirectory(info$staging)
#' removeAsset("test", "simple", info$staging) # clean out existing entries
#' for (v in c("v1", "v2")) {
#'     uploadDirectory("test", "simple", v, src, staging=info$staging)
#' }
#' 
#' # Delete the ..latest file.
#' unlink(file.path(info$registry, "test", "simple", "..latest")) 
#' fetchLatest("test", "simple", registry=info$registry)
#'
#' # Recomputing it:
#' refreshLatest("test", "simple", info$staging)
#' fetchLatest("test", "simple", registry=info$registry)
#'
#' @export
#' @importFrom jsonlite fromJSON
refreshLatest <- function(project, asset, staging) {
    chosen <- dump_request(staging, "refresh_latest", list(project=project, asset=asset))
    resp <- wait_response(staging, chosen)
    invisible(resp$version)
}

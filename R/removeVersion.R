#' Remove a versioned asset
#'
#' Remove a version of an asset of a project from the registry.
#'
#' @param project String containing the project to remove.
#' @param asset String containing the asset to remove.
#' @param version String containing the version of the asset to remove.
#' @param staging String containing a path to a staging directory.
#'
#' @return \code{NULL} is invisibly returned if the version was successfully removed. 
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{removeProject}} and \code{\link{removeAsset}}, to remove a project or asset.
#'
#' @examples
#' info <- startGobbler()
#'
#' # Mocking up an upload. 
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)
#' listVersions("test", "simple", registry=info$registry)
#'
#' # Removing the version.
#' removeVersion("test", "simple", "v1", staging=info$staging)
#' listProjects("test", "simple", registry=info$registry)
#'
#' stopGobbler(info)
#' @export
removeVersion <- function(project, asset, version, staging) {
    chosen <- dump_request(staging, "remove_version", list(project=project, asset=asset, version=version))
    wait_response(staging, chosen)
    invisible(NULL)
}

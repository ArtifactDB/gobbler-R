#' Remove a versioned asset
#'
#' Remove a version of an asset of a project from the registry.
#'
#' @param project String containing the project to remove.
#' @param asset String containing the asset to remove.
#' @param version String containing the version of the asset to remove.
#' @inheritParams createProject
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
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up a version if it doesn't already exist.
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#' listVersions("test", "simple", registry=info$registry)
#'
#' # Removing the version.
#' removeVersion("test", "simple", "v1", staging=info$staging, url=info$url)
#' listVersions("test", "simple", registry=info$registry)
#'
#' @export
removeVersion <- function(project, asset, version, staging, url) {
    dump_request(staging, url, "delete_version", list(project=project, asset=asset, version=version))
    invisible(NULL)
}

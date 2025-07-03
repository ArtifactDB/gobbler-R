#' Remove a versioned asset
#'
#' Remove a version of an asset of a project from the registry.
#' This should only be performed by Gobbler instance administrators,
#' who should consider running \code{\link{rerouteLinks}} beforehand to avoid dangling references to this version.
#'
#' @param project String specifying the project containing the version of the asset to remove.
#' @param asset String specifying the asset containing the version to remove.
#' @param version String containing the version of the asset to remove.
#' @param force Logical scalar indicating that the asset should be forcibly removed if it contains invalid files.
#' If this needs to be set to \code{TRUE}, users may need to call \code{\link{refreshUsage}} to correct project-level usage statistics.
#' Similarly, \code{\link{refreshLatest}} may also need to be called.
#' @inheritParams createProject
#'
#' @return \code{NULL} is invisibly returned if the version was successfully removed.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{removeProject}} and \code{\link{removeAsset}}, to remove a project or asset.
#'
#' \code{\link{rerouteLinks}}, to reroute links to this version's contents prior to deletion.
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up a version if it doesn't already exist.
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#' listVersions("test", "simple", registry=info$registry)
#'
#' # Removing the version.
#' removeVersion("test", "simple", "v1", staging=info$staging, url=info$url)
#' listVersions("test", "simple", registry=info$registry)
#'
#' @export
removeVersion <- function(project, asset, version, staging, url, force=FALSE) {
    dump_request(staging, url, "delete_version", list(project=project, asset=asset, version=version, force=force))
    invisible(NULL)
}

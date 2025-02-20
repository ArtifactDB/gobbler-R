#' Remove an asset of a project 
#'
#' Remove an asset of the project from the registry.
#' This should only be performed by Gobbler instance administrators,
#' who should consider running \code{\link{rerouteLinks}} beforehand to avoid dangling references to this version.
#'
#' @param project String specifying the project containing the asset to remove.
#' @param asset String specifying the asset to remove.
#' @param force Logical scalar indicating that the asset should be forcibly removed if it contains invalid files.
#' If this needs to be set to \code{TRUE}, users may need to call \code{\link{refreshUsage}} afterwards to correct project-level usage statistics.
#' @inheritParams createProject 
#'
#' @return \code{NULL} is invisibly returned if the asset was successfully removed. 
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{removeProject}} and \code{\link{removeVersion}}, to remove projects and versions respectively.
#'
#' \code{\link{rerouteLinks}}, to reroute links to this asset's contents prior to deletion.
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up an asset so we have something to remove.
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#' listAssets("test", registry=info$registry)
#'
#' # Removing the asset.
#' removeAsset("test", "simple", staging=info$staging, url=info$url)
#' listAssets("test", registry=info$registry)
#'
#' @export
removeAsset <- function(project, asset, staging, url, force=FALSE) {
    dump_request(staging, url, "delete_asset", list(project=project, asset=asset, force=force))
    invisible(NULL)
}

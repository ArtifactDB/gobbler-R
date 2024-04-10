#' Remove an asset of a project 
#'
#' Remove an asset of the project from the registry.
#'
#' @param project String containing the project to remove.
#' @param asset String containing the asset to remove.
#' @inheritParams createProject 
#'
#' @return \code{NULL} is invisibly returned if the asset was successfully removed. 
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{removeProject}} and \code{\link{removeVersion}}, to remove projects and versions respectively.
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up an asset so we have something to remove.
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#' listAssets("test", registry=info$registry)
#'
#' # Removing the asset.
#' removeAsset("test", "simple", staging=info$staging, url=info$url)
#' listAssets("test", registry=info$registry)
#'
#' @export
removeAsset <- function(project, asset, staging, url) {
    dump_request(staging, url, "delete_asset", list(project=project, asset=asset))
    invisible(NULL)
}

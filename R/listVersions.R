#' List asset versions
#'
#' List all versions of a project asset.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param registry String containing a path to the registry.
#'
#' @author Aaron Lun
#'
#' @return Character vector of versions.
#'
#' @examples
#' # Mocking up an upload. 
#' info <- startGobbler()
#' src <- allocateUploadDirectory(info$staging)
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)
#' res <- uploadDirectory("test", "simple", "v2", src, staging=info$staging)
#' stopGobbler(info, keep.dir=TRUE)
#'
#' # Listing the versions of the asset:
#' listVersions("test", "simple", registry=info$registry)
#' 
#' @export
listVersions <- function(project, asset, registry) {
    list.files(file.path(registry, project, asset))
}

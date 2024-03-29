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
#' info <- startGobbler()
#'
#' # Mocking up a few uploads.
#' src <- allocateUploadDirectory(info$staging)
#' removeAsset("test", "simple", info$staging) # clean out existing entries
#' for (v in c("v1", "v2")) {
#'     uploadDirectory("test", "simple", v, src, staging=info$staging)
#' }
#'
#' # Listing the versions of the asset:
#' listVersions("test", "simple", registry=info$registry)
#' 
#' @export
listVersions <- function(project, asset, registry) {
    list.files(file.path(registry, project, asset))
}

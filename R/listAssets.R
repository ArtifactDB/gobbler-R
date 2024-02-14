#' List assets
#'
#' List all assets in a project.
#'
#' @param project String containing the project name.
#' @param registry String containing a path to the registry.
#'
#' @author Aaron Lun
#'
#' @return Character vector of asset names.
#'
#' @examples
#' # Mocking up an upload. 
#' info <- startGobbler()
#' src <- allocateUploadDirectory(info$staging)
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)
#' res <- uploadDirectory("test", "more-simple", "v1", src, staging=info$staging)
#' res <- uploadDirectory("test", "even-more-simple", "v1", src, staging=info$staging)
#'
#' # Listing available assets:
#' listAssets("test", registry=info$registry)
#' 
#' @export
listAssets <- function(project, registry) {
    list.files(file.path(registry, project))
}

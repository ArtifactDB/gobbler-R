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
#' info <- startGobbler()
#'
#' # Mocking up a few uploads.
#' src <- allocateUploadDirectory(info$staging)
#' removeProject("test", info$staging) # clean out existing entries
#' for (ass in c("simple", "more-simple", "even-more-simple")) {
#'     uploadDirectory("test", ass, "v1", src, staging=info$staging)
#' }
#'
#' # Listing available assets:
#' listAssets("test", registry=info$registry)
#' 
#' @export
listAssets <- function(project, registry) {
    list.files(file.path(registry, project))
}

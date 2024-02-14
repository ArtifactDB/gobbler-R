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
#' listAssets("test-R")
#' 
#' @export
listAssets <- function(project, registry) {
    list.files(file.path(registry, project))
}

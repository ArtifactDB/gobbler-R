#' Fetch the latest version
#'
#' Fetch the latest version of a project's asset.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param registry String containing a path to the registry.
#'
#' @return String containing the latest version of the project.
#'
#' @author Aaron Lun
#'
#' @examples
#' fetchLatest("test-R", "basic")
#'
#' @seealso
#' \code{\link{refreshLatest}}, to refresh the latest version.
#'
#' @export
#' @importFrom jsonlite fromJSON
fetchLatest <- function(project, asset, registry=registryPath()) {
    vers <- fromJSON(file.path(registry, project, asset, "..latest"), simplifyVector=FALSE)
    vers$version
}

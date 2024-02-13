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
#' listVersions("test-R", "basic")
#' 
#' @export
#' @importFrom aws.s3 get_bucket
listVersions <- function(project, asset, registry=registryPath()) {
    list.files(file.path(registry, project, asset))
}

#' Fetch project usage details
#'
#' Fetch the quota usage for a project.
#' 
#' @param project String containing the project name.
#' @param registry String containing the path to the registry.
#'
#' @return Numeric scalar specifying the quota usage for the project, in bytes.
#'
#' @author Aaron Lun
#'
#' @examples
#' fetchUsage("test-R")
#'
#' @seealso
#' \code{\link{refreshUsage}}, to recompute the used quota.
#'
#' @export
#' @importFrom jsonlite fromJSON
fetchUsage <- function(project, registry=registryPath()) {
    out <- fromJSON(file.path(registry, project, "..usage"), simplifyVector=FALSE)
    out$total
}

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
#' info <- startGobbler()
#'
#' # Mocking up an upload. 
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' write(file=file.path(src, "whee"), "stuff")
#' removeProject("test", info$staging) # clean out any existing entry
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)
#'
#' # Obtaining the project usage.
#' fetchUsage("test", registry=info$registry)
#'
#' @seealso
#' \code{\link{refreshUsage}}, to recompute the used quota.
#'
#' @export
#' @importFrom jsonlite fromJSON
fetchUsage <- function(project, registry) {
    out <- fromJSON(file.path(registry, project, "..usage"), simplifyVector=FALSE)
    out$total
}

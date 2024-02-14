#' Refresh the quota usage
#'
#' Recompute the quota usage of a project.
#' This is useful on rare occasions where multiple simultaneous uploads cause the usage calculations to be out of sync.
#' 
#' @param project String containing the project name.
#' @param staging String containing the path to the staging directory.
#'
#' @return Numeric scalar specifying the total quota usage of this project, in bytes.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{fetchUsage}}, to get the usage without recomputing it.
#'
#' @examples
#' info <- startGobbler()
#'
#' # Mocking up an upload. 
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' write(file=file.path(src, "whee"), "stuff")
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)
#'
#' # Messing with the project usage:
#' write(file=file.path(info$registry, "test", "..usage"), '{ "total": 0 }')
#' fetchUsage("test", registry=info$registry)
#'
#' # Fixing the project usage.
#' refreshUsage("test", staging=info$staging)
#' fetchUsage("test", registry=info$registry)
#'
#' stopGobbler(info, keep.dir=TRUE)
#' @export
#' @importFrom jsonlite fromJSON
refreshUsage <- function(project, staging) {
    chosen <- dump_request(staging, "refresh_usage", list(project=project))
    resp <- wait_response(staging, chosen)
    invisible(resp$total)
}

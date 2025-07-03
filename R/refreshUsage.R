#' Refresh the quota usage
#'
#' Recompute the quota usage of a project.
#' This is useful on rare occasions where multiple simultaneous uploads cause the usage calculations to be out of sync.
#'
#' @param project String containing the project name.
#' @inheritParams createProject
#'
#' @return Numeric scalar specifying the total quota usage of this project, in bytes.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{fetchUsage}}, to get the usage without recomputing it.
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up an upload.
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' write(file=file.path(src, "whee"), "stuff")
#' uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#'
#' # Messing with the project usage:
#' write(file=file.path(info$registry, "test", "..usage"), '{ "total": 0 }')
#' fetchUsage("test", registry=info$registry)
#'
#' # Fixing the project usage.
#' refreshUsage("test", staging=info$staging, url=info$url)
#' fetchUsage("test", registry=info$registry)
#'
#' @export
#' @importFrom jsonlite fromJSON
refreshUsage <- function(project, staging, url) {
    resp <- dump_request(staging, url, "refresh_usage", list(project=project))
    invisible(resp$total)
}

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
#' if (interactive()) {
#'     refreshUsage("test-R")
#' }
#'
#' @export
#' @importFrom jsonlite fromJSON
refreshUsage <- function(project, staging) {
    chosen <- dump_request(staging, "refresh_usage", list(project=project))
    resp <- wait_response(staging, chosen)
    invisible(resp$total)
}

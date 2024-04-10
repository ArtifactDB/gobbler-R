#' Check the Gobbler's health
#'
#' Check that the Gobbler is still active and monitoring the staging directory.
#' 
#' @inheritParams createProject
#'
#' @return Logical scalar indicating whether the Gobbler is active.
#'
#' @author Aaron Lun
#'
#' @examples
#' info <- startGobbler()
#' checkHealth(info$staging, info$url)
#' 
#' stopGobbler()
#' checkHealth(info$staging, info$url)
#'
#' @export
#' @import methods
checkHealth <- function(staging, url) {
    out <- try(dump_request(staging, url, "health_check", NULL), silent=TRUE)
    !is(out, "try-error")
}

#' Check the Gobbler's health
#'
#' Check that the Gobbler is still active and monitoring the staging directory.
#' 
#' @param staging String containing the path to the staging directory.
#'
#' @return Logical scalar indicating whether the Gobbler responded within 10 seconds. 
#'
#' @author Aaron Lun
#'
#' @examples
#' if (interactive()) {
#'     refreshUsage("test-R")
#' }
#'
#' @export
checkHealth <- function(staging) {
    chosen <- dump_request(staging, "health_check", NULL)
    wait_response(staging, chosen, error=FALSE, timeout=10)
}

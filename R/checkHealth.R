#' Check the Gobbler's health
#'
#' Check that the Gobbler is still active and monitoring the staging directory.
#' 
#' @param staging String containing the path to the staging directory.
#' @param timeout Integer specifying the number of seconds to wait for a response.
#'
#' @return Logical scalar indicating whether the Gobbler responded within 10 seconds. 
#'
#' @author Aaron Lun
#'
#' @examples
#' info <- startGobbler()
#' checkHealth(info$staging)
#' stopGobbler(info, keep.dir=TRUE)
#' checkHealth(info$staging, timeout=1)
#'
#' @export
checkHealth <- function(staging, timeout=10) {
    chosen <- dump_request(staging, "health_check", NULL)
    wait_response(staging, chosen, error=FALSE, timeout=timeout)
}

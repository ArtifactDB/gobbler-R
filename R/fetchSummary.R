#' Fetch version summary
#'
#' Fetch the summary for a version of an asset of a project.
#'
#' @inheritParams fetchManifest
#'
#' @author Aaron Lun
#' 
#' @return List containing the summary for this version, with the following fields:
#' \itemize{
#' \item \code{upload_user_id}, string containing the identity of the uploader.
#' \item \code{upload_start}, a \link{POSIXct} object containing the upload start time.
#' \item \code{upload_finish}, a \link{POSIXct} object containing the upload finish time.
#' \item \code{on_probation} (optional), a logical scalar indicating whether the upload is probational.
#' If missing, this can be assumed to be \code{FALSE}.
#' }
#' 
#' @examples
#' # Mocking up an upload. 
#' info <- startGobbler()
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)
#'
#' # Obtain a summary for this version.
#' fetchSummary("test", "simple", "v1", registry=info$registry)
#' 
#' @export
fetchSummary <- function(project, asset, version, registry) {
    out <- fromJSON(file.path(registry, project, asset, version, "..summary"), simplifyVector=FALSE)
    out$upload_start <- cast_datetime(out$upload_start)
    out$upload_finish <- cast_datetime(out$upload_finish)
    out
}

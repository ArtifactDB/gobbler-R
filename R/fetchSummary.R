#' Fetch version summary
#'
#' Fetch the summary for a version of an asset of a project.
#' This will call the REST API if the caller is not on the same filesystem as the registry.
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
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up an upload.
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#'
#' # Obtain a summary for this version.
#' fetchSummary("test", "simple", "v1", registry=info$registry, url=info$url)
#'
#' # Force remote access.
#' fetchSummary(
#'     "test",
#'     "simple",
#'     "v1",
#'     registry=info$registry,
#'     url=info$url,
#'     forceRemote=TRUE
#' )
#' @export
#' @importFrom jsonlite fromJSON
fetchSummary <- function(project, asset, version, registry, url, cache=NULL, forceRemote=FALSE, overwrite=FALSE) {
    if (!forceRemote && file.exists(registry)) {
        path <- file.path(registry, project, asset, version, "..summary")
    } else {
        cache <- local_registry(cache, url)
        path <- acquire_file(cache, paste(project, asset, version, sep="/"), "..summary", url=url, overwrite=overwrite)
    }

    out <- fromJSON(path, simplifyVector=FALSE)
    out$upload_start <- cast_datetime(out$upload_start)
    out$upload_finish <- cast_datetime(out$upload_finish)
    out
}

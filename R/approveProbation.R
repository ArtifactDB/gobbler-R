#' Approve a probational upload
#'
#' Pretty much as it says: approve a probational upload of a version of a project's asset.
#' This removes the \code{on_probation} tag from the uploaded version.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @inheritParams createProject
#'
#' @return \code{NULL} is invisibly returned upon successful approval.
#'
#' @seealso
#' \code{\link{rejectProbation}}, to reject the probational upload.
#'
#' \code{\link{uploadDirectory}}, to specify probational uploads.
#' 
#' @author Aaron Lun
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # to start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up an upload. 
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' res <- uploadDirectory("test", "probation", "v1", src, 
#'     staging=info$staging, url=info$url, probation=TRUE)
#' fetchSummary("test", "probation", "v1", registry=info$registry)
#'
#' # After approval, the probation status disappears.
#' approveProbation("test", "probation", "v1", staging=info$staging, url=info$url)
#' fetchSummary("test", "probation", "v1", registry=info$registry)
#' 
#' @export
approveProbation <- function(project, asset, version, staging, url) {
    dump_request(staging, url, "approve_probation", list(project=project, asset=asset, version=version))
    invisible(NULL)
}

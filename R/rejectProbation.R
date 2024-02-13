#' Reject a probational upload
#'
#' Pretty much as it says: reject a probational upload of a version of a project's asset.
#' This removes all files associated with that version.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param staging String containing the path to the staging directory.
#'
#' @return \code{NULL} is invisibly returned upon successful rejection.
#'
#' @seealso
#' \code{\link{approveProbation}}, to approve the probational upload.
#'
#' \code{\link{uploadDirectory}}, to specify probational uploads.
#' 
#' @author Aaron Lun
#' @examples
#' if (interactive()) {
#'     # Mocking up a versioned asset.
#'     init <- startUpload(
#'         project="test-R", 
#'         asset="probation-reject", 
#'         version="v1", 
#'         files=character(0),
#'         probation=TRUE
#'     )
#'     completeUpload(init) 
#'
#'     # Rejecting the probation:
#'     rejectProbation("test-R", "probation-reject", "v1")
#' }
#' 
#' @export
#' @import httr2
rejectProbation <- function(project, asset, version, staging=stagingPath()) {
    chosen <- dump_request(staging, "reject_probation", list(project=project, asset=asset, version=version))
    wait_response(staging, chosen)
    invisible(NULL)
}

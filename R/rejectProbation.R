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
#' info <- startGobbler()
#'
#' # Mocking up an upload. 
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' res <- uploadDirectory("test", "probation", "v1", src, staging=info$staging, probation=TRUE)
#' listVersions("test", "probation", registry=info$registry)
#'
#' # After rejection, the version disppears.
#' rejectProbation("test", "probation", "v1", staging=info$staging)
#' listVersions("test", "probation", registry=info$registry)
#' 
#' stopGobbler(info)
#' 
#' @export
rejectProbation <- function(project, asset, version, staging) {
    chosen <- dump_request(staging, "reject_probation", list(project=project, asset=asset, version=version))
    wait_response(staging, chosen)
    invisible(NULL)
}

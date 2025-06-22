#' Reject a probational upload
#'
#' Pretty much as it says: reject a probational upload of a version of a project's asset.
#' This removes all files associated with that version.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param force Logical scalar indicating that the version should be forcibly rejected and removed if it contains invalid files.
#' If this needs to be set to \code{TRUE}, users may need to call \code{\link{refreshUsage}} afterwards to correct project-level usage statistics.
#' @param spoof String containing the name of a user on whose behalf this request is being made.
#' This should only be used if the Gobbler service allows spoofing by the current user. 
#' If \code{NULL}, no spoofing is performed.
#' @inheritParams createProject 
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
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up an upload. 
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' uploadDirectory("test", "probation", "v1", src, 
#'     staging=info$staging, url=info$url, probation=TRUE)
#' listVersions("test", "probation", registry=info$registry)
#'
#' # After rejection, the version disppears.
#' rejectProbation("test", "probation", "v1", staging=info$staging, url=info$url)
#' listVersions("test", "probation", registry=info$registry)
#' 
#' @export
rejectProbation <- function(project, asset, version, staging, url, force=FALSE, spoof=NULL) {
    dump_request(staging, url, "reject_probation", list(project=project, asset=asset, version=version, force=force), spoof=spoof)
    invisible(NULL)
}

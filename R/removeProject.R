#' Remove a project 
#'
#' Remove a project from the registry.
#'
#' @param project String containing the project to remove.
#' @param staging String containing the path to the staging directory. 
#'
#' @return \code{NULL} is invisibly returned if the project was successfully removed. 
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{removeAsset}} and \code{\link{removeVersion}}, to remove assets and versions respectively.
#'
#' @examples
#' info <- startGobbler()
#'
#' # Mocking up an upload. 
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging) 
#' listProjects("test", registry=info$registry)
#'
#' # Removing the asset.
#' removeProject("test", staging=info$staging)
#' listProjects("test", registry=info$registry)
#'
#' @export
removeProject <- function(project, staging) {
    chosen <- dump_request(registry, "remove_project", list(project=project))
    wait_response(staging, chosen)
    invisible(NULL)
}

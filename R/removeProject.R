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
#' \code{\link{createProject}}, to create a project.
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging) # start with a clean slate.
#'
#' # Mocking up a project so we have something to delete.
#' createProject("test", info$staging)
#' listProjects(registry=info$registry)
#'
#' # Removing the project.
#' removeProject("test", staging=info$staging)
#' listProjects(registry=info$registry)
#'
#' @export
removeProject <- function(project, staging) {
    chosen <- dump_request(staging, "delete_project", list(project=project))
    wait_response(staging, chosen)
    invisible(NULL)
}

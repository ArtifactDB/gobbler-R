#' Remove a project 
#'
#' Remove a project from the registry.
#'
#' @param project String containing the project to remove.
#' @inheritParams createProject
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
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#'
#' # Mocking up a project so we have something to delete.
#' createProject("test", info$staging, url=info$url)
#' listProjects(registry=info$registry)
#'
#' # Removing the project.
#' removeProject("test", staging=info$staging, url=info$url)
#' listProjects(registry=info$registry)
#'
#' @export
removeProject <- function(project, staging, url) {
    dump_request(staging, url, "delete_project", list(project=project))
    invisible(NULL)
}

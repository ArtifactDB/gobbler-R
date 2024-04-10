#' Create a project 
#'
#' Create a new project in the registry.
#'
#' @param project String containing the name of the project to create.
#' @param owners Character vector containing the user IDs for owners of this project.
#' This defaults to the current user.
#' @param uploaders List specifying the authorized uploaders for this project.
#' See the \code{uploaders} field in the \code{\link{fetchPermissions}} return value for the expected format. 
#' @param staging String containing the path to the staging directory.
#' @param url String containing the URL of the gobbler REST API.
#'
#' @return \code{NULL} is invisibly returned if the project was successfully created. 
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{uploadDirectory}}, to upload a new version of an asset to an existing project.
#'
#' \code{\link{removeProject}}, to remove a project respectively.
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", staging=info$staging, url=info$url) # start with a clean slate.
#'
#' # Creating our new project.
#' createProject("test", staging=info$staging, url=info$url)
#' listProjects(registry=info$registry)
#'
#' @export
createProject <- function(project, staging, url, owners=NULL, uploaders=NULL) {
    req <- list(project=project)

    permissions <- list()
    if (!is.null(owners)) {
        permissions$owners <- as.list(owners)
    }
    if (!is.null(uploaders)) {
        permissions$uploaders <- sanitize_uploaders(uploaders)
    }
    if (length(permissions)) {
        req$permissions <- permissions
    }

    dump_request(staging, url, "create_project", req)
    invisible(NULL)
}

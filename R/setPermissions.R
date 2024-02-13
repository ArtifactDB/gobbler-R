#' Set project permissions 
#' 
#' Set the owner and uploader permissions for a project.
#'
#' @param project String containing the project name.
#' @param owners Character vector containing the GitHub users or organizations that are owners of this project.
#' If \code{NULL}, no change is made to the existing owners of the project.
#' @param uploaders List specifying the authorized uploaders for this project.
#' See the \code{uploaders} field in the \code{\link{fetchPermissions}} return value for the expected format. 
#' If \code{NULL}, no change is made to the existing uploaders of the project.
#' @param append Logical scalar indicating whether \code{owners} and \code{uploaders} should be appended to the existing owners and uploaders, respectively, of the project.
#' If \code{FALSE}, the \code{owners} and \code{uploaders} are used to replace the existing values.
#' @param registry String containing a path to the registry.
#' @param staging String containing a path to the staging directory.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{fetchPermissions}}, to fetch the permissions.
#'
#' @return \code{NULL} is invisibly returned upon successful setting of the permissions.
#'
#' @examples
#' if (interactive()) {
#'     # Creating a project for demonstration purposes.
#'     createProject("test-R-perms", owners="LTLA")
#'
#'     # Setting extra permissions on this project.
#'     setPermissions("test-R-perms",
#'         owners="jkanche", 
#'         uploaders=list(list(id="lawremi", until=Sys.time() + 1000))
#'     )
#' }
#'
#' @export
setPermissions <- function(project, owners=NULL, uploaders=NULL, append=TRUE, registry=registryPath(), staging=stagingPath()) {
    perms <- list()
    if (append) {
        old.perms <- fetchPermissions(project, registry=registry)
        if (!is.null(owners)) {
            perms$owners <- as.list(union(unlist(old.perms$owners), owners))
        }
        if (!is.null(uploaders)) {
            perms$uploaders <- c(old.perms$uploaders, uploaders)
        }
    } else {
        if (!is.null(owners)) {
            perms$owners <- as.list(owners)
        }
        if (!is.null(uploaders)) {
            perms$uploaders <- uploaders 
        }
    }

    if (!is.null(perms$uploaders)) {
        perms$uploaders <- sanitize_uploaders(perms$uploaders)
    }

    chosen <- dump_request(staging, "set_permissions", list(project=project, permissions=perms))
    wait_response(staging, chosen)
    invisible(NULL)
}

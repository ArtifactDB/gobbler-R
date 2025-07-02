#' Set project permissions 
#' 
#' Set the owner and uploader permissions for a project.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' If specified, permissions are set on the asset rather than the entire project.
#' @param owners Character vector containing the user IDs for owners of this project/asset.
#' If \code{NULL}, no change is made to the existing owners of the project.
#' @param uploaders List specifying the authorized uploaders for this project/asset.
#' See the \code{uploaders} field in the \code{\link{fetchPermissions}} return value for the expected format. 
#' If \code{NULL}, no change is made to the existing uploaders of the project/asset.
#' @param globalWrite Logical scalar indicating whether global writes should be enabled (see \code{\link{fetchPermissions}} for details).
#' If \code{NULL}, no change is made to the global write status of the project.
#' Ignored if \code{asset} is specified.
#' @param append Logical scalar indicating whether \code{owners} and \code{uploaders} should be appended to the existing owners and uploaders, respectively, of the project/asset.
#' If \code{FALSE}, the \code{owners} and \code{uploaders} are used to replace the existing values.
#' @param registry String containing a path to the registry.
#' @param spoof String containing the name of a user on whose behalf this request is being made.
#' This should only be used if the Gobbler service allows spoofing by the current user. 
#' If \code{NULL}, no spoofing is performed.
#' @param dryRun Logical scalar indicating whether to return the new permissions without actually modifying the registry.
#' @inheritParams createProject
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{fetchPermissions}}, to fetch the permissions.
#'
#' \code{\link{createProject}}, to set permissions during project creation.
#'
#' @return If \code{dryRun=FALSE}, \code{NULL} is invisibly returned upon successful setting of the permissions.
#'
#' If \code{dryRun=TRUE}, a named list is returned containing the new permissions for the project/asset.
#' This contains zero, one or more of \code{owners}, \code{uploaders} and \code{global_write} (see \code{\link{fetchPermissions}} for descriptions).
#' A missing field indicates that it will not be updated in the project/asset permissions. 
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
#' fetchPermissions("test", registry=info$registry)
#'
#' # Setting them to something else.
#' setPermissions("test", 
#'     owners=c("mum", "dad"), 
#'     uploaders=list(
#'         list(id='brother1', asset='ps5', until=Sys.time() + 100000),
#'         list(id='brother2', asset='harry_potter', version='goblet_of_fire')
#'     ),
#'     staging=info$staging,
#'     url=info$url,
#'     registry=info$registry
#' )
#' fetchPermissions("test", registry=info$registry)
#'
#' @export
setPermissions <- function(project, registry, staging, url, asset=NULL, owners=NULL, uploaders=NULL, globalWrite=NULL, append=TRUE, spoof=NULL, dryRun=FALSE) {
    perms <- list()
    names(perms) <- character(0)

    if (append) {
        old.perms <- fetchPermissions(project, asset=asset, registry=registry, url=url)
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

    payload <- list(project=project)
    if (!is.null(asset)) {
        payload$asset <- asset
    } else if (!is.null(globalWrite)) {
        perms$global_write <- globalWrite
    }

    if (dryRun) {
        return(perms)
    }
    payload$permissions <- perms

    dump_request(staging, url, "set_permissions", payload, spoof=spoof)
    invisible(NULL)
}

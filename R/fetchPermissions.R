#' Fetch project permissions 
#'
#' Fetch the permissions for a project.
#' This will call the REST API if the caller is not on the same filesystem as the registry.
#' 
#' @param project String containing the project name.
#' @inheritParams listProjects
#'
#' @return List containing the permissions for this project.
#' This has the following elements:
#' \itemize{
#' \item \code{owners}, a character vector containing the user IDs of owners of this project.
#' \item \code{uploaders}, a list of lists specifying the users or organizations who are authorzied to upload to this project.
#' Each entry is a list with the following fields:
#' \itemize{
#' \item \code{id}, a string containing a user ID that is authorized to upload.
#' \item (optional) \code{asset}, a string containing the name of the asset that the uploader is allowed to upload to.
#' If not provided, there is no restriction on the uploaded asset name.
#' \item (optional) \code{version}, a string containing the name of the version that the uploader is allowed to upload to.
#' If not provided, there is no restriction on the uploaded version name.
#' \item (optional) \code{until}, a \link{POSIXct} object containing the expiry date of this authorization.
#' If not provided, the authorization does not expire.
#' \item (optional) \code{trusted}, a logical scalar indicating whether the uploader is trusted.
#' If not provided, defaults to \code{FALSE}.
#' }
#' \item (optional) \code{global_write}, a logical scalar indicating whether global writes are enabled. 
#' In this mode, any user can create any number of new assets in this project.
#' Each user can also upload new versions of any asset that they created in this mode.
#' }
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{setPermissions}}, to set the permissions.
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#'
#' # Mocking up a project.upload. 
#' createProject("test", info$staging, url=info$url,
#'     uploaders=list(list(id="urmom", until=Sys.time() + 1000)))
#'
#' # Fetching the permissions.
#' fetchPermissions("test", registry=info$registry, url=info$url)
#'
#' # Forcing remote access.
#' fetchPermissions("test", registry=info$registry, url=info$url, forceRemote=TRUE)
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @import httr2
fetchPermissions <- function(project, registry, url, forceRemote=FALSE) {
    if (file.exists(registry) && !forceRemote) {
        content <- file.path(registry, project, "..permissions")
    } else {
        req <- request(paste0(url, "/fetch/", paste(project, "..permissions", sep="/")))
        resp <- req_perform(req)
        content <- resp_body_string(resp)
    }

    perms <- fromJSON(content, simplifyVector=FALSE)

    # Converting everything to POSIX dates.
    for (i in seq_along(perms$uploaders)) {
        current <- perms$uploaders[[i]]
        if ("until" %in% names(current)) {
            perms$uploaders[[i]]$until <- cast_datetime(current$until)
        }
    }

    perms
}

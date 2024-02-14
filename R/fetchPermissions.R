#' Fetch project permissions 
#'
#' Fetch the permissions for a project.
#' 
#' @param project String containing the project name.
#' @param registry String containing the path to the registry.
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
#' \item (optional) \code{trusted}, whether the uploader is trusted.
#' If not provided, defaults to \code{FALSE}.
#' }
#' }
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{setPermissions}}, to set the permissions.
#'
#' @examples
#' # Mocking up an upload. 
#' info <- startGobbler()
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging,
#'     uploaders=list(list(id="urmom", until=Sys.time() + 1000)))
#' stopGobbler(info, keep.dir=TRUE)
#'
#' # Fetching the permissions.
#' fetchPermissions("test", registry=info$registry)
#'
#' @export
#' @importFrom jsonlite fromJSON
fetchPermissions <- function(project, registry) {
    perms <- fromJSON(file.path(registry, project, "..permissions"), simplifyVector=FALSE)

    # Converting everything to POSIX dates.
    for (i in seq_along(perms$uploaders)) {
        current <- perms$uploaders[[i]]
        if ("until" %in% names(current)) {
            perms$uploaders[[i]]$until <- cast_datetime(current$until)
        }
    }

    perms
}

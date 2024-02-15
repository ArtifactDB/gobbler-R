#' Upload a directory's contents
#'
#' Upload a directory as a new versioned asset of a project in the registry.
#'
#' @param project String containing the project name.
#' This may be \code{NULL}, in which case \code{prefix} should be provided.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' This may be \code{NULL} to generate a new version name based on an incrementing series.
#' @param directory String containing the path to a directory to be uploaded.
#' This should be inside \code{staging} for best performance, see \code{\link{allocateUploadDirectory}} for details.
#' @param prefix String containing the project name prefix.
#' This is used to derive a project name based on an incrementing series.
#' @param probation Logical scalar indicating whether to upload a probational version.
#' @param owners Character vector containing the user IDs for owners of this project.
#' This defaults to the current user.
#' @param uploaders List specifying the authorized uploaders for this project.
#' See the \code{uploaders} field in the \code{\link{fetchPermissions}} return value for the expected format. 
#' @param staging String containing the path to the staging directory.
#'
#' @return List containing \code{project} and \code{version},
#' the generated names for the project and version respectively.
#' (If these were explicitly provided in the arguments, the same values are returned verbatim.)
#'
#' @author Aaron Lun
#' @examples
#' info <- startGobbler()
#'
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#'
#' res <- uploadDirectory(
#'     project=NULL, 
#'     prefix="TEST", 
#'     asset="simple", 
#'     version=NULL, 
#'     directory=src, 
#'     staging=info$staging)
#' res
#'
#' @seealso
#' \code{\link{versionPath}}, to obtain the path to a versioned asset's contents inside the registry.
#'
#' \code{\link{listManifest}}, to obtain the manifest of the versioned asset's contents.
#'
#' @export
uploadDirectory <- function(project, asset, version, directory, staging, prefix=NULL, probation=FALSE, owners=NULL, uploaders=NULL) {
    directory <- normalizePath(directory)
    staging <- normalizePath(staging)
    if (dirname(directory) != staging) {
        new.dir <- allocateUploadDirectory(staging) 
        for (p in list.files(directory, recursive=TRUE)) {
            src <- file.path(directory, p)
            dest <- file.path(new.dir, p)
            dir.create(dirname(dest), showWarnings=FALSE)

            src.link <- Sys.readlink(src)
            if (src.link == "" || !startsWith(src, "/")) { # i.e., not a link to an absolute path.
                if (!file.link(src, dest) && !file.copy(src, dest)) {
                    stop("failed to link or copy '", p, "' to the staging directory")
                }
            } else {
                if (!file.symlink(src.link, dest)) {
                    stop("failed to create a symlink for '", p, "' in the staging directory")
                }
            }
        }
        directory <- new.dir
    }

    req <- list(
        `source` = basename(directory),
        asset = asset,
        on_probation = probation
    )
    if (!is.null(version)) {
        req$version <- version
    }

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

    if (is.null(project)) {
        if (is.null(prefix)) {
            stop("'prefix' should be provided if 'project=NULL'")
        }
        req$prefix <- prefix
    } else {
        req$project <- project
    }

    chosen <- dump_request(staging, "upload", req)
    wait_response(staging, chosen, timeout=1000)
}

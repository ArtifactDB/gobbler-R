#' List files for a version
#'
#' List the contents of a version of a project asset.
#' This will call the REST API if the caller is not on the same filesystem as the registry.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param prefix String containing the prefix for the path.
#' If provided, files are only listed if they have a relative path (i.e., inside the version subdirectory) that starts with this prefix.
#' If \code{NULL}, all files associated with this version are listed.
#' @param include.. Logical scalar indicating whether to list files with path components that start with \code{..}.
#' @inheritParams listProjects
#'
#' @author Aaron Lun
#'
#' @return Character vector of relative paths of files associated with the versioned asset.
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up an upload. 
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' dir.create(file.path(src, "whee"))
#' write(file=file.path(src, "whee", "blah"), "stuff")
#' write(file=file.path(src, "whee2"), "more-stuff")
#' uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#'
#' # List files, with or without a prefix.
#' listFiles("test", "simple", "v1", registry=info$registry, url=info$url)
#' listFiles("test", "simple", "v1", registry=info$registry, prefix="whee")
#' listFiles("test", "simple", "v1", registry=info$registry, prefix="whee/")
#'
#' # Forcing remote access.
#' listFiles("test", "simple", "v1", registry=info$registry, url=info$url, forceRemote=TRUE)
#' 
#' @export
#' @import httr2
listFiles <- function(project, asset, version, registry, url, prefix=NULL, include..=TRUE, forceRemote=FALSE) {
    filter <- NULL
    if (!is.null(prefix)) {
        if (endsWith(prefix, "/")) {
            prefix <- substr(prefix, 1, nchar(prefix) - 1)
        } else {
            filter <- basename(prefix)
            prefix <- dirname(prefix)
            if (prefix == ".") {
                prefix <- NULL
            }
        }
    }

    if (!forceRemote && file.exists(registry)) {
        target <- file.path(registry, project, asset, version)
        if (!is.null(prefix)) {
            target <- file.path(target, prefix)
        }
        listing <- list.files(target, recursive=TRUE, all.files=include..)
    } else {
        target <- paste(project, asset, version, sep="/")
        if (!is.null(prefix)) {
            target <- paste0(target, "/", prefix) 
        }
        req <- request(paste0(url, "/list?path=", URLencode(target, reserved=TRUE), "&recursive=true"))
        req <- req_error(req, body = function(res) resp_body_json(res)$reason)
        res <- req_perform(req)

        listing <- unlist(resp_body_json(res))
        if (!include..) {
            listing <- listing[!startsWith(listing, "..") & !grepl("/\\.\\.", listing)]
        }
    }

    if (!is.null(filter)) {
        listing <- listing[startsWith(listing, filter)]
    }
    if (!is.null(prefix)) {
        listing <- paste0(prefix, "/", listing)
    }

    listing
}

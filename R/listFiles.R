#' List files for a version
#'
#' List the contents of a version of a project asset.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param prefix String containing the prefix for the path.
#' If provided, files are only listed if they have a relative path (i.e., inside the version subdirectory) that starts with this prefix.
#' If \code{NULL}, all files associated with this version are listed.
#' @param include.. Logical scalar indicating whether to list files with path components that start with \code{..}.
#' @param registry String containing a path to the registry.
#'
#' @author Aaron Lun
#'
#' @return Character vector of relative paths of files associated with the versioned asset.
#'
#' @examples
#' info <- startGobbler()
#'
#' # Mocking up an upload. 
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' dir.create(file.path(src, "whee"))
#' write(file=file.path(src, "whee", "blah"), "stuff")
#' write(file=file.path(src, "whee2"), "more-stuff")
#' removeAsset("test", "simple", info$staging) # clean out any existing entry
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)
#'
#' # List files, with or without a prefix.
#' listFiles("test", "simple", "v1", registry=info$registry)
#' listFiles("test", "simple", "v1", registry=info$registry, prefix="whee")
#' listFiles("test", "simple", "v1", registry=info$registry, prefix="whee/")
#' 
#' @export
listFiles <- function(project, asset, version, registry, prefix=NULL, include..=TRUE) {
    target <- file.path(registry, project, asset, version)

    filter <- NULL
    if (!is.null(prefix)) {
        if (endsWith(prefix, "/")) {
            target <- file.path(target, prefix)
        } else {
            filter <- basename(prefix)
            prefix <- dirname(prefix)
            if (prefix != ".") {
                prefix <- paste0(prefix, "/")
                target <- file.path(target, prefix)
            } else {
                prefix <- ""
            }
        }
    }

    listing <- list.files(target, recursive=TRUE, all.files=include..)

    if (!is.null(prefix)) {
        if (!is.null(filter)) {
            listing <- listing[startsWith(listing, filter)]
        }
        listing <- paste0(prefix, listing)
    }

    listing
}

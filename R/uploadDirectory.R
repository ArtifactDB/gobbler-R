#' Upload a directory's contents
#'
#' Upload a directory as a new versioned asset of a project in the registry.
#'
#' @param project String containing an existing project name.
#' @param asset String containing a new or existing asset name in \code{project}.
#' @param version String containing the name of a new version of \code{asset}.
#' @param directory String containing the path to a directory to be uploaded.
#' This should be inside \code{staging} for best performance, see \code{\link{allocateUploadDirectory}} for details.
#' @param probation Logical scalar indicating whether to upload a probational version.
#' @inheritParams createProject
#'
#' @return On success, \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#'
#' uploadDirectory(
#'     project="test", 
#'     asset="simple", 
#'     version="v1", 
#'     directory=src, 
#'     staging=info$staging,
#'     url=info$url
#' )
#'
#' @seealso
#' \code{\link{createProject}}, to create a new project for uploads.
#'
#' \code{\link{versionPath}}, to obtain the path to a versioned asset's contents inside the registry.
#'
#' \code{\link{fetchManifest}}, to obtain the manifest of the versioned asset's contents.
#'
#' @export
uploadDirectory <- function(project, asset, version, directory, staging, url, probation=FALSE) {
    directory <- normalizePath(directory)
    staging <- normalizePath(staging)

    if (dirname(directory) != staging) {
        new.dir <- allocateUploadDirectory(staging) 
        for (p in list.files(directory, recursive=TRUE)) {
            src <- file.path(directory, p)
            dest <- file.path(new.dir, p)
            dir.create(dirname(dest), recursive=TRUE, showWarnings=FALSE)

            src.link <- Sys.readlink(src)
            if (src.link == "") {
                .link_or_copy(src, dest, p)

            } else if (.is_absolute_or_local_link(src.link, p)) {
                if (!file.symlink(src.link, dest)) {
                    stop("failed to create a symlink for '", p, "' in the staging directory")
                }

            } else {
                full.src <- normalizePath(file.path(dirname(src), src.link))
                .link_or_copy(full.src, dest, p)
            }
        }
        directory <- new.dir
    }

    req <- list(
        `source` = basename(directory),
        project = project,
        asset = asset,
        version = version,
        on_probation = probation
    )

    dump_request(staging, url, "upload", req)

    invisible(NULL)
}

#' @importFrom utils head
.is_absolute_or_local_link <- function(target, link.path) {
    # Assuming Unix-style file paths, who uses a Windows HPC anyway?
    if (startsWith(target, "/")) {
        return(TRUE)
    }

    # Both 'target' and 'link.path' should be relative at this point, so the
    # idea is to check whether 'file.path(dirname(link.path), target)' is still
    # a child of 'dirname(link.path)'.
    pre.length <- length(strsplit(link.path, "/")[[1]]) - 1L
    post.fragments <- head(strsplit(target, "/")[[1]], -1L)

    for (x in post.fragments) {
        if (x == ".") {
            next
        } else if (x == "..") {
            pre.length <- pre.length - 1L
            if (pre.length < 0L) {
                return(FALSE)
            }
        } else {
            pre.length <- pre.length + 1L
        }
    }

    TRUE
}

.link_or_copy <- function(src, dest, p) {
    if (!suppressWarnings(file.link(src, dest)) && !file.copy(src, dest)) {
        stop("failed to link or copy '", p, "' to the staging directory")
    }
}

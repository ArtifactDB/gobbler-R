#' Upload a directory's contents
#'
#' Upload a directory as a new versioned asset of a project in the Gobbler registry.
#'
#' @param project String containing an existing project name.
#' @param asset String containing a new or existing asset name in \code{project}.
#' @param version String containing the name of a new version of \code{asset}.
#' @param directory String containing the path to a directory to be uploaded.
#' This should be inside \code{staging} for best performance, typically using the directory allocated by \code{\link{allocateUploadDirectory}}.
#' Otherwise, the contents of \code{directory} will be copied to \code{staging} prior to upload.
#' @param probation Logical scalar indicating whether to upload a probational version.
#' @param consume Logical scalar indicating whether the contents of \code{directory} can be consumed by the upload process.
#' If \code{TRUE}, the Gobbler will attempt to move files from \code{directory} into the registry.
#' Otherwise, the contents of \code{directory} will not be modified by the upload.
#' Defaults to \code{TRUE} if the contents of \code{directory} need to be copied to \code{staging}.
#' @param ignore.. Logical scalar indicating whether to skip dotfiles in \code{directory} during upload.
#' @param spoof String containing the name of a user on whose behalf this request is being made.
#' This should only be used if the Gobbler service allows spoofing by the current user. 
#' If \code{NULL}, no spoofing is performed.
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
uploadDirectory <- function(project, asset, version, directory, staging, url, probation=FALSE, consume=NULL, ignore..=TRUE, spoof=NULL) {
    # Normalizing them so that they're comparable, in order to figure out whether 'directory' lies inside 'staging'.
    directory <- normalizePath(directory)
    staging <- normalizePath(staging)

    in.staging <- (function() {
        while (nchar(staging) < nchar(directory)) {
            directory <- dirname(directory)
            if (staging == directory) {
                return(TRUE)
            }
        }
        return(FALSE)
    })()

    if (!in.staging) {
        new.dir <- allocateUploadDirectory(staging) 
        on.exit(unlink(new.dir, recursive=TRUE), add=TRUE, after=FALSE) # cleaning up after the request is done.

        for (p in list.files(directory, recursive=TRUE, include.dirs=TRUE, all.files=TRUE)) {
            src <- file.path(directory, p)
            dest <- file.path(new.dir, p)
            if (file.info(src)$isdir) {
                stopifnot(dir.create(dest, showWarnings=FALSE))
                next
            }

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

    if (is.null(consume)) {
        # If we had to copy it over, we're entitled to consume it if the user provides no further instruction.
        consume <- !in.staging
    }

    req <- list(
        `source` = basename(directory),
        project = project,
        asset = asset,
        version = version,
        on_probation = probation,
        consume = consume,
        ignore_dot = ignore.. 
    )

    dump_request(staging, url, "upload", req, spoof=spoof)

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

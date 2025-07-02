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
#' @param concurrent Integer specifying the number of concurrent downloads.
#' Only used if files need to be copied from \code{directory} to \code{staging}.
#' @inheritParams createProject
#'
#' @details
#' If \code{directory} is not inside \code{staging}, a new staging subdirectory is allocated by \code{\link{allocateUploadDirectory}}.
#' The contents of \code{directory} are then copied to the new subdirectory, preserving all symbolic links, dotfiles and empty directories.
#' If \code{consume=NULL}, it is set to \code{TRUE} as the copied contents will no longer be used. 
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
uploadDirectory <- function(project, asset, version, directory, staging, url, probation=FALSE, consume=NULL, ignore..=TRUE, spoof=NULL, concurrent=1L) {
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
        to.copy <- character(0)

        for (p in list.files(directory, recursive=TRUE, include.dirs=TRUE, all.files=TRUE)) {
            src <- file.path(directory, p)
            dest <- file.path(new.dir, p)
            if (file.info(src)$isdir) {
                stopifnot(dir.create(dest, showWarnings=FALSE))
                next
            }

            src.link <- Sys.readlink(src)
            if (src.link == "") {
                to.copy <- c(to.copy, list(c(src, dest)))
            } else {
                if (!file.symlink(src.link, dest)) {
                    stop("failed to create a symlink '", dest, "' to '", src.link, "'")
                }
            }
        }

        if (concurrent == 1L) {
            lapply(to.copy, copy_file)
        } else {
            cl <- parallel::makeCluster(concurrent)
            on.exit(parallel::stopCluster(cl), add=TRUE, after=FALSE)
            parallel::parLapply(cl, to.copy, copy_file)
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

#' @importFrom digest digest
copy_file <- function(job) {
    src <- job[[1]]
    dest <- job[[2]]
    if (!file.copy(src, dest)) {
        stop("failed to copy '", src, "' to '", dest, "'")
    }

    old.size <- file.info(src)$size
    new.size <- file.info(dest)$size
    if (old.size != new.size) {
        stop("mismatching sizes for '", src, "' and '", dest, "'")
    }

    old.md5 <- digest(file=src)
    new.md5 <- digest(file=dest)
    if (old.md5 != new.md5) {
        stop("mismatching MD5 checksums for '", src, "' and '", dest, "'")
    }
}

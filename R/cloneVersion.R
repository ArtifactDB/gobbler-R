#' Clone a version's directory structure
#'
#' Clone the directory structure for a versioned asset into a separate location.
#' This is typically used to prepare a new version for a lightweight upload.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param destination String containing a path to a destination directory at which to create the clone.
#' @param registry String containing the path to the registry.
#'
#' @return The directory structure of the specified version is cloned to \code{destination},
#' and a \code{NULL} is invisibly returned.
#'
#' @details
#' Cloning involves creating a directory at \code{destination} that has the same structure as that of the specified project-asset-version.
#' All files in the version are represented as symlinks from \code{destination} to the corresponding file in the \code{registry}. 
#' The idea is that, when \code{destination} is used in \code{\link{uploadDirectory}}, the symlinks are converted into upload links.
#' This allows users to create new versions very cheaply as duplicate files are not uploaded to/stored in the backend.
#'
#' Users can more-or-less do whatever they want inside the cloned \code{destination}, 
#' but the symlink targets should be read-only as they refer to immutable files in the \code{registry}.
#' If a file in \code{destination} needs to be modified, the symlink should be deleted and replaced with a new file.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{allocateUploadDirectory}}, to obtain a possible value for \code{destination}.
#' 
#' \code{\link{uploadDirectory}}, to prepare an upload based on the directory contents.
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
#'
#' # Creating a clone.
#' dest <- tempfile()
#' out <- cloneVersion("test", "simple", "v1", dest, registry=info$registry)
#' list.files(dest, recursive=TRUE)
#' Sys.readlink(file.path(dest, "foo"))
#' readLines(file.path(dest, "foo"))
#'
#' # Files should be replaced rather than modified via the symlink:
#' existing <- file.path(dest, "foo")
#' unlink(existing) # Deleting the symlink...
#' write(file=existing, "YAY") # ... and writing a replacement file.
#'
#' @export
cloneVersion <- function(project, asset, version, destination, registry) {
    target <- file.path(registry, project, asset, version)
    listing <- fetchManifest(project, asset, version, registry=registry)
    dir.create(destination, showWarnings=FALSE)

    for (x in names(listing)) {
        dpath <- file.path(destination, x)
        dir.create(dirname(dpath), showWarnings=FALSE)
        src <- file.path(target, x)
        if (!file.symlink(src, dpath)) {
            stop("failed to create a symlink from '", src, "' to '", dpath, "'")
        }
    }

    invisible(NULL)
}

#' Validate a versioned asset
#'
#' Check the validity of a version of an asset of a project from the registry.
#' This compares the size, MD5 checksums and link information in the internal \code{..manifest} and \code{..links} files to the contents of the directory in the registry.
#'
#' @param project String containing the project to remove.
#' @param asset String containing the asset to remove.
#' @param version String containing the version of the asset to remove.
#' @inheritParams createProject
#'
#' @return \code{NULL} is invisibly returned if the version is valid, otherwise an error is thrown.
#'
#' @author Aaron Lun
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up a version if it doesn't already exist.
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#'
#' # Validation has no issue.
#' validateVersion("test", "simple", "v1", staging=info$staging, url=info$url)
#'
#' # Let's add a new file directly to the directory and rerun the validation.
#' write(file=file.path(info$registry, "test", "simple", "v1", "whee"), "stuff")
#' try(validateVersion("test", "simple", "v1", staging=info$staging, url=info$url))
#'
#' @export
validateVersion <- function(project, asset, version, staging, url) {
    dump_request(staging, url, "validate_version", list(project=project, asset=asset, version=version))
    invisible(NULL)
}


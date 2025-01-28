#' Reindex a versioned asset
#'
#' Reindex a version of an asset of a project from the registry.
#' This regenerates all of the internal \code{..manifest} and \code{..links} files.
#'
#' @param project String containing the project to remove.
#' @param asset String containing the asset to remove.
#' @param version String containing the version of the asset to remove.
#' @inheritParams createProject
#'
#' @return \code{NULL} is invisibly returned if the version was successfully reindexed.
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
#' fetchManifest("test", "simple", "v1", registry=info$registry, url=info$url)
#'
#' # Let's add a new file directly to the directory.
#' write(file=file.path(info$registry, "test", "simple", "v1", "whee"), "stuff")
#'
#' # And reindexing the version.
#' reindexVersion("test", "simple", "v1", staging=info$staging, url=info$url)
#' fetchManifest("test", "simple", "v1", registry=info$registry, url=info$url)
#'
#' @export
reindexVersion <- function(project, asset, version, staging, url) {
    dump_request(staging, url, "reindex_version", list(project=project, asset=asset, version=version))
    invisible(NULL)
}

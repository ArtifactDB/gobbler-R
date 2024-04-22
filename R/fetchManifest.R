#' Fetch version manifest 
#'
#' Fetch the manifest for a version of an asset of a project.
#' This will call the REST API if the caller is not on the same filesystem as the registry.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @inheritParams fetchDirectory
#'
#' @author Aaron Lun
#' 
#' @return List containing the manifest for this version.
#' Each element is named after the relative path of a file in this version.
#' The value of each element is another list with the following fields:
#' \itemize{
#' \item \code{size}, an integer specifying the size of the file in bytes.
#' \item \code{md5sum}, a string containing the hex-encoded MD5 checksum of the file.
#' \item \code{link} (optional): a list specifying the link destination for a file. 
#' This contains the strings \code{project}, \code{asset}, \code{version} and \code{path}.
#' }
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
#' uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#'
#' # Obtaining the manifest for this version.
#' fetchManifest("test", "simple", "v1", registry=info$registry, url=info$url)
#' 
#' # Force remote access.
#' fetchManifest(
#'     "test", 
#'     "simple", 
#'     "v1", 
#'     registry=info$registry, 
#'     url=info$url, 
#'     forceRemote=TRUE
#' )
#' @export
#' @importFrom jsonlite fromJSON
fetchManifest <- function(project, asset, version, registry, url, cache=NULL, forceRemote=FALSE, overwrite=FALSE) {
    if (!forceRemote && file.exists(registry)) {
        path <- file.path(registry, project, asset, version, "..manifest")
    } else {
        cache <- local_registry(cache, url)
        path <- acquire_file(cache, paste(project, asset, version, sep="/"), "..manifest", url=url, overwrite=overwrite)
    }
    fromJSON(path, simplifyVector=FALSE)
}

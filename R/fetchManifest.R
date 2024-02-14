#' Fetch version manifest 
#'
#' Fetch the manifest for a version of an asset of a project.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param registry String containing the path to the registry.
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
#' fetchManifest("test-R", "basic", "v1")
#' 
#' @export
#' @importFrom jsonlite fromJSON
fetchManifest <- function(project, asset, version, registry) {
    fromJSON(file.path(registry, project, asset, version, "..manifest"), simplifyVector=FALSE)
}

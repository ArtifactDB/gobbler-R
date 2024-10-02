#' Unpack a path to its project-asset-version combination
#'
#' Unpack a Gobbler path to its combination of project, asset, version, and (optionally) path,
#' for easier use in the various \pkg{gobbler} functions.
#'
#' @param path String containing a relative path within the Gobbler registry. 
#'
#' @return List containing \code{project}, \code{asset}, \code{version} and \code{path}.
#' All are strings, except for \code{path}, which may be \code{NULL}.
#'
#' @author Aaron Lun
#'
#' @examples
#' unpackPath("project/asset/version/path")
#' unpackPath("project/asset/version")
#'
#' @export
#' @importFrom utils tail
unpackPath <- function(path) {
    components <- strsplit(path, "/")[[1]]
    stopifnot(length(components) >= 3L)
    if (length(components) == 3L || components[4] == "") {
        path <- NULL
    } else {
        path <- paste(tail(components, -3), collapse="/")
    }
    list(project=components[1], asset=components[2], version=components[3], path=path)
}

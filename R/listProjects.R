#' List all projects
#'
#' List all projects in the registry.
#' This will call the REST API if the caller is not on the same filesystem as the registry.
#'
#' @param registry String containing a path to the registry.
#' @param url String containing the URL to the Gobbler REST API.
#' Only used for remote access.
#' @param forceRemote Logical scalar indicating whether to force remote access via the API,
#' even if \code{registry} is on the same filesystem as the caller.
#'
#' @author Aaron Lun
#'
#' @return Character vector of project names.
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # clean out any existing entries
#' removeProject("more-test", info$staging, url=info$url)
#' removeProject("even-more-test", info$staging, url=info$url)
#'
#' # Now mocking up the creation of some projects.
#' createProject("test", info$staging, url=info$url)
#' createProject("more-test", info$staging, url=info$url)
#' createProject("even-more-test", info$staging, url=info$url)
#'
#' # Listing out the projects.
#' listProjects(info$registry, info$url)
#'
#' # Forcing remote access.
#' listProjects(info$registry, info$url, forceRemote=TRUE)
#'
#' @export
listProjects <- function(registry, url, forceRemote = FALSE) {
    out <- list_registry_directories(".", registry, url, forceRemote)
    out[!startsWith(out, "..")] # remove the ..logs directory.
}

#' @import httr2
list_registry_directories <- function(path, registry, url, forceRemote) {
    if (!forceRemote && file.exists(registry)) {
        list.dirs(file.path(registry, path), full.names=FALSE, recursive=FALSE)
    } else {
        url <- paste0(url, "/list")
        if (path != ".") {
            url <- paste0(url, "?path=", URLencode(path, reserved=TRUE))
        }
        req <- request(url)
        req <- handle_error(req)
        res <- req_perform(req)
        listing <- unlist(resp_body_json(res))
        dirs <- listing[endsWith(listing, "/")]
        substr(dirs, 1, nchar(dirs) - 1L)
    }
}

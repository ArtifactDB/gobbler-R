#' Fetch the latest version
#'
#' Fetch the latest version of a project's asset.
#' This will call the REST API if the caller is not on the same filesystem as the registry.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @inheritParams listProjects
#'
#' @return String containing the latest version of the asset. 
#' This may also be \code{NULL} if the asset has no (non-probational) versions.
#'
#' @author Aaron Lun
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url) 
#'
#' # Mocking up a few uploads.
#' src <- allocateUploadDirectory(info$staging)
#' uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#' uploadDirectory("test", "simple", "v2", src, staging=info$staging, url=info$url)
#'
#' # Obtaining the latest version of this asset.
#' fetchLatest("test", "simple", registry=info$registry, url=info$url)
#'
#' # Forcing remote access.
#' fetchLatest("test", "simple", registry=info$registry, url=info$url, forceRemote=TRUE)
#'
#' @seealso
#' \code{\link{refreshLatest}}, to refresh the latest version.
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @import httr2
fetchLatest <- function(project, asset, registry, url, forceRemote=FALSE) {
    if (file.exists(registry) && !forceRemote) {
        proposed <- file.path(registry, project, asset, "..latest")
        if (!file.exists(proposed)) {
            return(NULL)
        } else {
            vers <- fromJSON(proposed, simplifyVector=FALSE)
            return(vers$version)
        }
    }

    tryCatch(
         {
            req <- request(paste0(url, "/fetch/", paste(project, asset, "..latest", sep="/")))
            resp <- req_perform(req)
            body <- resp_body_string(resp)
            vers <- fromJSON(body, simplifyVector=FALSE)
            vers$version
        },
        httr2_http_404 = function(e) NULL
    )
}

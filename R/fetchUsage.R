#' Fetch project usage details
#'
#' Fetch the quota usage for a project.
#'
#' @param project String containing the project name.
#' @inheritParams listProjects
#'
#' @return Numeric scalar specifying the quota usage for the project, in bytes.
#'
#' @author Aaron Lun
#'
#' @examples
#' info <- startGobbler()
#' removeProject("test", info$staging, url=info$url) # start with a clean slate.
#' createProject("test", info$staging, url=info$url)
#'
#' # Mocking up an upload.
#' src <- allocateUploadDirectory(info$staging)
#' write(file=file.path(src, "foo"), "BAR")
#' write(file=file.path(src, "whee"), "stuff")
#' uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
#'
#' # Obtaining the project usage.
#' fetchUsage("test", registry=info$registry)
#'
#' @seealso
#' \code{\link{refreshUsage}}, to recompute the used quota.
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @import httr2
fetchUsage <- function(project, registry, url, forceRemote=FALSE) {
    if (file.exists(registry) && !forceRemote) {
        content <- file.path(registry, project, "..usage")
    } else {
        req <- request(paste0(url, "/fetch/", paste(project, "..usage", sep="/")))
        resp <- req_perform(req)
        content <- resp_body_string(resp)
    }

    out <- fromJSON(content, simplifyVector=FALSE)
    out$total
}

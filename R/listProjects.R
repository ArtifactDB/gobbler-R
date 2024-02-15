#' List all projects
#'
#' List all projects in the registry.
#'
#' @param registry String containing a path to the registry.
#'
#' @author Aaron Lun
#'
#' @return Character vector of project names.
#'
#' @examples
#' info <- startGobbler()
#'
#' # Mocking up a few uploads.
#' src <- allocateUploadDirectory(info$staging)
#' for (proj in c("test", "more-tests", "even-more-tests")) {
#'     removeProject(proj, info$staging) # clean out any existing entry
#'     uploadDirectory(proj, "simple", "v1", src, staging=info$staging)
#' }
#'
#' # Listing out the projects.
#' listProjects(info$registry)
#'
#' @export
listProjects <- function(registry) {
    list.files(registry)
}

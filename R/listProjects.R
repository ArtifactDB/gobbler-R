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
#' # Mocking up an upload. 
#' info <- startGobbler()
#' src <- allocateUploadDirectory(info$staging)
#' res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)
#' res <- uploadDirectory("more-tests", "simple", "v1", src, staging=info$staging)
#' res <- uploadDirectory("even-more-tests", "simple", "v1", src, staging=info$staging)
#' stopGobbler(info, keep.dir=TRUE)
#'
#' # Listing out the projects.
#' listProjects(info$registry)
#'
#' @export
listProjects <- function(registry) {
    list.files(registry)
}

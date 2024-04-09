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
#' removeProject("test", info$staging) # clean out any existing entries
#' removeProject("more-test", info$staging)
#' removeProject("even-more-test", info$staging)
#'    
#' # Now mocking up the creation of some projects.
#' createProject("test", info$staging)
#' createProject("more-test", info$staging)
#' createProject("even-more-test", info$staging)
#'
#' # Listing out the projects.
#' listProjects(info$registry)
#'
#' @export
listProjects <- function(registry) {
    list.files(registry)
}

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
#' if (interactive()) {
#'     listProjects()
#' }
#' 
#' @export
listProjects <- function(registry) {
    list.files(registry)
}

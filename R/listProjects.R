#' List all projects
#'
#' List all projects in the gypsum backent.
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
listProjects <- function(registry=registryPath()) {
    list.files(registry)
}

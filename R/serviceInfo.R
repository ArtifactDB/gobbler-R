#' Gobbler service information
#'
#' Get information about the Gobbler service, namely the locations of the staging directory and registry.
#' 
#' @inheritParams createProject
#'
#' @return List containing the location of the staging and registry directories.
#'
#' @author Aaron Lun
#'
#' @examples
#' info <- startGobbler()
#' serviceInfo(info$url)
#'
#' @export
#' @import httr2
serviceInfo <- function(url) {
    req <- request(paste0(url, "/info"))
    req <- handle_error(req)
    res <- req_perform(req)
    resp_body_json(res)
}

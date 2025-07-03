#' Allocate a directory for upload
#'
#' Create a new subdirectory in the staging directory to store files for an upload to the Gobbler registry.
#' If the to-be-uploaded files are saved here, users can avoid an extra copying step in \code{\link{uploadDirectory}}.
#'
#' @param staging String containing the path to the staging directory.
#' @param create Logical scalar specifying whether to actually create the subdirectory.
#'
#' @return String containing a path to a newly created subdirectory within \code{staging}.
#' This (or its subdirectories) are typically used as the \code{directory} argument in \code{\link{uploadDirectory}}.
#'
#' If \code{create=FALSE}, a name will be chosen but the subdirectory itself will not yet be created.
#'
#' @author Aaron Lun
#'
#' @examples
#' staging <- tempfile()
#' dir.create(staging)
#' allocateUploadDirectory(staging)
#'
#' @export
allocateUploadDirectory <- function(staging, create=TRUE) {
    sinfo <- Sys.info()
    pattern <- paste0("upload-", sinfo[["user"]], "-")
    tmp <- tempfile(tmpdir=staging, pattern=pattern)
    if (create) {
        if (!dir.create(tmp, showWarnings=FALSE)) {
            stop("failed to allocate an upload directory")
        }
    }
    tmp
}

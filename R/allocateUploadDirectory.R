#' Allocate a directory for upload
#'
#' Create a new subdirectory in the staging directory to store files for an upload via \code{\link{uploadDirectory}}.
#'
#' @param staging String containing the path to the staging directory.
#'
#' @return String containing a path to a newly created subdirectory within \code{staging}.
#'
#' @author Aaron Lun
#'
#' @examples
#' staging <- tempfile()
#' dir.create(staging)
#' allocateUploadDirectory(staging)
#' 
#' @export
allocateUploadDirectory <- function(staging) {
    sinfo <- Sys.info()
    usernames <- unlist(sinfo[c("user", "login", "effective_user")])

    # Ensure that we create a unique path inside the staging
    # directory that is actually owned by the current user.
    while (TRUE) {
        tmp <- tempfile(tmpdir=staging, pattern="upload-")
        dir.create(tmp, showWarnings=FALSE)
        if (file.info(tmp)$uname %in% usernames) {
            return(tmp)
        }
    }

    stop("failed to allocate an upload directory")
}

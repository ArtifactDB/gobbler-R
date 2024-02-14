#' Start and stop a Gobbler service
#'
#' This is intended for examples and tests.
#' Real Gobbler deployments should operate outside of R.
#'
#' @param staging String containing a path to a staging directory.
#' @param registry String containing a path to a registry.
#' @param info List created by \code{startGobbler}.
#' @param keep.dir Logical scalar indicating whether the staging directory and regisry should be retained.
#'
#' @return \code{startGobbler} starts a Gobbler service and returns a list including the locations of the staging directory and registry. 
#'
#' \code{stopGobbler} stops the Gobbler service, deletes the relevant directories if \code{keep.dir=FALSE}, and returns \code{NULL} invisibly.
#'
#' @seealso
#' \url{https://github.com/ArtifactDB/gobbler}, for source code and binaries to build and run a Gobbler service.
#' 
#' @examples
#' info <- startGobbler()
#' stopGobbler(info)
#' 
#' @export
startGobbler <- function(staging = tempfile(), registry = tempfile()) {
    dir.create(staging, showWarnings=FALSE)
    dir.create(registry, showWarnings=FALSE)
    cache <- tools::R_user_dir("gobbler", "cache")

    sinfo <- Sys.info()
    sysname <- sinfo["sysname"]
    if (sysname == "Darwin") {
        os <- "darwin"
    } else if (sysname == "Linux") {
        os <- "linux"
    } else {
        stop("unsupported operating system '", sysname, "'")
    }

    sysmachine <- sinfo["machine"]
    if (sysmachine == "arm64") {
        arch <- "arm64"
    } else if (sysmachine == "x86_64") {
        arch <- "amd64"
    } else {
        stop("unsupported architecture '", sysmachine, "'")
    }

    desired <- sprintf("gobbler-%s-%s", os, arch)
    exe <- file.path(cache, desired)
    if (!file.exists(exe)) {
        url <- paste0("https://github.com/ArtifactDB/gobbler/releases/download/latest/", desired)
        tmp <- tempfile()
        download.file(url, tmp)
        Sys.chmod(tmp, "0755")
        dir.create(cache, showWarnings=FALSE)
        file.rename(tmp, exe)
    }

    self <- sinfo["login"]
    script <- system.file("scripts", "deploy.sh", package="gobbler", mustWork=TRUE)
    pid <- system2(script, c(exe, staging, registry, self), stdout=TRUE) 
    list(process=pid, staging = staging, registry = registry)
}

#' @export
#' @rdname startGobbler
stopGobbler <- function(info, keep.dir=FALSE) {
    system2("kill", c("-9", info$process))
    if (!keep.dir) {
        unlink(info$staging, recursive=TRUE) 
        unlink(info$registry, recursive=TRUE) 
    }
    invisible(NULL)
}

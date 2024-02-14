#' Start and stop a Gobbler service
#'
#' This sets up a single Gobbler service for the entire R session, is intended for examples and tests.
#' Real Gobbler deployments should operate outside of R.
#'
#' @param staging String containing a path to a staging directory.
#' Ignored if the service is already running.
#' @param registry String containing a path to a registry.
#' Ignored if the service is already running.
#'
#' @return For \code{startGobbler}, a list indicating whether a new service was set up, plus the locations of the staging directory and registry. 
#'
#' For \code{stopGobbler}, any existing service is shut down, and \code{NULL} is invisibly returned.
#'
#' @seealso
#' \url{https://github.com/ArtifactDB/gobbler}, for source code and binaries to build and run a Gobbler service.
#' 
#' @examples
#' startGobbler()
#' 
#' @export
startGobbler <- function(staging=tempfile(), registry=tempfile()) {
    if (!is.null(running$active)) {
        return(list(new=FALSE, staging=running$staging, registry=running$registry))
    }

    dir.create(staging)
    dir.create(registry)
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

    process <- new.env()
    process$pid <- pid
    reg.finalizer(process, kill_gobbler, onexit=TRUE)

    running$active <- process
    running$staging <- staging 
    running$registry <- registry
    list(new=TRUE, staging=staging, registry=registry)
}

kill_gobbler <- function(process) {
    if (!is.null(process$pid)) {
        system2("kill", c("-9", process$pid))
    }
}

running <- new.env()

#' @export
#' @rdname startGobbler
stopGobbler <- function() {
    if (!is.null(running$active)) {
        kill_gobbler(running$active)
        running$active$pid <- NULL
        running$active <- NULL
        running$staging <- NULL
        running$registry <- NULL
    }
    invisible(NULL)
}

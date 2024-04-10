#' Start and stop a Gobbler service
#'
#' This sets up a single Gobbler service for the entire R session, is intended for examples and tests.
#' Real Gobbler deployments should operate outside of R.
#'
#' @param staging String containing a path to a staging directory.
#' Ignored if the service is already running.
#' @param registry String containing a path to a registry.
#' Ignored if the service is already running.
#' @param port Integer specifying the port to use for hosting the service.
#' If \code{NULL}, a free port is randomly selected.
#' @param wait Integer specifying the number of seconds to wait for service initialization.
#'
#' @return For \code{startGobbler}, a list containing:
#' \itemize{
#' \item \code{new}, a logical scalar indicating whether a new service was set up.
#' \item \code{staging}, string containing the location of the staging directory.
#' \item \code{registry}, string containing the location of the registry.
#' \item \code{port}, integer containing the port.
#' \item \code{url}, string containing the URL of the REST API.
#' }
#'
#' For \code{stopGobbler}, any existing service is shut down, and \code{NULL} is invisibly returned.
#'
#' @seealso
#' \url{https://github.com/ArtifactDB/gobbler}, for source code and binaries to build and run a Gobbler service.
#' 
#' @examples
#' startGobbler()
#' startGobbler() # repeated calls just re-use the same instance.
#'
#' stopGobbler()
#' startGobbler() # reinitializes a new instance.
#' 
#' @export
#' @importFrom utils download.file
startGobbler <- function(staging=tempfile(), registry=tempfile(), port = NULL, wait = 1) {
    if (!is.null(running$active)) {
        return(list(new=FALSE, staging=running$staging, registry=running$registry, port=running$port, url=assemble_url(running$port)))
    }

    dir.create(staging)
    dir.create(registry)

    # This should really be the cache directory, but our HPC deployment does
    # naughty things with mounting .cache, so we'll just use data instead. 
    cache <- tools::R_user_dir("gobbler", "data")

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

    version <- "0.3.0"
    binary <- sprintf("gobbler-%s-%s", os, arch)
    desired <- paste0(binary, "-", version)
    exe <- file.path(cache, desired)

    if (!file.exists(exe)) {
        url <- paste0("https://github.com/ArtifactDB/gobbler/releases/download/", version,  "/", binary)
        tmp <- tempfile()
        if (download.file(url, tmp)) {
            stop("failed to download the Gobbler binary")
        }
        Sys.chmod(tmp, "0755")

        # Using a write-and-rename paradigm to provide some atomicity. Note
        # that renaming doesn't work across different filesystems so in that
        # case we just fall back to copying.
        dir.create(cache, recursive=TRUE, showWarnings=FALSE)
        if (!file.rename(tmp, exe) && !file.copy(tmp, exe)) { 
            stop("cannot transfer file from '", tmp, "' to '", exe, "'")
        }
    }

    if (is.null(port)) {
        port <- choose_port()
    }

    self <- sinfo["user"]
    script <- system.file("scripts", "deploy.sh", package="gobbler", mustWork=TRUE)
    pid <- system2(script, c(shQuote(exe), shQuote(staging), shQuote(registry), shQuote(self), shQuote(port)), stdout=TRUE) 
    Sys.sleep(wait)

    process <- new.env()
    process$pid <- pid
    reg.finalizer(process, kill_gobbler, onexit=TRUE)

    running$active <- process
    running$staging <- staging 
    running$registry <- registry
    running$port <- port
    list(new=TRUE, staging=staging, registry=registry, port=port, url=assemble_url(port))
}

#' @import methods
choose_port <- function() {
    # Based on the same logic as shiny::runApp. 
    choices <- 3000:8000
    choices <- setdiff(choices, c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)) 

    for (i in 1:10) {
        port <- sample(choices, 1)
        soc <- try(serverSocket(port), silent=TRUE)
        if (!is(soc, "try-error")) {
            close(soc)
            return(port)
        }
    }
}

assemble_url <- function(port) { 
    paste0("http://0.0.0.0:", port)
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
        running$port <- NULL
    }
    invisible(NULL)
}

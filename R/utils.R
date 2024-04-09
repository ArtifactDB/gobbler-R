cast_datetime <- function(x) {
    zend <- endsWith(x, "Z")

    if (any(zend)) {
        # strptime doesn't know how to handle 'Z' offsets.
        xz <- x[zend]
        x[zend] <- sprintf("%s+0000", substr(xz, 1L, nchar(xz)-1L))
    }

    if (!all(zend)) {
        # Remove colon in the timezone, which confuses as.POSIXct().
        x[!zend] <- sub(":([0-9]{2})$", "\\1", x[!zend])
    }

    # Remove fractional seconds.
    x <- sub("\\.[0-9]+", "", x)

    as.POSIXct(x, format="%Y-%m-%dT%H:%M:%S%z")
}

sanitize_uploaders <- function(uploaders) {
    for (i in seq_along(uploaders)) {
        current <- uploaders[[i]]
        if ("until" %in% names(current)) {
            uploaders[[i]]$until <- sub("([0-9]{2})$", ":\\1", strftime(current$until, "%Y-%m-%dT%H:%M:%S%z"))
        }
    }
    uploaders
}

#' @importFrom jsonlite toJSON
dump_request <- function(staging, action, payload) {
    if (is.null(payload)) {
        as_str <- character(0)
    } else {
        as_str <- toJSON(payload, auto_unbox=TRUE) 
    }

    temp <- tempfile(tmpdir=staging, pattern=".")
    write(file=temp, x=as_str)

    actual <- tempfile(tmpdir=staging, pattern=paste0("request-", action, "-"))
    file.rename(temp, actual)
    basename(actual)
}

#' @importFrom jsonlite fromJSON
wait_response <- function(staging, request_name, error=TRUE, timeout=10) {
    target <- file.path(staging, "responses", request_name)

    start <- Sys.time()
    while (!file.exists(target)) {
        if (Sys.time() - start > timeout) {
            if (error) {
                stop("timed out waiting for a response to '", request_name, "'")
            } else {
                return(FALSE)
            }
        }
        Sys.sleep(0.2)
    }

    # If we have a response, we clean out the request file to declutter the
    # staging directory. This also reduces the chance for conflicts in the
    # temporary files, which become more likely as we run out of names.
    unlink(file.path(staging, request_name))

    if (!error) {
        return(TRUE)
    }

    output <- fromJSON(target, simplifyVector=FALSE)
    if (output$status == "FAILED") {
        stop(output$reason)
    }

    output
}

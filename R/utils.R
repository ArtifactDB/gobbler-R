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
#' @import httr2
dump_request <- function(staging, url, action, payload) {
    if (is.null(payload)) {
        as_str <- character(0)
    } else {
        as_str <- toJSON(payload, auto_unbox=TRUE) 
    }

    actual <- tempfile(tmpdir=staging, pattern=paste0("request-", action, "-"))
    write(file=actual, x=as_str)
    Sys.sleep(0.1)

    req <- request(paste0(url, "/new/", basename(actual)))
    req <- req_method(req, "POST")
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)
    res <- req_perform(req)
    resp_body_json(res)
}

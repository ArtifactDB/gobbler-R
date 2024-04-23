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

handle_error <- function(req) {
    req_error(req, body = function(res) {
        ct <- resp_content_type(res)
        if (ct == "application/json") {
            resp_body_json(res)$reason
        } else if (ct == "text/plain") {
            resp_body_string(res)
        } else {
            NULL
        }
    })
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
    on.exit(unlink(actual), add=TRUE, after=FALSE) # cleaning up the file once the request is done.

    wait <- getOption("gobbler_request_wait", 0.1)
    Sys.sleep(wait) # some leeway to allow network drives to sync.

    req <- request(paste0(url, "/new/", basename(actual)))
    req <- req_method(req, "POST")
    req <- handle_error(req)
    res <- req_perform(req)
    resp_body_json(res)
}

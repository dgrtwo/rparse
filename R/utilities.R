#' assert that a class has an object ID
#'
#' @param x A Parse object
assert_object_id <- function(x) {
    if (is.null(x$objectId)) {
        stop("Object has no ID")
    }
}


#' turn multiple columns into a batch set of requests
#'
#' @param method HTTP method such as GET, POST, or DELETE
#' @param path path within Parse API, or vector of paths
#' @param body data frame of body for each request in batch
batch_body <- function(method, path, body = NULL) {
    # sorta hacky; fix this function
    path <- paste0("/1/", path)
    if (!is.null(body)) {
        b <- lapply(split(body, seq_len(nrow(body))), as.list)
        req <- dplyr::data_frame(body = b)
        req$method <- method
        req$path <- path
        req <- apply(req, 1, function(d) {
            # have to unpack list columns so they're just an object
            for (colname in names(d$body)) {
                if (is(d$body[[colname]], "list") && length(d$body[[colname]]) == 1) {
                    d$body[[colname]] <- d$body[[colname]][[1]]
                    # if it's an object, make it a pointer
                    if (is(d$body[[colname]], "parse_object")) {
                        d$body[[colname]] <- as.parse_pointer(d$body[[colname]])
                    }
                }
            }
            list(method = d$method, path = d$path, body = d$body)
        })
    }
    else {
        req <- apply(as.data.frame(cbind(method = method, path = path)), 1, as.list)
    }

    names(req) <- NULL
    list(requests = req)
}

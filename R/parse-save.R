#' save a parse object or batch
#'
#' @param x object to be saved
#' @param ... extra arguments
#'
#' @export
parse_save <- function(x, ...) UseMethod("parse_save")


#' prepare an object to be saved or queried to Parse, by converting items to pointers
#'
#' @param x object to be converted
#' @param ... extra arguments
#'
#' @export
convert <- function(x, ...) UseMethod("convert")


#' prepare a Parse object to be saved or queried, by converting items to pointers
#'
#' @param x parse_object to be converted
#' @param ... extra arguments
#'
#' @export
convert.parse_object <- function(x, ...) {
    ret <- lapply(x, as.parse_pointer)
    ret
}


#' save a parse object
#'
#' @param x parse_object to be saved
#' @param ... extra arguments to be passed on to parse_api_POST
#'
#' @export
parse_save.parse_object <- function(x, ...) {
    if (!is.null(x$objectId)) {
        body <- convert(x)
        body$objectId <- NULL
        body$createdAt <- NULL
        body$updatedAt <- NULL
        ret <- parse_api_PUT(file.path("classes", parse_class(x), x$objectId),
                         body = body, ...)
    } else {
        ret <- parse_api_POST(file.path("classes", parse_class(x)),
                          body = convert(x), ...)
    }
    ret <- modifyList(x, ret)
    as.parse_object(ret, parse_class(x))
}


#' create a Parse object from a list
#'
#' @param x list to be saved
#' @param class_name class to save the object to
#' @param ... extra arguments to pass on to parse_api_POST
#'
#' @export
parse_save.list <- function(x, class_name, ...) {
    parse_save(as.parse_object(x, class_name))
}


#' create a batch of Parse objects
#'
#' @param x parse_batch to be saved
#' @param ... extra arguments to pass on to parse_api_POST
#'
#' @export
parse_save.parse_batch <- function(x, ...) {
    if (nrow(x) > 50) {
        # divide into smaller requests
        f <- rep(seq_len(ceiling(nrow(x) / 50)), each = 50)
        spl <- split(x, head(f, nrow(x)))
        rets <- lapply(spl, function(e) {
            parse_save(as.parse_batch(e, parse_class(x)), ...)
        })

        return(dplyr::bind_rows(rets))
    }

    path <- file.path("classes", parse_class(x))
    method <- "POST"

    if (!is.null(x$objectId)) {
        if (any(x$objectId == "")) {
            stop("Currently cannot mix create and update actions")
        }

        # updating objects
        path <- paste(path, x$objectId, sep = "/")
        method <- "PUT"
        x <- dplyr::select(x, -objectId, -updatedAt, -createdAt)
    }

    body <- batch_body(method, path, x)
    httr_ret <- parse_api_POST("batch", body, ...)
    ret <- modifyList(x, httr_ret)

    as.parse_batch(ret, parse_class(x))
}


#' create a batch of Parse objects from a data frame
#'
#' @param x data.frame to be saved
#' @param class_name class to save the objects to
#' @param ... extra arguments to pass on to parse_api_POST
#'
#' @export
parse_save.data.frame <- function(x, class_name, ...) {
    parse_save(as.parse_batch(x, class_name), ...)
}

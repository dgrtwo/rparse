#' delete a parse object or batch of objects
#'
#' @param x Parse object or batch to be deleted
#' @param ... extra arguments to be passed on to parse_api_DELETE
#'
#' @export
parse_delete <- function(x, ...) UseMethod("parse_delete")


#' delete a parse object
#'
#' @param x parse_object to be deleted
#' @param ... extra arguments to be passed on to parse_api_DELETE
#'
#' @export
parse_delete.parse_object <- function(x, ...) {
    assert_object_id(x)
    parse_api_DELETE(file.path("classes", parse_class(x), x$objectId), ...)
}


#' delete a batch of Parse objects
#'
#' @param x parse_batch to be deleted
#' @param ... extra arguments to pass on to parse_api_POST
#'
#' @export
parse_delete.parse_batch <- function(x, ...) {
    if (nrow(x) > 50) {
        # divide into smaller requests
        f <- rep(seq_len(ceiling(nrow(x) / 50)), each = 50)
        spl <- split(x, head(f, nrow(x)))
        rets <- lapply(spl, function(e) {
            parse_delete(as.parse_batch(e, parse_class(x)), ...)
        })

        return()
    }

    assert_object_id(x)
    paths <- paste0("classes/", parse_class(x), "/", x$objectId)
    body <- batch_body("DELETE", paths)
    parse_api_POST("batch", body, ...)
    NULL
}

#' coerce an object into a parse_object
#'
#' @param x object to be coerced
#' @param ... extra arguments
#'
#' @export
as.parse_object <- function(x, ...) {
    UseMethod("as.parse_object")
}


#' coerce a list into a parse_object
#'
#' @param x list to be coerced
#' @param class_name class name to convert to
#' @param ... extra arguments, not used
#'
#' @export
as.parse_object.list <- function(x, class_name, ...) {
    class(x) <- c(class_name, "parse_object", "list")
    x
}


#' create and save a parse object
#'
#' Construct a parse_object with the given fields. By default, saves the object to
#' Parse afterwards.
#'
#' @param class_name name of the class
#' @param ... fields contained in the object
#' @param save_after whether to s
#'
#' @export
parse_object <- function(class_name, ..., save_after = TRUE) {
    x <- list(...)
    ret <- as.parse_object(x, class_name)
    if (save_after) {
        ret <- parse_save(ret)
    }
    ret
}


#' get the class of a Parse object
#'
#' @param x parse_object or parse_batch
#'
#' @export
parse_class <- function(x) {
    if (!(is(x, "parse_object") || is(x, "parse_batch"))) {
        stop("not a Parse object")
    }
    class(x)[1]
}


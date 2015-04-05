#' convert an object to a Parse pointer
#'
#' Works on parse_object and parse_file
#'
#' @param x object to convert
#' @param ... extra arguments
#'
#' @export
as.parse_pointer <- function(x, ...) {
    UseMethod("as.parse_pointer")
}


#' convert an ordinary object to a Parse pointer
#'
#' This does nothing to the object
#'
#' @param x ordinary object
#' @param ... extra arguments, not used
#'
#' @export
as.parse_pointer.default <- function(x, ...) x


#' convert a Parse object to a Parse pointer
#'
#' Note that the Parse object must already be saved
#'
#' @param x parse_object to convert
#' @param ... extra arguments, not used
#'
#' @export
as.parse_pointer.parse_object <- function(x, ...) {
    assert_object_id(x)
    list("__type" = "Pointer",
         "className" = parse_class(x),
         "objectId" = x$objectId)
}

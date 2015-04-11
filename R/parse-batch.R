#' convert an object to a batch of Parse objects
#'
#' @param x object to be converted
#' @param ... extra arguments
#'
#' @export
as.parse_batch <- function(x, ...) {
    UseMethod("as.parse_batch")
}

#' convert a data.frame to a batch of Parse objects
#'
#' @param x data.frame to be converted
#' @param class_name class name of parse objects to be created
#' @param ... extra arguments, not used
#'
#' @export
as.parse_batch.data.frame <- function(x, class_name, ...) {
    class(x) <- c(class_name, "parse_batch", class(x))
    x
}

#' Upload a file to Parse
#'
#' @param name name to save the file to
#' @param path path to file to upload
#' @param type type of file (e.g. "application/zip")
#'
#' @return a parse_file object, containing a name and URL
#'
#' @export
parse_file <- function(name, path, type = NULL) {
    upfile <- httr::upload_file(path, type = type)
    ret <- parse_api_POST(file.path("files", name), body = upfile, to_json = FALSE)
    class(ret) <- c("parse_file", class(ret))
    ret
}


#' convert a Parse file to a pointer
#'
#' @param x parse_file to be converted
#' @param ... extra arguments (not used)
#'
#' @export
as.parse_pointer.parse_file <- function(x, ...) {
    list("__type" = "File",
         "name" = x$name)
}

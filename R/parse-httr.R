## Atomic Parse operations, such as GET, POST, DELETE, etc,
## including processing the result JSON objects and handling erros

Parse_URL <- "https://api.parse.com"

Parse_headers <- function() {
    header_strs <- c('X-Parse-Application-Id' = Sys.getenv("PARSE_APPLICATION_ID"),
                     'X-Parse-REST-API-Key' = Sys.getenv("PARSE_API_KEY"))

    if (any(header_strs == "")) {
        stop("Must set PARSE_APPLICATION_ID and PARSE_API_KEY environment variables")
    }

    # if currently logged in:
    user <- getOption("parse_user")
    if (!is.null(user)) {
        session_token <- user$sessionToken
        header_strs <- c(header_strs, "X-Parse-Session-Token" = session_token)
    }

    httr::add_headers(.headers = header_strs)
}


#' Perform a GET request to parse
#'
#' @param path path to send GET request to
#' @param ... extra arguments passed on to \code{httr::GET}
parse_api_GET <- function(path, ...) {
    req <- httr::GET(Parse_URL, path = paste0("1/", path), Parse_headers(), ...)
    process_Parse(req)
}


#' Perform a POST request to parse
#'
#' @param path path to send POST request to
#' @param body body of POST request
#' @param to_json whether to convert body to JSON
#' @param ... extra arguments passed on to \code{httr::POST}
parse_api_POST <- function(path, body, to_json = TRUE, ...) {
    if (to_json) {
        body <- rjson::toJSON(body)
    }
    req <- httr::POST(Parse_URL, path = paste0("1/", path),
                      body = body, encode = "json",
                      Parse_headers(), ...)

    process_Parse(req)
}


#' Perform a PUT request to parse
#'
#' @param path path to send PUT request to
#' @param body body of PUT request
#' @param to_json whether to convert body to JSON
#' @param ... extra arguments passed on to \code{httr::PUT}
parse_api_PUT <- function(path, body, to_json = TRUE, ...) {
    if (to_json) {
        body <- rjson::toJSON(body)
    }

    req <- httr::PUT(Parse_URL, path = paste0("1/", path), body = body,
                     Parse_headers(), ...)

    process_Parse(req)
}


#' Perform a DELETE request to parse
#'
#' @param path path to send DELETE request to
#' @param ... extra arguments passed on to \code{httr::DELETE}
parse_api_DELETE <- function(path, ...) {
    req <- httr::DELETE(Parse_URL, path = paste0("1/", path), Parse_headers(), ...)
}


#' process a request object from Parse
#'
#' @param req request object returned from an httr method
process_Parse <- function(req) {
    txt <- httr::content(req, as = "text")
    j <- jsonlite::fromJSON(txt)

    if (!is.null(j$error)) {
        stop("Error ", j$code, ": ", j$error)
    }

    if ("results" %in% names(j)) {
        j <- j$results
    }
    if ("success" %in% names(j)) {
        j <- j$success
    }

    for (col in names(j)) {
        if (grepl("At$", col)) {
            j[[col]] <- as.POSIXct(j[[col]], origin = "1970-01-01", format="%Y-%m-%dT%H:%M:%S")
        }
    }

    j
}

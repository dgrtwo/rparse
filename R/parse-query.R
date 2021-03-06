#' retrieve an object based on a query
#'
#' @param class_name name of Parse class
#' @param object_id if given, used to retrieve an object
#' @param ... extra arguments, not used
#'
#' @return a Parse object
#'
#' @export
parse_query <- function(class_name, object_id, limit = 1000, skip=0, ...) {
    url <- file.path("classes", class_name)

    if (!missing(object_id)) {
        ret <- parse_api_GET(file.path(url, object_id), ...)
    }
    else {
        # as of now, accepts only exact queries (not filtering etc)
        params <- list(...)
        q <- list(limit = limit,skip = skip)
        if (length(params) > 0) {
            q[["where"]] <- rjson::toJSON(params)
        }
        ret <- parse_api_GET(url, query = q)
    }
    if (length(ret) == 0) {
        return(NULL)
    }
    if (is(ret, "list")) {
        as.parse_object(ret, class_name)
    } else {
        as.parse_batch(ret, class_name)
    }
}

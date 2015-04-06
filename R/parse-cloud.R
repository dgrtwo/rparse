#' Create an interface to a Parse CloudCode function
#'
#' Create an interface to a Parse CloudCode function. This would have to have
#' been added to your "cloud" directory in your Parse application. For more,
#' see here: https://parse.com/docs/cloud_code_guide
#'
#' @param name Name of the function in CloudCode
#'
#' @examples
#'
#' # suppose a "hello world" function has been set up in the cloud
#'
#' hello_function <- parse_function("hello")
#' hello_function()
#'
#' @export
parse_function <- function(name) {
    ret <- function(...) {
        body <- list(...)
        if (length(body) == 0) {
            parse_api_POST(file.path("functions", name))
        } else {
            parse_api_POST(file.path("functions", name), body = body)
        }
    }
}

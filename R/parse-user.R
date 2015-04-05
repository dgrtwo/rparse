#' create a new user
#'
#' Create and sign in a user
#'
#' @param username desired username
#' @param password desired password
#' @param ... extra arguments to set, such as email
#'
#' @details A user's login token and other info is stored in the
#' \code{parse_user} option.
#'
#' @return A _User parse_object
#'
#' @export
parse_signup <- function(username, password, ...) {
    ret <- parse_api_POST("users/", list(username = username, password = password, ...))
    ret <- as.parse_object(ret, "_User")
    options(parse_user = ret)
    invisible(ret)
}


#' Log a user into Parse
#'
#' This logs a user into Parse based on a username and pasword combination.
#'
#' @param username username
#' @param password password
#'
#' @details A user's login token and other info is stored in the
#' \code{parse_user} option.
#'
#' @return A _User parse_object
#'
#' @export
parse_login <- function(username, password) {
    ret <- parse_api_GET("login/", query = list(username = username, password = password))
    ret <- as.parse_object(ret, "_User")
    options(parse_user = ret)
    invisible(ret)
}


#' Log out the current Parse user
#'
#' @export
parse_logout <- function() {
    options(parse_user = NULL)
}


#' lookup information about the current user
#'
#' @export
parse_current_user <- function() {
    as.parse_object(parse_api_GET("users/me"), "_User")
}


#' reset a user's password (if they have an email address registered)
#'
#' @param email User's e-mail address
#'
#' @export
parse_password_reset <- function(email) {
    ret <- parse_api_POST("requestPasswordReset", email = email)
    invisible(ret)
}

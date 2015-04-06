#' Create an Access Control List for an Parse object
#'
#' An Access Control List determines who can read and write a particular
#' object. In particular, it chooses whether there is public read or
#' write access. The user could also choose to deny themselves
#' write access.
#'
#' @param public_read whether anyone can read this object
#' @param public_write whether anyone can modify this object
#' @param owner_write whether the currently logged-in user can modify
#' this object
#'
#' @details The user must be logged in to create an ACL object.
#'
#' @return An "ACL" object, which is a list containing permissions
#' for the current user and/or the public ("*"). This object
#' can be passed as an \code{ACL =} argument to \code{parse_object}.
#'
#' @examples
#'
#' \dontrun{
#'
#' # save some scores from a game
#' parse_login("myaccount", "mypassword")
#'
#' # public:
#' game_score <- parse_object("GameScore", score = 42)
#'
#' # closed to public writing:
#' nowrite_acl <- ACL(public_write = FALSE)
#' game_score2 <- parse_object("GameScore", score = 43, ACL = nowrite_acl)
#'
#' # closed to public reading or writing:
#' noread_acl <- ACL(public_read = FALSE, public_write = FALSE)
#' game_score3 <- parse_object("GameScore", score = 44, ACL = noread_acl)
#'
#' }
#'
#' @export
ACL <- function(public_read = TRUE, public_write = TRUE,
                owner_write = TRUE) {
    ret <- list()

    if (public_read || public_write) {
        ret[["*"]] <- list()
    }
    if (public_read) ret[["*"]][["read"]] <- TRUE
    if (public_write) ret[["*"]][["write"]] <- TRUE

    u <- getOption("parse_user")
    if (is.null(u)) {
        stop("Not signed in; cannot set an ACL")
    }
    ret[[u$objectId]] <- list(read = TRUE)
    if (owner_write) ret[[u$objectId]][["write"]] <- TRUE

    class(ret) <- "ACL"
    ret
}

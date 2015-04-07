#' functions for setting up a Parse testing environment

#' Delete all objects of a class
#'
#' This is vectorized to take multiple classes. Be sure not to
#' call this by accident, and note that you need permission to
#' delete all of these objects.
#'
#' @param class_name one or more classes to delete all objects from
#'
#' @examples
#'
#' \dontrun{
#' parse_delete_all(c("Car", "GameScore"))
#' }
#'
#' @export
parse_delete_all <- Vectorize(function(class_name) {
    all_objs <- parse_query(class_name)
    if (!is.null(all_objs)) {
        parse_delete(all_objs)
    }
})


#' Set up a testing environment with a separate Parse application
#'
#' This changes Parse's application to a separate application that can be used
#' for testing, then deletes all objects that you request within it.
#' To use this, set up three system variables:
#' \code{PARSE_APPLICATION_ID_TEST}, \code{PARSE_API_KEY_TEST}, and
#' \code{PARSE_MASTER_KEY_TEST}. These should point somewhere besides your
#' production application. If requested, it will leave the test in the
#' master key state, and can back up the environment variables so that the
#' \code{\link{parse_restore_state}} function will return them.
#'
#' @param to_remove a list of class names of objects to be cleared from
#' the test environment
#' @param master whether to leave the master key set
#' @param backup whether to back environment variables up (be careful to
#' set this as FALSE if you are calling this a second time)
#' @param error whether to throw an error if the environment variables aren't
#' set
#' @param message whether to display an message if the environment variables aren't
#' set
#'
#' @details \code{error} is set to TRUE by default to ensure you don't try to set up
#' a test environment, fail, and run tests on your production database.
#' If you set \code{error = TRUE}, be sure you put all your tests in an
#' if statement.
#'
#' @examples
#'
#' \dontrun{
#' if (parse_setup_test(error = FALSE)) {
#'     # run some tests, then
#'     parse_restore_state()
#' }
#' }
#'
#' @export
parse_setup_test <- function(to_remove = NULL, master = FALSE, backup = TRUE,
                             error = TRUE, message = TRUE) {
    # first, check everything is set
    if (is.null(Sys.getenv("PARSE_APPLICATION_ID_TEST")) ||
        is.null(Sys.getenv("PARSE_API_KEY_TEST")) ||
        is.null(Sys.getenv("PARSE_MASTER_KEY_TEST"))) {
        msg <- paste("In order to set up a test environment, you need to set",
                     "PARSE_APPLICATION_ID_TEST and PARSE_API_KEY_TEST,",
                     "and PARSE_MASTER_KEY_TEST environment variables to point",
                     "to a Parse test application")

        if (error) stop(msg)
        if (message) message(msg)
        return(FALSE)
    }

    if (backup) {
        Sys.setenv(PARSE_APPLICATION_ID_BACKUP = Sys.getenv("PARSE_APPLICATION_ID"))
        Sys.setenv(PARSE_API_KEY_BACKUP = Sys.getenv("PARSE_API_KEY"))
        Sys.setenv(PARSE_MASTER_KEY_BACKUP = Sys.getenv("PARSE_MASTER_KEY"))
    }

    Sys.setenv(PARSE_APPLICATION_ID = Sys.getenv("PARSE_APPLICATION_ID_TEST"))
    Sys.setenv(PARSE_API_KEY = Sys.getenv("PARSE_API_KEY_TEST"))
    Sys.setenv(PARSE_MASTER_KEY = Sys.getenv("PARSE_MASTER_KEY_TEST"))

    # clear out test environment
    parse_delete_all(to_remove)

    if (!master) {
        Sys.setenv(PARSE_MASTER_KEY = "")
    }
    parse_logout()

    TRUE
}


#' Restore a testing environment back to its original state
#'
#' Restore a testing environment created with \code{\link{parse_setup_test}}
#' back to its original state, based on the backup environment variables
#' created therein. In the process it can clear one or more classes.
#'
#' @param to_remove one or more classes to delete all objects of in test
#' environment
#'
#' @export
parse_restore_state <- function(to_remove = NULL) {
    # clean up objects, making sure we're in the test environment
    # (so that we never clear out someone's main environment)
    parse_setup_test(backup = FALSE)
    parse_logout()

    # return sessions to their original
    Sys.setenv(PARSE_APPLICATION_ID = Sys.getenv("PARSE_APPLICATION_ID_BACKUP"))
    Sys.setenv(PARSE_API_KEY = Sys.getenv("PARSE_API_KEY_BACKUP"))
    Sys.setenv(PARSE_MASTER_KEY = Sys.getenv("PARSE_MASTER_KEY_BACKUP"))
}

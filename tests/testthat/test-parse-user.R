library(rparse)

context("Parse Users")

test_that("a user can be signed up, log in, and delete itself", {
    u <- parse_signup("dhelmet", "12345")
    expect_is(u, "_User")

    current_u <- parse_current_user()
    expect_equal(u$objectId, current_u$objectId)

    # log out and log back in
    parse_logout()
    expect_error(parse_current_user(), "invalid session")

    parse_login("dhelmet", "12345")
    current_u2 <- parse_current_user()
    expect_equal(current_u$objectId, current_u2$objectId)

    # delete the user
    parse_delete(u)
})

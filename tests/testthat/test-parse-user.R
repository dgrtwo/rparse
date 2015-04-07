library(rparse)

context("Parse Users")

classes <- c("_User", "GameScore")

if (parse_setup_test(classes, error = FALSE)) {
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


    test_that("a user can use ACL to save an object with private permissions", {
        u1 <- parse_signup("user1", "swordfish")

        open_score <- parse_object("GameScore", score = 500, cheat = FALSE)
        score_nowrite <- parse_object("GameScore", score = 600, cheat = FALSE,
                                      ACL = ACL(public_write = FALSE))
        score_noread <- parse_object("GameScore", score = 700, cheat = TRUE,
                                     ACL = ACL(public_read = FALSE,
                                               public_write = FALSE))

        # can you access all of them
        scores <- parse_query("GameScore")
        expect_equal(nrow(scores), 3)

        parse_logout()

        # should not be able to access the non-public one
        scores <- parse_query("GameScore")
        expect_equal(nrow(scores), 2)
        expect_true(!(700 %in% scores$score))
        expect_true(!(any(scores$cheat)))

        parse_query("GameScore", score_nowrite$objectId)
        expect_error(parse_query("GameScore", score_noread$objectId),
                     "object not found for get")

        u2 <- parse_signup("user2", "joshua")
        # other users should not be able to either
        scores <- parse_query("GameScore")
        expect_equal(nrow(scores), 2)
        expect_true(!(700 %in% scores$score))
        expect_true(!(any(scores$cheat)))

        # can write to one but not the other
        open_score$score <- 50
        parse_save(open_score)

        score_nowrite$score <- -1
        expect_error(parse_save(score_nowrite), "object not found for update")

        # you can deny yourself write access, I should write one for that
        # (am cleaning up with master key)

        # clean up
        parse_delete(u2)

        parse_login("user1", "swordfish")
        parse_delete_all("GameScore")
        parse_delete(u1)
        parse_logout()
    })


    test_that("ACL works for batch uploads", {
        u1 <- parse_signup("user1", "swordfish")

        scores <- data.frame(score = c(10, 20, 30), cheat = c(TRUE, TRUE, FALSE))
        # add ACLs
        scores$ACL <- list(ACL(public_write = FALSE),
                           ACL(public_write = FALSE),
                           ACL(public_read = FALSE, public_write = FALSE))

        parse_save(scores, "GameScore")

        s <- parse_query("GameScore")
        expect_equal(nrow(s), 3)

        parse_logout()

        s <- parse_query("GameScore")
        expect_equal(nrow(s), 2)

        parse_login("user1", "swordfish")
        parse_delete_all("GameScore")
        parse_delete(u1)
        parse_logout()
    })

    parse_restore_state(classes)
}

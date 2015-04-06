library(rparse)

context("Parse Objects")

# clear to start
remove_all("GameScore")

p <- parse_object("GameScore", score = 1337, save_after = FALSE)

test_that("parse_object creates an object with the appropriate types", {
    expect_is(p, "GameScore")
    expect_is(p, "parse_object")
    expect_is(p, "list")

    expect_equal(parse_class(p), "GameScore")
})


test_that("can save, retrieve and delete a parse_object", {
    saved <- parse_save(p)

    expect_true(!is.null(saved$objectId))

    retrieved <- parse_query("GameScore", saved$objectId)

    expect_is(retrieved, "GameScore")
    expect_is(retrieved, "parse_object")
    expect_equal(saved$objectId, retrieved$objectId)
    expect_equal(saved$score, retrieved$score)

    # delete
    parse_delete(saved)

    expect_error(parse_query("GameScore", saved$objectId), "object not found")
})


test_that("can save and delete a batch of objects", {
    remove_all("GameScore")

    d <- data.frame(cheat = c(FALSE, FALSE, TRUE), score = c(100, 200, 300))
    b <- as.parse_batch(d, "GameScore")

    expect_is(b, "parse_batch")

    b <- parse_save(b)

    ret <- parse_query("GameScore", score = 200)
    expect_equal(nrow(ret), 1)

    parse_delete(b)

    ret2 <- parse_query("GameScore", score = 200)
    expect_equal(length(ret2), 0)

    # clean up by removing all GameScore
    remove_all("GameScore")
})


test_that("can update an object or batch of objects", {
    score <- parse_object("GameScore", cheat = FALSE, score = 1337)
    expect_equal(parse_query("GameScore")$score, 1337)

    score$score <- 999
    parse_save(score)
    expect_equal(parse_query("GameScore")$score, 999)

    parse_delete(score)

    # batch
    d <- data.frame(cheat = c(FALSE, FALSE, TRUE), score = c(100, 200, 300))
    scores <- parse_save(d, "GameScore")
    expect_equal(d$score, scores$score)

    retrieved <- parse_query("GameScore")
    expect_equal(sort(d$score), sort(retrieved$score))

    # add one and update
    retrieved$score <- retrieved$score + 1
    parse_save(retrieved)

    retrieved2 <- parse_query("GameScore")
    expect_equal(sort(d$score + 1), sort(retrieved2$score))

    remove_all("GameScore")
})


test_that("can save an object with a pointer to another object", {
    remove_all(c("Movie", "Review"))

    the_matrix <- parse_object("Movie", title = "The Matrix")

    review <- parse_object("Review", movie = the_matrix,
                           stars = 5,
                           text = "Too bad they never made any sequels")

    retrieved_review <- parse_query("Review", stars = 5)
    expect_equal(retrieved_review$movie$objectId, the_matrix$objectId)

    remove_all(c("Movie", "Review"))
})

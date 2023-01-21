test_that("basic test case #1", {
    content <- c("\"", "\u010d", "\u0161", "\u00c4", "\u5b57", "\u30a2", "\u30a2\u30e0\u30ed")
    x <- data.frame(col = content)
    ascii_test_file <- tempfile(fileext = ".ods")
    write_ods(x, ascii_test_file)
    y <- read_ods(ascii_test_file)
    testthat::expect_equal(content, y$col)
})

test_that("XML escape characters", {
    content <- c("\"aa", "you & me", "he's bad", "1 <- 3", "2 >= 3")
    x <- data.frame(col = content)
    ascii_test_file <- tempfile(fileext = ".ods")
    write_ods(x, ascii_test_file)
    y <- read_ods(ascii_test_file)
    testthat::expect_equal(content, y$col)
})

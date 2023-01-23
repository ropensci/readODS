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


test_that("Fuzz the content space", {
    fuzz_dict <- readLines("../testdata/fuzz.txt")
    for (symbol in fuzz_dict) {
        x <- data.frame(col = symbol)
        ascii_test_file <- tempfile(fileext = ".ods")
        write_ods(x, ascii_test_file)
        y <- read_ods(ascii_test_file)
        testthat::expect_equal(symbol, y$col)
    }
})

test_that("Fuzz the sheet space", {
    fuzz_dict <- readLines("../testdata/fuzz.txt")
    for (symbol in fuzz_dict) {
        x <- data.frame(col = symbol)
        ascii_test_file <- tempfile(fileext = ".ods")
        write_ods(x, ascii_test_file, sheet = symbol)
        sheet <- list_ods_sheets(ascii_test_file)
        testthat::expect_equal(symbol, sheet)
    }
})

## random fuzzing for 30 times

test_that("random fuzz: content space / tame (Latin Range)", {
    skip_on_cran()
    for (i in seq_len(30)) {
        content <- stringi::stri_rand_strings(10, 10, "[\u0021-\u007e]")
        x <- data.frame(col = content)
        ascii_test_file <- tempfile(fileext = ".ods")
        write_ods(x, ascii_test_file, sheet = content[1])
        y <- read_ods(ascii_test_file)
        testthat::expect_equal(content, y$col)
        sheet <- list_ods_sheets(ascii_test_file)
        testthat::expect_equal(content[1], sheet)
    }
})

test_that("random fuzz: content space / Arrows", {
    skip_on_cran()
    for (i in seq_len(30)) {
        content <- stringi::stri_rand_strings(10, 10, "[\u2190-\u228F]")
        x <- data.frame(col = content)
        ascii_test_file <- tempfile(fileext = ".ods")
        write_ods(x, ascii_test_file, sheet = content[1])
        y <- read_ods(ascii_test_file)
        testthat::expect_equal(content, y$col)
        sheet <- list_ods_sheets(ascii_test_file)
        testthat::expect_equal(content[1], sheet)
    }
})

test_that("random fuzz: content space / Japanese and Co.", {
    skip_on_cran()
    for (i in seq_len(30)) {
        content <- stringi::stri_rand_strings(10, 10, "[\u3041-\u313F]")
        x <- data.frame(col = content)
        ascii_test_file <- tempfile(fileext = ".ods")
        write_ods(x, ascii_test_file, sheet = content[1])
        y <- read_ods(ascii_test_file)
        testthat::expect_equal(content, y$col)
        sheet <- list_ods_sheets(ascii_test_file)
        testthat::expect_equal(content[1], sheet)
    }
})

test_that("random fuzz: content space / Something else", {
    skip_on_cran()
    for (i in seq_len(30)) {
        content <- stringi::stri_rand_strings(10, 10, "[\uAA80-\uAE7F]")
        x <- data.frame(col = content)
        ascii_test_file <- tempfile(fileext = ".ods")
        write_ods(x, ascii_test_file, sheet = content[1])
        y <- read_ods(ascii_test_file)
        testthat::expect_equal(content, y$col)
        sheet <- list_ods_sheets(ascii_test_file)
        testthat::expect_equal(content[1], sheet)
    }
})

## Numeric

test_that("Numeric", {
    skip_on_cran()
    for (i in seq_len(30)) {
        nums <- sample(1:1000) * rnorm(100)
        x <- data.frame(col = nums)
        ascii_test_file <- tempfile(fileext = ".ods")
        write_ods(x, ascii_test_file)
        y <- read_ods(ascii_test_file)
        expect_equal(y$col, nums)
    }    
})

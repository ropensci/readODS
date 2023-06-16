library(dplyr)

tmp <- tempfile(fileext=".ods")

# Use this instead of numbers_to_letters for testing - only works for 1:26
cols_to_letters <- function(n) {
    stopifnot(n <= 26)
    
    seq <- seq_len(n)
    sapply(seq, function(n) LETTERS[n])
}

setup({
    
})
teardown({

    unlink(tmp)
})

test_that("Write Excel sheets", {

    # Use a dataframe with row and column headers, and at least one charactor column
    # If you write a dataframe which has not had rownames explicitly set and use row_names=T,
    # reading it back and comparing will give an attribute difference
    starwars10 <- readRDS("../testdata/starwars10.rds")

    expect_silent(write_ods(starwars10, tmp, "SW", row_names = FALSE, col_names = FALSE))
    expect_true(file.exists(tmp))
    expect_silent(write_ods(starwars10, tmp, "SWR", row_names=TRUE, col_names = FALSE, append = TRUE))
    expect_silent(write_ods(starwars10, tmp, "SWC", row_names = FALSE, col_names = TRUE, append = TRUE))
    expect_silent(write_ods(starwars10, tmp, "SWRC", row_names=TRUE, col_names = TRUE, append = TRUE))
    expect_silent(write_ods(starwars10[1, seq_len(ncol(starwars10))], tmp, "SW1", row_names=TRUE, col_names = TRUE, append = TRUE))
    expect_silent(write_ods(starwars10[seq_len(nrow(starwars10)), 1, drop=FALSE], tmp, "SW10", row_names=TRUE, col_names = TRUE, append = TRUE))

    ## SWRC is there
    expect_error(write_ods(starwars10, tmp, "SWRC", row_names=TRUE, col_names = TRUE, append = TRUE))
    ## SWRC is there, but this is update
    expect_error(write_ods(starwars10, tmp, "SWRC", row_names=TRUE, col_names = TRUE, update = TRUE), NA)
    expect_error(write_ods(starwars10, tmp, "whatevernotexists", row_names=TRUE, col_names = TRUE, update = TRUE))

    df <- read_ods(tmp, "SW", row_names = FALSE, col_names = FALSE, strings_as_factors = TRUE)
    expect_true(all.equal({
        cars <- starwars10
        rownames(cars) <- NULL
        colnames(cars) <- cols_to_letters(ncol(cars))
        cars
    }, df))

    df <- read_ods(tmp, "SWR", row_names = TRUE, col_names = FALSE, strings_as_factors = TRUE)
    expect_true(all.equal({
        cars <- starwars10
        colnames(cars) <- cols_to_letters(ncol(cars))
        cars}, df))

    df <- read_ods(tmp, "SWC", row_names = FALSE, col_names = TRUE, strings_as_factors = TRUE)
    expect_true(all.equal({
        cars <- starwars10
        rownames(cars) <- NULL
        cars
    }, df))

    df <- read_ods(tmp, "SWRC", row_names = TRUE, col_names = TRUE, strings_as_factors = TRUE)
    expect_true(all.equal(starwars10, df))

    df <- read_ods(tmp, "SW1", row_names = TRUE, col_names = TRUE, strings_as_factors = TRUE)

    expect_false(isTRUE(all.equal(starwars10[1, seq_len(ncol(starwars10))], df))) # factor mismatch
    expect_true(all((df == starwars10[1, seq_len(ncol(starwars10))])[1,]))

    df <- read_ods(tmp, "SW10", row_names = TRUE, col_names = TRUE, strings_as_factors = TRUE)
    expect_true(all.equal(starwars10[seq_len(nrow(starwars10)), 1, drop=FALSE], df))
})

test_that("issue 107", {
    legend <- readRDS("../testdata/legend.rds")
    expect_error(write_ods(legend, tmp, sheet = "Legend"), NA)
    expect_error(write_ods(legend, tmp, sheet = "Legend", update = TRUE), NA)
    expect_error(write_ods(legend, tmp, sheet = "Legend2", append = TRUE), NA)
})

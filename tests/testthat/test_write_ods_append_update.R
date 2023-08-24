test_that("defensive", {
    ## file doesn't exists
    expect_error(write_ods(mtcars, "../testdata/this_surely_doesnt_exists.ods", sheet = "whatever", append = TRUE))
    expect_error(write_ods(mtcars, "../testdata/this_surely_doesnt_exists.ods", sheet = "whatever", update = TRUE))
    expect_error(write_fods(mtcars, "../testdata/this_surely_doesnt_exists.fods", sheet = "whatever", append = TRUE))
    expect_error(write_fods(mtcars, "../testdata/this_surely_doesnt_exists.fods", sheet = "whatever", update = TRUE))
})

.test_funcs <- function(funcs) {
    ## Use a dataframe with row and column headers, and at least one charactor column
    ## If you write a dataframe which has not had rownames explicitly set and use row_names=T,
    ## reading it back and comparing will give an attribute difference
    starwars10 <- readRDS("../testdata/starwars10.rds")
    expect_silent(tmp <- funcs[["w"]](starwars10, path = tempfile(fileext = ".ods"), sheet = "SW", row_names = FALSE, col_names = FALSE))
    expect_true(file.exists(tmp))
    expect_silent(funcs[["w"]](starwars10, tmp, "SWR", row_names = TRUE, col_names = FALSE, append = TRUE))
    expect_silent(funcs[["w"]](starwars10, tmp, "SWC", row_names = FALSE, col_names = TRUE, append = TRUE))
    expect_silent(funcs[["w"]](starwars10, tmp, "SWRC", row_names = TRUE, col_names = TRUE, append = TRUE))
    expect_silent(funcs[["w"]](starwars10[1, seq_len(ncol(starwars10))], tmp, "SW1", row_names = TRUE, col_names = TRUE, append = TRUE))
    expect_silent(funcs[["w"]](starwars10[seq_len(nrow(starwars10)), 1, drop = FALSE], tmp, "SW10", row_names=TRUE, col_names = TRUE, append = TRUE))

    ## SWRC is there
    expect_error(funcs[["w"]](starwars10, tmp, "SWRC", row_names = TRUE, col_names = TRUE, append = TRUE))
    ## SWRC is there, but this is update
    expect_error(funcs[["w"]](starwars10, tmp, "SWRC", row_names = TRUE, col_names = TRUE, update = TRUE), NA)
    expect_error(funcs[["w"]](starwars10, tmp, "whatevernotexists", row_names = TRUE, col_names = TRUE, update = TRUE))

    df <- suppressMessages(funcs[["r"]](tmp, "SW", row_names = FALSE, col_names = FALSE, strings_as_factors = TRUE, as_tibble = FALSE))
    expect_true(all.equal({
        cars <- starwars10
        rownames(cars) <- NULL
        colnames(cars) <- vctrs::vec_as_names(rep("", 9), repair = "unique")
        cars
    }, df))

    df <- funcs[["r"]](tmp, "SWR", row_names = TRUE, col_names = FALSE, strings_as_factors = TRUE, as_tibble = FALSE)
    expect_true(all.equal({
        cars <- starwars10
        colnames(cars) <- vctrs::vec_as_names(rep("", 9), repair = "unique")
        cars}, df))

    df <- funcs[["r"]](tmp, "SWC", row_names = FALSE, col_names = TRUE, strings_as_factors = TRUE, as_tibble = FALSE)
    expect_true(all.equal({
        cars <- starwars10
        rownames(cars) <- NULL
        cars
    }, df))

    df <- funcs[["r"]](tmp, "SWRC", row_names = TRUE, col_names = TRUE, strings_as_factors = TRUE, as_tibble = FALSE)
    expect_true(all.equal(starwars10, df))

    df <- funcs[["r"]](tmp, "SW1", row_names = TRUE, col_names = TRUE, strings_as_factors = TRUE, as_tibble = FALSE)

    expect_false(isTRUE(all.equal(starwars10[1, seq_len(ncol(starwars10))], df))) # factor mismatch
    expect_true(all((df == starwars10[1, seq_len(ncol(starwars10))])[1,]))

    df <- funcs[["r"]](tmp, "SW10", row_names = TRUE, col_names = TRUE, strings_as_factors = TRUE, as_tibble = FALSE)
    expect_true(all.equal(starwars10[seq_len(nrow(starwars10)), 1, drop = FALSE], df))

}

test_that("Update / append sheets ods & fods", {
    .test_funcs(list("r" = read_ods, "w" = write_ods))
    .test_funcs(list("r" = read_fods, "w" = write_fods))
})

test_that("issue 107", {
    legend <- readRDS("../testdata/legend.rds")
    temp <- tempfile()
    expect_error(write_ods(legend, path = temp, sheet = "Legend"), NA)
    expect_error(write_ods(legend, path = temp, sheet = "Legend", update = TRUE), NA)
    expect_error(write_ods(legend, path = temp, sheet = "Legend2", append = TRUE), NA)
})

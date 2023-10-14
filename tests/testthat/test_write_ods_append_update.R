test_that("defensive", {
    ## file doesn't exists
    expect_error(write_ods(mtcars, "../testdata/this_surely_doesnt_exists.ods", sheet = "whatever", append = TRUE))
    expect_error(write_ods(mtcars, "../testdata/this_surely_doesnt_exists.ods", sheet = "whatever", update = TRUE))
    expect_error(write_fods(mtcars, "../testdata/this_surely_doesnt_exists.fods", sheet = "whatever", append = TRUE))
    expect_error(write_fods(mtcars, "../testdata/this_surely_doesnt_exists.fods", sheet = "whatever", update = TRUE))
    ## non df
    ods_path <- write_ods(mtcars, sheet = "whatever")
    fods_path <- write_fods(mtcars, sheet = "whatever")

    expect_error(write_ods(NA, ods_path, sheet = "whatever", update = TRUE))
    expect_error(write_fods(NA, fods_path, sheet = "whatever", update = TRUE))
    expect_error(write_ods(NULL, ods_path, sheet = "whatever", update = TRUE))
    expect_error(write_fods(NULL, fods_path, sheet = "whatever", update = TRUE))
    expect_error(write_ods(c(123), ods_path, sheet = "whatever", update = TRUE))
    expect_error(write_fods(c(22323), fods_path, sheet = "whatever", update = TRUE))
    expect_error(write_fods(list(c(22323)), fods_path, sheet = "whatever", update = TRUE))
    expect_error(write_ods(NA, ods_path, sheet = "whatever1", append = TRUE))
    expect_error(write_fods(NA, fods_path, sheet = "whatever1", append = TRUE))
    expect_error(write_ods(NULL, ods_path, sheet = "whatever1", append = TRUE))
    expect_error(write_fods(NULL, fods_path, sheet = "whatever1", append = TRUE))
    expect_error(write_ods(c(123), ods_path, sheet = "whatever1", append = TRUE))
    expect_error(write_fods(c(22323), fods_path, sheet = "whatever1", append = TRUE))
    expect_error(write_fods(list(c(22323)), fods_path, sheet = "whatever1", append = TRUE))
})

.test_funcs <- function(funcs) {
    ## Use a dataframe with row and column headers, and at least one charactor column
    ## If you write a dataframe which has not had rownames explicitly set and use row_names=T,
    ## reading it back and comparing will give an attribute difference
    starwars10 <- readRDS("../testdata/starwars10.rds")
    withr::with_tempfile("tempods", fileext = ".ods", code = {
        expect_silent(funcs[["w"]](starwars10, path = tempods, sheet = "SW", row_names = FALSE, col_names = FALSE))
        expect_true(file.exists(tempods))
        expect_silent(funcs[["w"]](starwars10, tempods, "SWR", row_names = TRUE, col_names = FALSE, append = TRUE))
        expect_silent(funcs[["w"]](starwars10, tempods, "SWC", row_names = FALSE, col_names = TRUE, append = TRUE))
        expect_silent(funcs[["w"]](starwars10, tempods, "SWRC", row_names = TRUE, col_names = TRUE, append = TRUE))
        expect_silent(funcs[["w"]](starwars10[1, seq_len(ncol(starwars10))], tempods, "SW1", row_names = TRUE, col_names = TRUE, append = TRUE))
        expect_silent(funcs[["w"]](starwars10[seq_len(nrow(starwars10)), 1, drop = FALSE], tempods, "SW10", row_names=TRUE, col_names = TRUE, append = TRUE))

        ## SWRC is there
        expect_error(funcs[["w"]](starwars10, tempods, "SWRC", row_names = TRUE, col_names = TRUE, append = TRUE))
        ## SWRC is there, but this is update
        expect_error(funcs[["w"]](starwars10, tempods, "SWRC", row_names = TRUE, col_names = TRUE, update = TRUE), NA)
        expect_error(funcs[["w"]](starwars10, tempods, "whatevernotexists", row_names = TRUE, col_names = TRUE, update = TRUE))

        df <- suppressMessages(funcs[["r"]](tempods, "SW", row_names = FALSE, col_names = FALSE, strings_as_factors = TRUE, as_tibble = FALSE))
        expect_true(all.equal({
            cars <- starwars10
            rownames(cars) <- NULL
            colnames(cars) <- vctrs::vec_as_names(rep("", 9), repair = "unique")
            cars
        }, df))

        df <- funcs[["r"]](tempods, "SWR", row_names = TRUE, col_names = FALSE, strings_as_factors = TRUE, as_tibble = FALSE)
        expect_true(all.equal({
            cars <- starwars10
            colnames(cars) <- vctrs::vec_as_names(rep("", 9), repair = "unique")
            cars}, df))

        df <- funcs[["r"]](tempods, "SWC", row_names = FALSE, col_names = TRUE, strings_as_factors = TRUE, as_tibble = FALSE)
        expect_true(all.equal({
            cars <- starwars10
            rownames(cars) <- NULL
            cars
        }, df))

        df <- funcs[["r"]](tempods, "SWRC", row_names = TRUE, col_names = TRUE, strings_as_factors = TRUE, as_tibble = FALSE)
        expect_true(all.equal(starwars10, df))

        df <- funcs[["r"]](tempods, "SW1", row_names = TRUE, col_names = TRUE, strings_as_factors = TRUE, as_tibble = FALSE)

        expect_false(isTRUE(all.equal(starwars10[1, seq_len(ncol(starwars10))], df))) # factor mismatch
        expect_true(all((df == starwars10[1, seq_len(ncol(starwars10))])[1,]))
        df <- funcs[["r"]](tempods, "SW10", row_names = TRUE, col_names = TRUE, strings_as_factors = TRUE, as_tibble = FALSE)
        expect_true(all.equal(starwars10[seq_len(nrow(starwars10)), 1, drop = FALSE], df))
    })
}

test_that("Update / append sheets ods & fods", {
    .test_funcs(list("r" = read_ods, "w" = write_ods))
    .test_funcs(list("r" = read_fods, "w" = write_fods))
})

test_that("issue 107", {
    legend <- readRDS("../testdata/legend.rds")
    withr::with_tempfile("temp", {
        expect_error(write_ods(legend, path = temp, sheet = "Legend"), NA)
        expect_error(write_ods(legend, path = temp, sheet = "Legend", update = TRUE), NA)
        expect_error(write_ods(legend, path = temp, sheet = "Legend2", append = TRUE), NA)
    })
})

test_empty_edge <- function(.write_func, .list_func, .read_func) {
    x <- .write_func(mtcars, sheet = "not_empty")
    .write_func(tibble::tibble(), path = x, sheet = "empty", append = TRUE)
    expect_equal(.list_func(x), c("not_empty", "empty"))
    expect_true(nrow(.read_func(x, "not_empty")) > 0)
    expect_true(nrow(suppressWarnings(.read_func(x, "empty"))) == 0)
    ## reverse
    x <- .write_func(tibble::tibble(), sheet = "empty")
    .write_func(mtcars, path = x, sheet = "not_empty", append = TRUE)
    expect_equal(.list_func(x), c("empty", "not_empty"))
    expect_true(nrow(.read_func(x, "not_empty")) > 0)
    expect_true(nrow(suppressWarnings(.read_func(x, "empty"))) == 0)
    ## update
    x <- .write_func(mtcars, sheet = "x1")
    expect_error(.write_func(tibble::tibble(), path = x, sheet = "x1", update = TRUE), NA)
    expect_true(nrow(suppressWarnings(.read_func(x, "x1"))) == 0)
    x <- .write_func(tibble::tibble(), sheet = "x1")
    expect_error(.write_func(mtcars, path = x, sheet = "x1", update = TRUE), NA)
    expect_true(nrow(.read_func(x, "x1")) != 0)

}

test_that("edge cases", {
    test_empty_edge(write_ods, list_ods_sheets, read_ods)
    test_empty_edge(write_fods, list_fods_sheets, read_fods)
    ## #163
    withr::with_tempfile("rubbish_file", {
        writeLines("<ul><li>hello</li></ul>", rubbish_file)
        expect_error(write_fods(mtcars, rubbish_file, sheet = "whatever", append = TRUE))
        expect_error(write_fods(mtcars, rubbish_file, sheet = "whatever", update = TRUE))
    }, fileext = ".fods")
})

library(dplyr)

tmp <- tempfile(fileext=".ods")

# Use this instead of numbers_to_letters for testing - only works for 1:26
cols_to_letters <- function(n)
{
    assert_that(n <= 26)
    
    seq <- 1:n
    sapply(seq, function(n) { LETTERS[n] })
}

setup({
    
})
teardown({

    unlink(tmp)
})

test_that("Write Excel sheets", {
    
    mtcars44 <- mtcars[1:4, 1:4]
    
    expect_silent(write_ods_2(mtcars44, tmp, "Cars", row_names = FALSE, col_names = FALSE))
    expect_true(file.exists(tmp))
    expect_warning(write_ods_2(mtcars44, tmp, "CarsR", row_names=TRUE, col_names = FALSE))
    expect_warning(write_ods_2(mtcars44, tmp, "CarsC", row_names = FALSE, col_names = TRUE))
    expect_warning(write_ods_2(mtcars44, tmp, "CarsRC", row_names=TRUE, col_names = TRUE))
    expect_warning(write_ods_2(mtcars[1:1, 1:3], tmp, "Cars13", row_names=TRUE, col_names = TRUE))
    expect_warning(write_ods_2(mtcars[1:3, 1:1, drop=FALSE], tmp, "Cars31", row_names=TRUE, col_names = TRUE))
    expect_error(write_ods_2(mtcars44, tmp, "CarsRC", row_names=TRUE, col_names = TRUE, overwrite_sheet = FALSE))
    
    df <- read_ods(tmp, "Cars", row_names = FALSE, col_names = FALSE)
    expect_true(all.equal({ 
        cars <- mtcars44
        rownames(cars) <- 1:nrow(cars)
        colnames(cars) <- cols_to_letters(ncol(cars))
        cars
    }, df))
    
    df <- read_ods(tmp, "CarsR", row_names = TRUE, col_names = FALSE)
    expect_true(all.equal({ 
        cars <- mtcars44
        colnames(cars) <- cols_to_letters(ncol(cars))
        cars
    }, df))
    
    df <- read_ods(tmp, "CarsC", row_names = FALSE, col_names = TRUE)
    expect_true(all.equal({ 
        cars <- mtcars44
        rownames(cars) <- 1:nrow(cars)
        cars
    }, df))
    
    df <- read_ods(tmp, "CarsRC", row_names = TRUE, col_names = TRUE)
    expect_true(all.equal(mtcars44, df))
    
    df <- read_ods(tmp, "Cars13", row_names = TRUE, col_names = TRUE)
    expect_true(all.equal(mtcars[1:1, 1:3], df))
    
    df <- read_ods(tmp, "Cars31", row_names = TRUE, col_names = TRUE)
    expect_true(all.equal(mtcars[1:3, 1:1, drop=FALSE], df))
})

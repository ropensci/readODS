library("datasets")
test_that("Single column ODS", {
    expect_true(file.exists(write_ods(mtcars)))
})

test_that("Single column FODS", {
    expect_true(file.exists(write_fods(mtcars)))
})

test_that("overwrite, #54", {
    data("faithful")
    path <- write_ods(faithful)
    data("CO2")
    write_ods(CO2, path)
    content_in_file <- read_ods(path)
    expect_true(nrow(content_in_file) == 84)
})

test_that("overwrite (fods), #54", {
    data("faithful")
    path <- write_fods(faithful)
    data("CO2")
    write_fods(CO2, path)
    content_in_file <- read_fods(path)
    expect_true(nrow(content_in_file) == 84)
})

test_that(".preprocess_x", {
    expect_error(.preprocess_x(matrix()))
    expect_error(.preprocess_x(matrix()))
    expect_error(.preprocess_x(as.data.frame(t(matrix(rep(NA, times = 16385))))))
    expect_error(.preprocess_x(as.data.frame(matrix(rep(NA, times = 2^20 + 1)))))
    expect_false(inherits(.preprocess_x(tibble::tibble()), "tbl_df"))
})

test_that("write to non-existing location", {
    expect_error(write_ods(mtcars, file.path("/there/is/no/way/this/exists/anyway", stringi::stri_rand_strings(1, 20, pattern = "[A-Za-z0-9]"),"mtcars.ods")))
    expect_error(write_fods(mtcars, file.path("/there/is/no/way/this/exists/anyway", stringi::stri_rand_strings(1, 20, pattern = "[A-Za-z0-9]"),"mtcars.fods")))
})

## from now on no need to test both fods and ods and they are using the same engine

test_that("na_as_string, #79", {
    temp_odsfile <- tempfile(fileext = ".ods")
    temp_odsdir <- tempdir()
    na_data <- data.frame(x = c(1.0, NA, 2.0))
    write_ods(na_data, path = temp_odsfile, na_as_string = TRUE, col_names = FALSE)
    zip::unzip(temp_odsfile, exdir = temp_odsdir)
    contentfile <- file.path(temp_odsdir, "content.xml")
    expect_true(any(grepl("office:value-type=\"string\"", suppressWarnings(readLines(contentfile)))))
    temp_odsfile <- tempfile(fileext = ".ods")
    temp_odsdir <- tempdir()
    na_data <- data.frame(x = c(1.0, NA, 2.0))
    write_ods(na_data, path = temp_odsfile, na_as_string = FALSE, col_names = FALSE)
    zip::unzip(temp_odsfile, exdir = temp_odsdir)
    contentfile <- file.path(temp_odsdir, "content.xml")
    expect_false(any(grepl("office:value-type=\"string\"", suppressWarnings(readLines(contentfile)))))
})

test_that("na_as_string, round trip", {
    iris_na <- tibble::as_tibble(iris)
    iris_na[1,1] <- NA
    iris_na[1,2] <- NA
    iris_na[2,1] <- NA
    iris_na[5,2] <- NA
    iris_na$Species <- as.character(iris_na$Species)
    expect_true(all.equal(iris_na, readODS::read_ods(readODS::write_ods(iris_na, na_as_string = FALSE))))
    ## default
    expect_true(all.equal(iris_na, readODS::read_ods(readODS::write_ods(iris_na))))
    expect_false(is.logical(all.equal(iris_na, readODS::read_ods(readODS::write_ods(iris_na, na_as_string = TRUE)))))
    ## sanity check
    iris2 <- tibble::as_tibble(iris)
    iris2$Species <- as.character(iris2$Species)
    expect_true(all.equal(iris2, readODS::read_ods(readODS::write_ods(iris2, na_as_string = FALSE))))
    expect_true(all.equal(iris2, readODS::read_ods(readODS::write_ods(iris2, na_as_string = TRUE))))
})

test_that("data time columns #137", {
    flights_head <- readRDS("../testdata/flights_head.RDS")
    back <- read_ods(write_ods(flights_head))
    expect_equal(ncol(back), ncol(flights_head))
    expect_equal(colnames(back), colnames(flights_head))
})

test_that("edge cases 0 x 0 no column info #142", {
    expect_error(filename <- write_ods(tibble::tibble()), NA)
    expect_equal(suppressWarnings(read_ods(filename)), tibble::tibble())
    expect_error(filename <- write_ods(data.frame()), NA)
    expect_equal(suppressWarnings(read_ods(filename)), tibble::tibble())
})

test_that("edge cases 0 x 0 with column info #142", {
    zero_rows <- tibble::tibble(mtcars[0,])
    expect_error(filename <- write_ods(zero_rows), NA) ## default col_names = TRUE
    expect_true(nrow(suppressMessages(read_ods(filename, col_names = FALSE))) == 1)
    expect_error(filename <- write_ods(zero_rows, col_names = FALSE), NA)
    expect_true(nrow(suppressWarnings(read_ods(filename, col_names = FALSE))) == 0)
})

test_that("edge cases rownames / colnames only, no data, #142", {
    all_na <- mtcars[,c(1,2), drop = FALSE]
    all_na[,1] <- NA
    all_na[,2] <- NA
    expect_error(filename <- write_ods(all_na), NA)
    expect_true(nrow(suppressMessages(read_ods(filename, col_names = FALSE))) == 1)
    expect_error(filename <- write_ods(all_na, col_names = FALSE), NA)
    expect_true(nrow(suppressWarnings(read_ods(filename, col_names = FALSE))) == 0)
    expect_error(filename <- write_ods(all_na, row_names = TRUE), NA)
    expect_equal(dim(suppressMessages(read_ods(filename, col_names = FALSE))), dim(all_na) + 1)
    ## just rownames
    expect_error(filename <- write_ods(all_na, row_names = TRUE, col_names = FALSE), NA)
    expect_equal(nrow(suppressMessages(read_ods(filename, col_names = FALSE))), nrow(all_na))
})

test_that("edge case, list-columns #142", {
    ## no one knows how list-columns should be dealt with.
    ## just process them with no errors
    lc_test <- tibble::tibble(mtcars)
    lc_test$lc <- strsplit(rownames(mtcars), " ")
    expect_error(write_ods(lc_test), NA)
})

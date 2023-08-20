library("datasets")
test_that("Single column ODS", {
    expect_true(write_ods(mtcars, "test.ods") %in% dir())
    file.remove("test.ods")
})

test_that("overwrite, #54", {
    data("faithful")
    write_ods(faithful, "mydata.ods")
    data("CO2")
    write_ods(CO2, "mydata.ods")
    content_in_file <- read_ods('mydata.ods')
    expect_true(nrow(content_in_file) == 84)
    ## cleanup
    file.remove("mydata.ods")
})

test_that("na_as_string, #79", {
    temp_odsfile <- tempfile(fileext = ".ods")
    temp_odsdir <- tempdir()
    na_data <- data.frame(x = c(1.0, NA, 2.0))
    write_ods(na_data, path = temp_odsfile, na_as_string = TRUE, col_names = FALSE)
    zip::unzip(temp_odsfile, exdir = temp_odsdir)
    contentfile <- file.path(temp_odsdir, "content.xml")
    expect_true(grepl("office:value-type=\"string\"", suppressWarnings(readLines(contentfile))))
    temp_odsfile <- tempfile(fileext = ".ods")
    temp_odsdir <- tempdir()
    na_data <- data.frame(x = c(1.0, NA, 2.0))
    write_ods(na_data, path = temp_odsfile, na_as_string = FALSE, col_names = FALSE)
    zip::unzip(temp_odsfile, exdir = temp_odsdir)
    contentfile <- file.path(temp_odsdir, "content.xml")
    expect_false(grepl("office:value-type=\"string\"", suppressWarnings(readLines(contentfile))))
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

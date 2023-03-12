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
    na_data <- data.frame(x = c(1.0, NA))
    write_ods(na_data, path = temp_odsfile, na_as_string = TRUE, col_names = FALSE)
    utils::unzip(temp_odsfile, exdir = temp_odsdir)
    contentfile <- file.path(temp_odsdir, "content.xml")
    expect_true(grepl("office:value-type=\"string\" office:value=\"NA\"", suppressWarnings(readLines(contentfile))))
    temp_odsfile <- tempfile(fileext = ".ods")
    temp_odsdir <- tempdir()
    na_data <- data.frame(x = c(1.0, NA))
    write_ods(na_data, path = temp_odsfile, na_as_string = FALSE, col_names = FALSE)
    utils::unzip(temp_odsfile, exdir = temp_odsdir)
    contentfile <- file.path(temp_odsdir, "content.xml")
    expect_false(grepl("office:value-type=\"string\" office:value=\"NA\"", suppressWarnings(readLines(contentfile))))
})

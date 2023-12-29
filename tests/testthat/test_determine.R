test_that(".determine_ods_format works", {
    expect_equal(.determine_ods_format(readODS::write_ods(iris)), "ods")
    expect_equal(.determine_ods_format(readODS::write_fods(iris)), "fods")
    expect_equal(.determine_ods_format(readODS::write_ods(iris, tempfile(fileext = ".fods"))), "fods")
    expect_equal(.determine_ods_format(readODS::write_ods(iris, tempfile(fileext = ".fods")), guess = TRUE), "ods")
    expect_equal(.determine_ods_format(readODS::write_fods(iris, tempfile(fileext = ".xml"))), "fods")
    expect_equal(.determine_ods_format(readODS::write_ods(iris, tempfile(fileext = ".fods")), guess = TRUE), "ods")
    expect_equal(.determine_ods_format(readODS::write_ods(iris, tempfile(fileext = ".xml"))), "fods")
    expect_equal(.determine_ods_format(readODS::write_ods(iris, tempfile(fileext = ".xml")), guess = TRUE), "ods")
    expect_equal(.determine_ods_format(readODS::write_fods(iris, tempfile(fileext = ".ods")), guess = TRUE), "fods")
    ## don't guess
    expect_equal(.determine_ods_format(readODS::write_fods(iris, tempfile(fileext = ".ods")), ods_format = "zip"), "zip")
})

test_that("integration in read_ods", {
    expect_error(readODS::read_ods(readODS::write_fods(iris)), NA)
    expect_error(readODS::read_ods(readODS::write_ods(iris)), NA)
    expect_error(readODS::read_fods(readODS::write_ods(iris))) ## err, read_fods is not a common interface
    expect_error(readODS::read_fods(readODS::write_fods(iris)), NA)
    ## override
    temp_fods <- tempfile(fileext = ".fods")
    expect_error(readODS::read_ods(readODS::write_ods(iris, temp_fods), ods_format = "ods"), NA)
    expect_error(readODS::read_ods(readODS::write_ods(iris, temp_fods), guess = TRUE), NA)
})

test_that("integration in list_ods_sheets", {
    expect_error(readODS::list_ods_sheets(readODS::write_fods(iris)), NA)
    expect_error(readODS::list_ods_sheets(readODS::write_ods(iris)), NA)
    expect_error(readODS::list_fods_sheets(readODS::write_ods(iris))) ## err
    temp_fods <- tempfile(fileext = ".fods")
    expect_error(readODS::list_ods_sheets(readODS::write_ods(iris, temp_fods), ods_format = "ods"), NA)
    expect_error(readODS::list_ods_sheets(readODS::write_ods(iris, temp_fods), guess = TRUE), NA)
})

library("datasets")
test_that("Single column ODS", {
    expect_true(write_ods(mtcars, "test.ods") %in% dir())
})

test_that("read_ods verbose switch", {
    expect_silent(read_ods('../testdata/col_types.ods'))
    expect_message(read_ods('../testdata/col_types.ods', verbose = TRUE))
})

## test_that("write_ods verbose switch", {
##     expect_silent(write_ods(iris, 'iris.ods'))
##     expect_message(write_ods(iris, 'iris.ods', verbose = TRUE))
## })

## unlink("iris.ods")

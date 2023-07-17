test_that("read_ods verbose switch", {
    expect_silent(read_ods('../testdata/col_types.ods'))
    expect_message(read_ods('../testdata/col_types.ods', verbose = TRUE))
})

test_that("multiline values", {
    x <- read_ods('../testdata/multiline_cells.ods', col_names = FALSE)
    expect_equal(x[1, 1], "Multiline cell, line 1\nMultiline cell, line 2")
})

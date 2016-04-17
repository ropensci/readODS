test_that("col_types ODS", {
    x <- read_ods('../testdata/col_types.ods', col_types = NA)
    expect_equal(class(x[,2]), "character")
    x <- read_ods('../testdata/col_types.ods')
    expect_equal(class(x[,2]), "integer")
    x <- read_ods('../testdata/col_types.ods', col_types = 'cic')
    expect_equal(class(x[,3]), "character")
})

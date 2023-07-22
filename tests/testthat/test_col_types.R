test_that("col_types ODS", {
    x <- read_ods('../testdata/col_types.ods', col_types = NA)
    expect_equal(class(x[,2]), "character")
    x <- read_ods('../testdata/col_types.ods')
    expect_equal(class(x[,2]), "numeric")
})

### test for issue #41
test_that("multi col_types ODS", {
    x <- read_ods('../testdata/col_types.ods', col_types = NA)
    expect_equal(class(x[,2]), "character")
    x <- read_ods('../testdata/col_types.ods', col_types = readr::cols(cola = 'c', colb = 'c', colc = 'i'))
    expect_equal(class(x[,2]), "character")
})

### throw an error if col_types is not col_spec, single value NA or single value NULL

test_that("col_types throw error", {
    expect_error(read_ods('../testdata/col_types.ods', col_types = 123))
    expect_error(read_ods('../testdata/col_types.ods', col_types = c(NA, NA)))
    expect_error(read_ods('../testdata/col_types.ods', col_types = c(NA, 123)))
})
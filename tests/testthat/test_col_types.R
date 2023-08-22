### throw an error if col_types is not col_spec, single value NA or single value NULL

test_that("col_types throw error", {
    expect_error(read_ods('../testdata/col_types.ods', col_types = 123))
    expect_error(read_ods('../testdata/col_types.ods', col_types = c(NA, NA)))
    expect_error(read_ods('../testdata/col_types.ods', col_types = c(NA, 123)))
    expect_error(x <- read_ods("../testdata/col_types.ods", col_types = TRUE))
    expect_error(x <- read_ods("../testdata/col_types.ods", col_types = 123))

})

test_that("col_types ODS", {
    x <- read_ods('../testdata/col_types.ods', col_types = NA, as_tibble = TRUE)
    expect_equal(class(x[[2]]), "character")
    x <- read_ods('../testdata/col_types.ods')
    expect_equal(class(x[[2]]), "numeric")
    x <- read_ods('../testdata/col_types.ods', col_types = NA, as_tibble = FALSE)
    expect_equal(class(x[,2]), "character")
    x <- read_ods('../testdata/col_types.ods', as_tibble = FALSE)
    expect_equal(class(x[,2]), "numeric")
})

### test for issue #41
test_that("multi col_types ODS", {
    x <- read_ods('../testdata/col_types.ods', col_types = NA, as_tibble = FALSE)
    expect_equal(class(x[,2]), "character")
    x <- read_ods('../testdata/col_types.ods', col_types = readr::cols(cola = 'c', colb = 'c', colc = 'i'), as_tibble = FALSE)
    expect_equal(class(x[,2]), "character")
    x <- read_ods('../testdata/col_types.ods', col_types = NA, as_tibble = TRUE)
    expect_equal(class(x[[2]]), "character")
    x <- read_ods('../testdata/col_types.ods', col_types = readr::cols(cola = 'c', colb = 'c', colc = 'i'), as_tibble = TRUE)
    expect_equal(class(x[[2]]), "character")
})

test_that("col_types shorthand", {
    x <- read_ods("../testdata/col_types.ods") # col_types = NULL
    expect_equal(class(x[[1]]), "character")
    expect_equal(class(x[[2]]), "numeric")
    expect_equal(class(x[[3]]), "numeric")
    x <- read_ods("../testdata/col_types.ods", col_types = NA)
    expect_equal(class(x[[1]]), "character")
    expect_equal(class(x[[2]]), "character")
    expect_equal(class(x[[3]]), "character")
    x <- read_ods("../testdata/col_types.ods", col_types = "fii")
    expect_equal(class(x[[1]]), "factor")
    expect_equal(class(x[[2]]), "integer")
    expect_equal(class(x[[3]]), "integer")
    x <- read_ods("../testdata/col_types.ods", col_types = "cii")
    expect_equal(class(x[[1]]), "character")
    expect_equal(class(x[[2]]), "integer")
    expect_equal(class(x[[3]]), "integer")
    ## https://github.com/tidyverse/readr/issues/1509
    ## readr::type_convert(df, col_types = "f_")
    ## x <- read_ods("../testdata/col_types.ods", col_types = "fi_")
    ## expect_equal(class(x[,1]]), "factor")
    ## expect_equal(class(x[,2]]), "integer")
    ## expect_equal(ncol(x), 3)
})

test_that("col_types orthodox", {
    x <- read_ods('../testdata/col_types.ods', col_types = readr::cols(cola = readr::col_factor(), colb = readr::col_integer(), colc = readr::col_integer()), as_tibble = TRUE)
    expect_equal(class(x[[1]]), "factor")
    expect_equal(class(x[[2]]), "integer")
    expect_equal(class(x[[2]]), "integer")
    x <- read_ods('../testdata/col_types.ods', col_types = readr::cols(cola = readr::col_character(), colb = readr::col_integer(), colc = readr::col_integer()), as_tibble = TRUE)
    expect_equal(class(x[[1]]), "character")
    expect_equal(class(x[[2]]), "integer")
    expect_equal(class(x[[2]]), "integer")
})


test_that("col_types list", {
    x <- read_ods("../testdata/col_types.ods", col_types = list(cola = "f", colb = "i", colc = "i"))
    expect_equal(class(x[[1]]), "factor")
    expect_equal(class(x[[2]]), "integer")
    expect_equal(class(x[[3]]), "integer")
    x <- read_ods("../testdata/col_types.ods", col_types = list(cola = "c", colb = "i", colc = "i"))
    expect_equal(class(x[[1]]), "character")
    expect_equal(class(x[[2]]), "integer")
    expect_equal(class(x[[3]]), "integer")
})

test_that("Incorrect Argument", {
    expect_error(read_ods(), "No file path was")
    expect_error(read_ods(path = "not/real/file.ods"), "file does not exist")
    expect_error(read_ods(path = "../testdata/sum.ods", col_names = "a"), "col_names must be of type `boolean`")
    expect_error(read_ods(path = '../testdata/sum.ods', col_types = "a"), "Unknown col_types. Can either be a class col_spec, NULL or NA.")
    expect_error(read_ods(path = "../testdata/sum.ods", skip = -1), "skip must be a positive integer")
    expect_error(read_ods(path = "../testdata/sum.ods", formula_as_formula = "a"), "formula_as_formula must be of type `boolean`")
    expect_error(read_ods(path = "../testdata/sum.ods", row_names = "a"), "row_names must be of type `boolean`")
    expect_error(read_ods(path = "../testdata/sum.ods", strings_as_factors = "a"), "strings_as_factors must be of type `boolean`")
    expect_error(read_ods(path = "../testdata/sum.ods", check_names = "a"), "check_names must be of type `boolean`")
    expect_error(read_ods(path = "../testdata/sum.ods", verbose = "a"), "verbose must be of type `boolean`")
})

test_that("Single column ODS", {
    single_col <- read_ods('../testdata/sum.ods', sheet = 1)
    expect_equal(ncol(single_col),1)
    expect_equal(colnames(single_col), c("1"))
    expect_warning(read_ods('../testdata/sum.ods', sheet = 1, row_names = TRUE), "Cannot make")
})

test_that("Single row ODS", {
    expect_warning(single_row <- read_ods('../testdata/onerow.ods', sheet = 1), "Cannot make")
    expect_equal(nrow(single_row), 1)
    expect_equal(single_row[1,1], 1)
})

test_that("Single column range", {
      expect_error(read_ods("../testdata/starwars.ods", range="A1:A5"), NA)
})

test_that("read_ods works with all kind of character encodings", {
    expect_error(read_ods('../testdata/wild_character_encoding.ods', sheet='évaluation'),  NA) # é as e and accent
    expect_error(read_ods('../testdata/wild_character_encoding.ods', sheet='évaluation 2'), NA) # é as one character
})

test_that("read_ods reads decimals properly with comma", {
    df <- read_ods('../testdata/decimal_comma.ods')
    df_expected <- structure(list(A = 3.4, B = 2.3, C = 0.03),
                           .Names = c("A", "B", "C"),
                           row.names = 1L, class = "data.frame")
    expect_equal(df, df_expected)
})

test_that("eating space issue #74", {
    df <- read_ods("../testdata/eating_spaces.ods", sheet = 2, col_names = FALSE)
    expect_equal(df[1,1], "A   B")
    df <- read_ods("../testdata/eating_spaces.ods", sheet = 3, col_names = FALSE)
    expect_equal(df[1,1], "A    B C")
    df <- read_ods("../testdata/eating_spaces.ods", sheet = 4, col_names = FALSE)
    expect_equal(df[1,1], "A     B   C")
    df <- read_ods("../testdata/eating_spaces.ods", sheet = 5, col_names = FALSE)
    expect_equal(df[1,1], "A     B\nC")
})

test_that("skip", {
    expect_silent(x <- read_ods("../testdata/starwars.ods", skip = 0))
    expect_equal(nrow(x), 10)
    expect_silent(x <- read_ods("../testdata/starwars.ods", skip = 1, col_names = FALSE))
    expect_equal(nrow(x), 10)
    expect_warning(x <- read_ods("../testdata/starwars.ods", skip = 11), "empty sheet")
    expect_equal(nrow(x), 0)
})

test_that("Check names works properly", {
    expect_silent(x <- read_ods("../testdata/test_naming.ods"))
    expect_equal(colnames(x), c("a", "a", "Var.3"))
    expect_silent(x <- read_ods("../testdata/test_naming.ods", check_names = TRUE))
    expect_equal(colnames(x), c("a", "a.1", "Var.3"))
})
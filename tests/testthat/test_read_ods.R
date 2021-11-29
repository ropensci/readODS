test_that("No path", {
    expect_error(read_ods(), "No file path was provided for the")
})

test_that("Single column ODS", {
    single_col <- read_ods('../testdata/sum.ods', sheet = 1)
    expect_equal(ncol(single_col),1)
    expect_equal(colnames(single_col), c("1"))
})

test_that('read_ods works with all kind of character encodings', {
  expect_error(read_ods('../testdata/wild_character_encoding.ods', sheet='évaluation'),   NA) # é as e and accent
  expect_error(read_ods('../testdata/wild_character_encoding.ods', sheet='évaluation 2'), NA) # é as one character
})

test_that('read_ods reads decimals properly with comma', {
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

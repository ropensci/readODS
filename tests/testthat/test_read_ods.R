test_that("Single column ODS", {
    single_col <- read_ods('../testdata/sum.ods', sheet = 1)
    expect_equal(ncol(single_col),1)
    expect_equal(colnames(single_col), c("1"))
})

test_that('read_ods works with all kind of character encodings', {
  expect_error(read_ods('../testdata/wild_character_encoding.ods', sheet='évaluation'),   NA) # é as e and accent
  expect_error(read_ods('../testdata/wild_character_encoding.ods', sheet='évaluation 2'), NA) # é as one character
})

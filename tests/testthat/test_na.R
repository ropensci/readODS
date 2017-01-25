test_that("Expected na behavior", {
    na_res <- read_ods('../testdata/na_test.ods', na = c("3", "999", "missing"))
    expect_true(is.na(na_res[4,1]))
    expect_true(is.na(na_res[4,2]))
    expect_true(is.na(na_res[3,3]))
    expect_true(is.na(na_res[4,3]))
})

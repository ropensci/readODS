test_that("issue 81 basic test", {
    file <- "../testdata/issue81.ods"    
    res <- read_ods(file, sheet = 2, skip = 4)
    testthat::expect_equal(sum(is.na(res[,1])), 0)
})

## This issue actually only affects ods files created by Excel.
## excel_repeat.ods is created with MS Office 365 online

test_that("issue 81, correctness", {
    res <- read_ods("../testdata/excel_repeat.ods", col_names = FALSE)
    expect_identical(res[,1], c(rep("A", 12), rep("C", 11)))
    expect_identical(res[,2], c(rep("B", 12), rep("D", 11)))
})

test_that("issue 81 real test", {
    file <- "../testdata/issue81.ods"
    res <- read_ods(file, sheet = 2, skip = 4)
    expect_equal(sum(is.na(res[,1])), 0)
})

test_that("issue 84", {
    file <- "../testdata/issue81.ods"
    expect_error(read_ods(file, sheet = "Leavers"), NA)
})
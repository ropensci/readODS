test_that("Sheets are listed correctly", {
  expect_equal(list_ods_sheets("../testdata/linkeddata.ods"),
    c("Own", "contains_linked_data"))
  expect_equal(list_ods_sheets("../testdata/linkeddata.ods",
    include_external_data = TRUE),
    c("Own",
    "contains_linked_data",
    "'file:///D:/Users/peter.brohan/Documents/R/readODScpp/tests/testdata/linksource.xlsx'#Sheet1"))
})

test_that("fods works as well", {
  expect_equal(list_fods_sheets("../testdata/flat.fods"),
    c("Sheet1", "Sheet2"))
})

test_that("keep `ods_sheets` #131", {
    expect_warning(y <- ods_sheets("../testdata/linkeddata.ods"))
    expect_equal(y,
                 c("Own",
                   "contains_linked_data",
                   "'file:///D:/Users/peter.brohan/Documents/R/readODScpp/tests/testdata/linksource.xlsx'#Sheet1"))
})

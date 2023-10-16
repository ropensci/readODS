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

test_that("keep `ods_sheets` #131 #133", {
    expect_silent(y <- ods_sheets("../testdata/linkeddata.ods"))
    expect_equal(y,
                 c("Own",
                   "contains_linked_data",
                   "'file:///D:/Users/peter.brohan/Documents/R/readODScpp/tests/testdata/linksource.xlsx'#Sheet1"))
})

test_that("fix #157", {
    skip_if(file.access("~/", 2) != 0)
    file.copy("../testdata/flat.fods", "~/flat_you_must_not_use_this_path.fods")
    expect_equal(list_fods_sheets("~/flat_you_must_not_use_this_path.fods"),
                 c("Sheet1", "Sheet2"))
    on.exit(unlink("~/flat_you_must_not_use_this_path.fods"))
})

test_that("fix #163", {
    rubbish_file <- tempfile(fileext = ".fods")
    writeLines("<ul><li>hello</li></ul>", rubbish_file)
    expect_error(list_fods_sheets(rubbish_file)) ## won't stat in infinite loop
})

test_that("fix #169", {
    expect_error(list_ods_sheets("this_surely_not_exists.ods"), "does not exist")
    expect_error(list_fods_sheets("this_surely_not_exists.fods"), "does not exist")
})

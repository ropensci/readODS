test_that("Read fods", {
  expect_silent(a <- read_fods("../testdata/flat.fods"))
  expect_equal(a[1,1], "A2")
  b <- read_fods("../testdata/flat.fods", range = "Sheet2!B2:D3")
  expect_equal(b[1,3], "S2D3")
})

test_that("Error when not correct", {
  expect_error(read_fods("../testdata/sum.ods"))
  expect_error(read_fods("../testdata/notreal.ods"))
})

test_that("Return blank/error if mangled FODS", {
  expect_warning(read_fods("../testdata/norows.fods"))
  expect_warning(read_fods("../testdata/nocells.fods"))
})
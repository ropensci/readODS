test_that("Read fods", {
  expect_silent(a <- read_fods("../testdata/flat.fods"))
  expect_equal(a[[1,1]], "A2")
  b <- read_fods("../testdata/flat.fods", range = "Sheet2!B2:D3")
  expect_equal(b[[1,3]], "S2D3")
})

test_that("Error when not correct", {
  expect_error(read_fods("../testdata/sum.ods"))
  expect_error(read_fods("../testdata/notreal.ods"))
})

test_that("fix #157", {
    skip_if(file.access("~/", 2) != 0)
    file.copy("../testdata/flat.fods", "~/flat_you_must_not_use_this_path.fods")
    expect_error(read_fods("~/flat_you_must_not_use_this_path.fods"), NA)
    expect_error(read_fods("~/flat_you_must_not_use_this_path_not.fods"))
    on.exit(unlink("~/flat_you_must_not_use_this_path.fods"))
})

## default

test_that("Return blank/error if mangled FODS", {
    expect_silent(read_fods("../testdata/norows.fods"))
    expect_silent(read_fods("../testdata/nocells.fods"))
})

## V2.0.0 behavior: backward compatibility

ori_option <- getOption("readODS.v200") ## probably NULL
options("readODS.v200" = TRUE)

test_that("Return blank/error if mangled FODS v2.0.0", {
    expect_warning(read_fods("../testdata/norows.fods"))
    expect_warning(read_fods("../testdata/nocells.fods"))
})

options("readODS.v200" = ori_option)

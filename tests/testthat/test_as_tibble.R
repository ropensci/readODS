test_that("as_tibble works", {
  expect_error(read_ods("../testdata/sum.ods", as_tibble = "true"))
  x <- read_ods("../testdata/multisheet.ods", as_tibble = TRUE)
  expect_equal(tibble::is_tibble(x), TRUE)
  x <- read_ods("../testdata/multisheet.ods", sheet = 2, col_names = FALSE, as_tibble = TRUE)
  expect_equal(x[[1, "...1"]], "COOKIES")
  expect_equal(x[[4, "...2"]], 1)
  expect_equal(typeof(x[[4, "...2"]]), "double")
})

test_that(".name_repair works", {
  expect_error(read_ods("../testdata/test_naming.ods",
    as_tibble = TRUE,
    .name_repair = "error"))
  expect_error(read_ods("../testdata/test_naming.ods", as_tibble = TRUE, .name_repair = "check_unique"))
  expect_error(x <- read_ods("../testdata/test_naming.ods", as_tibble = TRUE), NA)
  expected_names <- c("a...1", "a...2", "...3")
  expect_equal(colnames(x), expected_names)
})
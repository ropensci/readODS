test_that("as_tibble works", {
  expect_error(read_ods("../testdata/sum.ods", as_tibble = "true"))
  x <- read_ods("../testdata/multisheet.ods", as_tibble = TRUE)
  expect_equal(tibble::is_tibble(x), TRUE)
  x <- read_ods("../testdata/multisheet.ods", sheet = 2, col_names = FALSE, as_tibble = TRUE)
  expect_equal(x[[1, "A"]], "COOKIES")
  expect_equal(x[[4, "B"]], 1)
  expect_equal(typeof(x[[4, "B"]]), "double")
})

test_that(".name_repair words", {
  expect_error(read_ods("../testdata/test_naming.ods",
    as_tibble = TRUE,
    .name_repair = "error"),
  ".name_repair must either")
  expect_error(read_ods("../testdata/test_naming.ods", as_tibble = TRUE))
  expect_error(x <- read_ods("../testdata/test_naming.ods", as_tibble = TRUE, .name_repair = "unique"), NA)
  expected_names <- c("a...1", "a...2", "Var.3")
  expect_equal(colnames(x), expected_names)
})
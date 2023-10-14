test_that("Incorrect Argument", {
    expect_error(read_ods(), "No file path was")
    expect_error(read_ods(path = "not/real/file.ods"), "file does not exist")
    expect_error(read_ods(path = "../testdata/sum.ods", col_names = "a"), "col_names must be of type `boolean`")
    expect_error(read_ods(path = "../testdata/sum.ods", skip = -1), "skip must be a positive integer")
    expect_error(read_ods(path = "../testdata/sum.ods", formula_as_formula = "a"), "formula_as_formula must be of type `boolean`")
    expect_error(read_ods(path = "../testdata/sum.ods", row_names = "a"), "row_names must be of type `boolean`")
    expect_error(read_ods(path = "../testdata/sum.ods", strings_as_factors = "a"), "strings_as_factors must be of type `boolean`")
    expect_error(read_ods(path = "../testdata/sum.ods", verbose = "a"), "verbose must be of type `boolean`")
    expect_error(read_ods(path = "../testdata/sum.ods", row_names = TRUE), "Tibbles do not support")
})

test_that("exceptions in C++ (hard to test)", {
    tempzip <- tempfile(fileext = ".zip")
    suppressWarnings(zip::zip(tempzip, "../testthat/test_read_ods.R"))
    expect_error(read_ods(tempzip), "is not a correct ODS file")
    expect_error(read_ods(path = "../testdata/sum.ods", sheet = -1), "Cannot have sheet index less than 1")
})

test_that("Single column range", {
      expect_error(read_ods("../testdata/starwars.ods", range="A1:A5"), NA)
})

test_that("read_ods works with all kind of character encodings", {
    expect_error(read_ods('../testdata/wild_character_encoding.ods', sheet='évaluation'),  NA) # é as e and accent
    expect_error(read_ods('../testdata/wild_character_encoding.ods', sheet='évaluation 2'), NA) # é as one character
})

test_that("read_ods reads decimals properly with comma", {
    df <- read_ods('../testdata/decimal_comma.ods', as_tibble = FALSE)
    df_expected <- structure(list(A = 3.4, B = 2.3, C = 0.03),
                           .Names = c("A", "B", "C"),
                           row.names = 1L, class = "data.frame")
    expect_equal(df, df_expected)
})

test_that("eating space issue #74", {
    df <- read_ods("../testdata/eating_spaces.ods", sheet = 2, col_names = FALSE)
    expect_equal(df[[1,1]], "A   B")
    df <- read_ods("../testdata/eating_spaces.ods", sheet = 3, col_names = FALSE)
    expect_equal(df[[1,1]], "A    B C")
    df <- read_ods("../testdata/eating_spaces.ods", sheet = 4, col_names = FALSE)
    expect_equal(df[[1,1]], "A     B   C")
    df <- read_ods("../testdata/eating_spaces.ods", sheet = 5, col_names = FALSE)
    expect_equal(df[[1,1]], "A     B\nC")
})

test_that("Check .name_repair works properly", {
    expect_silent(x <- read_ods("../testdata/test_naming.ods", .name_repair = "minimal"))
    expect_equal(colnames(x), c("a", "a", ""))
    expect_error(x <- read_ods("../testdata/test_naming.ods", .name_repair = "check_unique"))
    expect_message(x <- read_ods("../testdata/test_naming.ods", .name_repair = "unique"))
    expect_equal(colnames(x), c("a...1", "a...2", "...3"))
})

test_that("Parses range inputs correctly", {
    expect_warning(x <- read_ods("../testdata/multisheet.ods", sheet = 3, range = "Sheet2!B4:D9"), "Sheet suggested in range and using sheet")
    expect_equal(x[[2,2]], 2)
    expect_message(x <- read_ods("../testdata/multisheet.ods", range = "Sheet3!D2:E4"))
    expect_equal(x[[1,1]], 3)
})

test_that("Deals with repeated spaces correctly when fetching only part of sheet",{
    df <- data.frame("...1" = c(1, NA, NA, NA),
                    "...2" = c(NA, NA, 2, NA),
                    "...3" = c(NA, NA, NA, NA),
                    "...4" = c(NA, NA, NA, 3))
    expect_equal(read_ods("../testdata/multisheet.ods", range = "Sheet2!B4:E7", col_names = FALSE, as_tibble = FALSE), df)
    expect_equal(read_ods("../testdata/excel_repeat.ods", range = "A9:B18", col_names = FALSE)[[5,1]], "C")
})

test_that("read with column headers", {
    expect_silent(x <- read_ods("../testdata/starwars.ods", col_names = TRUE))
    expect_equal(ncol(x), 3)
    expect_equal(nrow(x), 10)
    expect_silent(x <- read_ods("../testdata/starwars.ods", row_names = TRUE, as_tibble = FALSE))
    expect_equal(ncol(x), 2)
    expect_equal(colnames(x), c("homeworld", "species"))
})

## default single behavior
test_that("Single column ODS", {
    single_col <- read_ods('../testdata/sum.ods', sheet = 1)
    expect_equal(ncol(single_col),1)
    expect_equal(colnames(single_col), c("1"))
    expect_equal(nrow(read_ods('../testdata/sum.ods', sheet = 1, row_names = TRUE, as_tibble = FALSE)), 0)
})

test_that("Single row ODS", {
    expect_silent(single_row <- read_ods('../testdata/onerow.ods', sheet = 1))
    expect_equal(nrow(single_row), 0)
})

test_that("skip", {
    expect_silent(x <- read_ods("../testdata/starwars.ods", skip = 0))
    expect_equal(nrow(x), 10)
    expect_message(x <- read_ods("../testdata/starwars.ods", skip = 1, col_names = FALSE))
    expect_equal(nrow(x), 10)
    expect_silent(x <- read_ods("../testdata/starwars.ods", skip = 11))
    expect_equal(nrow(x), 0)
})

test_that("No Warning of empty sheet", {
    expect_silent(read_ods("../testdata/empty.ods"))
    expect_silent(read_fods("../testdata/empty.fods"))
})

## V2.0.0 behavior: backward compatibility

test_that("Single column ODS v2.0.0", {
    withr::with_options(list(readODS.v200 = TRUE), {
        single_col <- read_ods("../testdata/sum.ods", sheet = 1)
        expect_equal(ncol(single_col),1)
        expect_equal(colnames(single_col), c("1"))
        expect_warning(read_ods("../testdata/sum.ods", sheet = 1, row_names = TRUE, as_tibble = FALSE), "Cannot make")
    })
})

test_that("Single row ODS v2.0.0", {
    withr::with_options(list(readODS.v200 = TRUE), {
        expect_warning(single_row <- read_ods("../testdata/onerow.ods", sheet = 1), "Cannot make")
        expect_equal(nrow(single_row), 1)
        expect_equal(single_row[[1,1]], 1)
    })
})

test_that("skip v2.0.0", {
    withr::with_options(list(readODS.v200 = TRUE), {
        expect_silent(x <- read_ods("../testdata/starwars.ods", skip = 0))
        expect_equal(nrow(x), 10)
        expect_message(x <- read_ods("../testdata/starwars.ods", skip = 1, col_names = FALSE))
        expect_equal(nrow(x), 10)
        expect_warning(x <- read_ods("../testdata/starwars.ods", skip = 11), "empty sheet")
        expect_equal(nrow(x), 0)
    })
})

test_that("Warns of empty sheet", {
    withr::with_options(list(readODS.v200 = TRUE), {
        expect_warning(read_ods("../testdata/empty.ods"))
        expect_warning(read_fods("../testdata/empty.fods"))
    })
})

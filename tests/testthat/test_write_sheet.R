## These can't be tested with write_ods and then read_ods

## keep here for the defaults
.write_sheet_ <- function(x, filename, sheet = "Sheet1", row_names = FALSE, col_names = FALSE, na_as_string = FALSE, padding = FALSE, header = "", footer = "") {
    write_sheet_file_(filename = filename, x = x,
                 sheet_name = sheet,
                 row_names = row_names, col_names = col_names,
                 na_as_string = na_as_string, padding = padding, header = header, footer = footer)
    return(invisible(filename))
}

test_that("padding TRUE, ncol <= 1024", {
    withr::with_tempfile("throwaway_xml_file", {
        test_data <- data.frame(x = c(1.1, 2.2, 3.3), y = c("a", "b", "c"))
        .write_sheet_(test_data, filename = throwaway_xml_file, padding = TRUE)
        content <- readLines(throwaway_xml_file, warn = FALSE)
        expect_true(any(grepl("table:number-columns-repeated=\"1024\"", content)))
        ## 1024 - ncol(test_data)
        expect_true(any(grepl("table:number-columns-repeated=\"1022\"", content)))
        ## 2^20 - 3
        expect_true(any(grepl("table:number-rows-repeated=\"1048573\"", content)))
    }, fileext = ".xml")
})

test_that("padding FALSE, ncol <= 1024", {
    withr::with_tempfile("throwaway_xml_file", {
        test_data <- data.frame(x = c(1.1, 2.2, 3.3), y = c("a", "b", "c"))
        .write_sheet_(test_data, throwaway_xml_file, padding = FALSE)
        content <- readLines(throwaway_xml_file, warn = FALSE)
        expect_false(any(grepl("table:number-columns-repeated=\"1024\"", content)))
        ## 1024 - ncol(test_data)
        expect_false(any(grepl("table:number-columns-repeated=\"1022\"", content)))
        ## 2^20 - 3
        expect_false(any(grepl("table:number-rows-repeated=\"1048573\"", content)))
    }, fileext = ".xml")
})

## padding > 1024 cols

test_that("padding TRUE, ncol > 1024", {
    withr::with_tempfile("throwaway_xml_file", {
        test_data <- as.data.frame(matrix(rnorm(1025), nrow = 1))
        .write_sheet_(test_data, throwaway_xml_file, padding = TRUE)
        content <- readLines(throwaway_xml_file, warn = FALSE)
        expect_true(any(grepl("table:number-columns-repeated=\"16384\"", content)))
        ## 16384 - ncol(test_data)
        expect_true(any(grepl("table:number-columns-repeated=\"15359\"", content)))
        ## 2^20 - 1
        expect_true(any(grepl("table:number-rows-repeated=\"1048575\"", content)))
    }, fileext = ".xml")
})

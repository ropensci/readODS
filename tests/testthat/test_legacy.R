test_that("read_ods", {
    file <- "../testdata/sum.ods"
    expect_equal(read_ods(file, sheet=1, col_names = FALSE, formula_as_formula=TRUE)[[3,1]],"of:=SUM([.A1:.A2])")
    expect_equal(read_ods(file, sheet=1, col_names = FALSE, col_types = NA, formula_as_formula=FALSE)[[3,1]],"3")
  
    df <- data.frame("...1" = as.character(1:3),stringsAsFactors = F)
    rODS <- read_ods(file, sheet = 1, col_names = FALSE, col_types=NA, formula_as_formula = FALSE, as_tibble = FALSE)
    expect_equal(rODS, df)
  
    file <- "../testdata/lotsofnothing_test.ods"
    expect_equal(dim(read_ods(file, sheet = 1, col_names = FALSE)),c(21,13)) # test if empty rows at the end are ignored
    expect_equal(inherits(read_ods(file, sheet = 1), "tbl_df"), TRUE)
    ## small file
    file <- "../testdata/table.ods"
    df <- data.frame("...1" = c("gender", "m", "f", "m"),
        "...2" = c("visit1", "4", "8", "8"),
        "...3" = c("visit2", "6", "9", "2"),
        "...4" = c("visit3", "8", "4", "1"),
        stringsAsFactors = F)
    expect_equal(read_ods(file, sheet = 1, col_names = FALSE, col_types = NA, as_tibble = FALSE), df)
    
    file <- "../testdata/layout_test.ods"
    sheet1 <- read_ods(file, sheet = 1, col_names = FALSE)
    expect_equal(sheet1[[8, "...6"]], "empty") # this is a repeated element
    
    sheet2 <- read_ods(file, sheet=2, col_names = FALSE)
    expect_equal(dim(sheet2),c(22,13))
    expect_true(all(sheet1[21,]==sheet2[22,]))
    
    file <- paste("../testdata/multisheet.ods",sep="")
    df <- data.frame(matrix(as.character(NA),14,7), stringsAsFactors = F)
    df[1,1] <- "COOKIES"
    df[4,2] <- "1"
    df[6,3] <- "2"
    df[8,3] <- "3"
    df[14,4] <- "3"
    df[7,5] <- "3"
    df[9,5] <- "1"
    df[10,7] <- "1"
    sheet2 <- read_ods(file, sheet=2, col_names = FALSE, as_tibble = FALSE)
    expect_true(all(sheet2==df, na.rm = TRUE))
 
    file <- "../testdata/1996-2000.ods"
    expect_true(all(dim(read_ods(file, sheet = 2, col_names = FALSE)) == c(36, 21)))
})

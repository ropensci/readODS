library("datasets")
test_that("Single column ODS", {
    expect_true(write_ods(mtcars, "test.ods") %in% dir())
    file.remove("test.ods")
})

test_that("overwrite, #54", {
    data("faithful")
    write_ods(faithful, "mydata.ods")
    data("CO2")
    write_ods(CO2, "mydata.ods")
    content_in_file <- read_ods('mydata.ods')
    expect_true(nrow(content_in_file) == 84)
    ## cleanup
    file.remove("mydata.ods")
})

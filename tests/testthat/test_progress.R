test_that("readODs_progress, normal case", {
    ## pretend to be interactive
    local_mocked_bindings(.is_interactive = function(...) TRUE)
    expect_true(readODS_progress())
})

test_that("readODs_progress, suppress in batch", {
    ## pretend to be batch
    local_mocked_bindings(.is_interactive = function(...) FALSE)
    expect_false(readODS_progress())
})

test_that("readODS_progress, suppression by options", {
    ## pretend to be interactive
    local_mocked_bindings(.is_interactive = function(...) TRUE)

    withr::with_options(list(readODS.show_progress = FALSE), {
        expect_false(readODS_progress())
    })
    withr::with_options(list(knitr.in.progress = TRUE), {
        expect_false(readODS_progress())
    })
    withr::with_envvar(new = c("RSTUDIO_NOTEBOOK" = "123"), {
        expect_false(readODS_progress())
    })
    withr::with_envvar(new = c("RSTUDIO_NOTEBOOK" = ""), {
        expect_true(readODS_progress())
    })
})

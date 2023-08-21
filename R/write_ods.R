.zip_tmp_to_path <- function(temp_ods_dir, path, overwrite = TRUE) {
    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(temp_ods_dir)
    zip::zip(basename(path), include_directories = FALSE, recurse = TRUE, files = dir(), mode = "cherry-pick")
    setwd(wd)
    file.copy(file.path(temp_ods_dir, basename(path)), path, overwrite = overwrite)
}

.find_sheet_node_by_sheet <- function(spreadsheet_node, sheet) {
    sheet_node <- NULL
    for (i in seq(2, length(xml2::xml_children(spreadsheet_node)))) {
        if (!is.na(xml2::xml_attr(xml2::xml_children(spreadsheet_node)[[i]], "name") == sheet) &&
            xml2::xml_attr(xml2::xml_children(spreadsheet_node)[[i]], "name") == sheet) {
            sheet_node <- xml2::xml_children(spreadsheet_node)[[i]]
        }
    }
    return(sheet_node)
}

.silent_read_xml <- function(x) {
    suppressWarnings({
        return(xml2::read_xml(x))
    })
}

.silent_add_sheet_node <- function(sheet) {
    .silent_read_xml(sprintf('<table:table table:name="%s" table:style-name="ta1"><table:table-column table:style-name="co1" table:number-columns-repeated="16384" table:default-cell-style-name="ce1"/></table:table>', sheet))
}

## CREATION OF sysdata
## .CONTENT <- readLines("benchmark/header.xml")
## .FOOTER <- readLines("benchmark/footer.xml")
## .FODS_HEADER <- paste(readLines("benchmark/fods_header.xml"), collapse = "\n")
## .FODS_FOOTER <- paste(readLines("benchmark/fods_footer.xml"), collapse = "\n")
## usethis::use_data(.CONTENT, .FOOTER, .FODS_HEADER, .FODS_FOOTER, internal = TRUE, overwrite = TRUE)

.convert_df_to_sheet <- function(x, sheet = "Sheet1", row_names = FALSE, col_names = FALSE, na_as_string = FALSE, padding = FALSE) {
    throwaway_xml_file <- file.path(tempfile(fileext = ".xml"))
    write_sheet_(x = x, filename = throwaway_xml_file, sheet = sheet, row_names = row_names, col_names = col_names,
                  na_as_string = na_as_string, padding = padding,
                  header = "",
                  footer = "")
}

## https://github.com/ropensci/readODS/issues/88
.vfwrite_ods <- function(x, temp_ods_dir, sheet = "Sheet1", row_names = FALSE, col_names = TRUE, na_as_string = FALSE, padding = FALSE) {
    templatedir <- system.file("template", package = "readODS")
    file.copy(dir(templatedir, full.names = TRUE), temp_ods_dir, recursive = TRUE, copy.mode = FALSE)
    filename <- file.path(temp_ods_dir, "content.xml")
    write_sheet_(x = x, filename = filename, sheet = sheet, row_names = row_names, col_names = col_names,
                  na_as_string = na_as_string, padding = padding,
                  header = paste0(.CONTENT[1], .CONTENT[2]),
                  footer = .FOOTER)
}

.preprocess_x <- function(x) {
    if (inherits(x, "tbl_df")) { #Convert to a df if currently a tibble
        x <- as.data.frame(x)
    }
    if (!is.data.frame(x)) {
        stop("x must be a data.frame.", call. = FALSE)
    }
    ## Limit writing to only files that Libreoffice and Excel can read
    if (ncol(x) > 16383 || nrow(x) > 2^20) {
        stop("Data exceeds max sheet size of 16383 x 1048576", call. = FALSE)
    }
    return(x)
}

.preprocess_path <- function(path) {
    normalized_path <- normalizePath(path, mustWork = FALSE)
    ## ensure the file can be created
    file.create(normalized_path, showWarnings = FALSE)
    if (!file.exists(normalized_path)) {
        stop("File cannot be created at this path: ", normalized_path, call. = FALSE)
    }
    return(normalized_path)
}

#' Write Data to (F)ODS File
#' @description
#' Function to write a single data.frame to a (f)ods file.
#'
#' @param x a data.frame
#' @param path Path to the (f)ods file to write
#' @param sheet Name of the sheet
#' @param append logical, TRUE indicates that x should be appended to the existing file (path) as a new sheet. If a sheet with the same sheet_name exists, an exception is thrown. See update. Please also note that writing is slower if TRUE. Default is FALSE.
#' @param update logical, TRUE indicates that the sheet with sheet_name in the existing file (path) should be updated with the content of x. If a sheet with sheet_name does not exist, an exception is thrown. Please also note that writing is slower if TRUE. Default is FALSE.
#' @param row_names logical, TRUE indicates that row names of x are to be included in the sheet. Default is FALSE.
#' @param col_names logical, TRUE indicates that column names of x are to be included in the sheet. Default is TRUE.
#' @param na_as_string logical, TRUE indicates that NAs are written as string; FALSE indicates that NAs are written as empty cells.
#' @param padding logical, TRUE indicates that the sheet is padded with repeated empty cells to the maximum size, either 2^20 x 1024 (if the number of columns of `x` is less than or equal 1024) or 2^20 x 16,384 (otherwise). This is the default behaviour of Microsoft Excel. Default is FALSE
#' @return An (F)ODS file written to the file path location specified by the user. The value of \code{path} is also returned invisibly.
#' @details This function emulates [writexl::write_xlsx()] and [openxlsx::write.xlsx()] except in the handling of list columns. The expected behaviour for this is undefined and the two functions behave differently. This function handles list columns by converting them to character vectors of R code (similar to the output of [dput()]), which is probably not ideal.
#' @author Detlef Steuer <steuer@@hsu-hh.de>, Thomas J. Leeper <thosjleeper@@gmail.com>, John Foster <john.x.foster@@nab.com.au>, Chung-hong Chan <chainsawtiney@@gmail.com>
#' @examples
#' \dontrun{
#' # preserve the row names
#' write_ods(mtcars, "mtcars.ods", row_names = TRUE)
#' # append a sheet to an existing file
#' write_ods(PlantGrowth, "mtcars.ods", append = TRUE, sheet = "plant")
#' # write flat ODS file
#' write_fods(mtcars, "mtcars.fods", sheet = "mtcars")
#' }
#' @export
write_ods <- function(x, path = tempfile(fileext = ".ods"), sheet = "Sheet1", append = FALSE, update = FALSE, row_names = FALSE, col_names = TRUE, na_as_string = FALSE, padding = FALSE) {
    ## setup temp directory
    ## one can't just use tempdir() because it is the same in the same session
    temp_ods_dir <- file.path(tempdir(), stringi::stri_rand_strings(1, 20, pattern = "[A-Za-z0-9]"))
    dir.create(temp_ods_dir)
    on.exit(unlink(temp_ods_dir))
    ## x <- .preprocess_x(x)
    if (!file.exists(path) || (!append && !update)) {
        path <- .preprocess_path(path)
        .vfwrite_ods(x = x, temp_ods_dir = temp_ods_dir, sheet = sheet, row_names = row_names, col_names = col_names, na_as_string = na_as_string, padding = padding)
    } else {
        ## The file must be there.
        zip::unzip(path, exdir = temp_ods_dir)
        contentfile <- file.path(temp_ods_dir, "content.xml")
        content <- xml2::read_xml(contentfile)
        spreadsheet_node <- xml2::xml_children(xml2::xml_children(content)[[which(!is.na(xml2::xml_find_first(xml2::xml_children(content),"office:spreadsheet")))]])[[1]]
        sheet_node <- .find_sheet_node_by_sheet(spreadsheet_node, sheet)
        if ((!is.null(sheet_node) && append && !update) || (!is.null(sheet_node) && !update)) {
            ## Sheet exists so we cannot append
            stop(paste0("Sheet ", sheet, " exists. Set update to TRUE is you want to update this sheet."), call. = FALSE)
        }
        if (is.null(sheet_node) && update) {
            stop(paste0("Sheet ", sheet, " does not exist. Cannot update."), call. = FALSE)
        }
        if (!is.null(sheet_node) && update) {
            ## clean up the sheet
            xml2::xml_remove(xml2::xml_children(sheet_node)[2:length(xml2::xml_children(sheet_node))])
        }
        if (is.null(sheet_node) && append) {
            ## Add a new sheet
            sheet_node <- xml2::xml_add_child(spreadsheet_node, .silent_add_sheet_node(sheet))
        }
        throwaway_xml_file <- .convert_df_to_sheet(x = x, sheet = sheet, row_names = row_names, col_names = col_names,
                                                       na_as_string = na_as_string, padding = padding)
        xml2::xml_replace(sheet_node, .silent_read_xml(throwaway_xml_file))
        ## write xml to contentfile
        xml2::write_xml(content, contentfile)
    }
    ## zip up ODS archive
    .zip_tmp_to_path(temp_ods_dir, path)
    invisible(path)
}

#' @rdname write_ods
#' @export
write_fods <- function(x, path = tempfile(fileext = ".fods"), sheet = "Sheet1", row_names = FALSE, col_names = TRUE, na_as_string = FALSE) {
    x <- .preprocess_x(x)
    path <- .preprocess_path(path)
    write_sheet_(filename = path, x = x, sheet = sheet, row_names = row_names, col_names = col_names, na_as_string = na_as_string, padding = FALSE, header = .FODS_HEADER, footer = .FODS_FOOTER)
    invisible(path)
}

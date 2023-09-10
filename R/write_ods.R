## CREATION OF sysdata
## .CONTENT <- readLines("benchmark/header.xml")
## .FOOTER <- readLines("benchmark/footer.xml")
## .FODS_HEADER <- paste(readLines("benchmark/fods_header.xml"), collapse = "\n")
## .FODS_FOOTER <- paste(readLines("benchmark/fods_footer.xml"), collapse = "\n")
## usethis::use_data(.CONTENT, .FOOTER, .FODS_HEADER, .FODS_FOOTER, internal = TRUE, overwrite = TRUE)

.zip_tmp_to_path <- function(temp_ods_dir, path, flat = FALSE) {
    if (flat) {
        return(path) ## do nothing
    }
    temp_ods <- file.path(temp_ods_dir, basename(path))
    zip::zip(zipfile = temp_ods, include_directories = FALSE, recurse = TRUE,
             files = list.files(temp_ods_dir), mode = "cherry-pick",
             root = temp_ods_dir)
    file.copy(temp_ods, path, overwrite = TRUE)
    return(path)
}

.convert_df_to_sheet_file <- function(x, sheet_name = "Sheet1", row_names = FALSE, col_names = FALSE, na_as_string = FALSE, padding = FALSE, xml_file = file.path(tempfile(fileext = ".xml"))) {
    write_sheet_file_(x = x, filename = xml_file, sheet_name = sheet_name, row_names = row_names, col_names = col_names,
                 na_as_string = na_as_string, padding = padding,
                 header = "",
                 footer = "")
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

.update_ods <- function(x, path = tempfile(fileext = ".ods"), sheet = "Sheet1", append = FALSE, update = FALSE, row_names = FALSE, col_names = TRUE, na_as_string = FALSE, padding = FALSE, flat = FALSE, temp_ods_dir = NULL) {
    sheet_name <- sheet
    if (!file.exists(path)) {
        stop("File does not exist: ", path, call. = FALSE)
    }
    if (isFALSE(flat)) {
        zip::unzip(path, exdir = temp_ods_dir)
        contentfile <- file.path(temp_ods_dir, "content.xml")
        sheet_names <- list_ods_sheets(path, include_external_data = TRUE)
    } else {
        contentfile <- path
        sheet_names <- list_fods_sheets(path, include_external_data = TRUE)
    }
    is_in_sheet_names <- stringi::stri_cmp(e1 = sheet_name, e2 = sheet_names) == 0
    sheet_exist <- any(is_in_sheet_names)
    if ((sheet_exist && append && !update) || (sheet_exist && !update)) {
        ## Sheet exists so we cannot append
        stop(paste0("Sheet ", sheet_name, " exists. Set update to TRUE is you want to update this sheet."), call. = FALSE)
    }
    if (!sheet_exist && update) {
        stop(paste0("Sheet ", sheet_name, " does not exist. Cannot update."), call. = FALSE)
    }
    ## numeric
    sheet_index <- which(is_in_sheet_names)
    throwaway_xml_file <- .convert_df_to_sheet_file(x = x, sheet_name = sheet_name, row_names = row_names, col_names = col_names,
                                                    na_as_string = na_as_string, padding = padding)
    if (append) {
        return(splice_sheet_(contentfile, throwaway_xml_file, flat))
    }
    return(update_sheet_(contentfile, throwaway_xml_file, flat, sheet_index))
}

.write_ods <- function(x, path = tempfile(fileext = ".ods"), sheet = "Sheet1", append = FALSE, update = FALSE, row_names = FALSE, col_names = TRUE, na_as_string = FALSE, padding = FALSE, flat = FALSE) {
    sheet_name <- sheet
    if (isFALSE(inherits(x, "data.frame")) && !is.list(x)) {
        stop("x must be data.frame or list.", call. = FALSE)
    }
    if (isFALSE(inherits(x, "data.frame")) && is.list(x) && length(x) == 0) {
        stop("x must contain at least one data.frame.", call. = FALSE)
    }
    temp_ods_dir <- NULL
    if (isFALSE(flat)) {
        temp_ods_dir <- file.path(tempdir(), stringi::stri_rand_strings(1, 30, pattern = "[A-Za-z0-9]"))
        dir.create(temp_ods_dir)
        on.exit(unlink(temp_ods_dir))
    }
    if (append || update) {
        .update_ods(x = .preprocess_x(x), path = path, sheet = sheet, append = append, update = update, row_names = row_names,
                    col_names = col_names, na_as_string = na_as_string, padding = padding, flat = flat,
                    temp_ods_dir = temp_ods_dir)
        return(invisible(.zip_tmp_to_path(temp_ods_dir, path, flat = flat)))
    }
    if (isTRUE(inherits(x, "data.frame"))) {
        x <- .preprocess_x(x)
        .write_fun <- write_sheet_file_
    } else {
        ## it's a list (at least we assume)
        x <- lapply(x, .preprocess_x)
        if (is.null(names(x))) {
            names(x) <- paste0("Sheet", seq_along(x))
        }
        .write_fun <- write_sheet_file_list_
    }
    path <- .preprocess_path(path)
    if (isFALSE(flat)) {
        templatedir <- system.file("template", package = "readODS")
        file.copy(dir(templatedir, full.names = TRUE), temp_ods_dir, recursive = TRUE, copy.mode = FALSE)
        filename <- file.path(temp_ods_dir, "content.xml")
        header <- paste0(.CONTENT[1], .CONTENT[2])
        footer <- .FOOTER
    } else {
        filename <- path
        header <- .FODS_HEADER
        footer <- .FODS_FOOTER
    }
    .write_fun(filename = filename, x = x, sheet_name = sheet_name, row_names = row_names, col_names = col_names, na_as_string = na_as_string, padding = padding, header = header, footer = footer)
    return(invisible(.zip_tmp_to_path(temp_ods_dir, path, flat = flat)))
}

#' Write Data to (F)ODS File
#' @description
#' Function to write a single data frame or a list of data frames to a (f)ods file.
#'
#' @param x data frame or list of data frames that will be sheets in the (f)ods. If the list is named, the names are used as sheet names
#' @param path Path to the (f)ods file to write
#' @param sheet Name of the sheet; ignore if `x` is a list of data frames
#' @param append logical, TRUE indicates that x should be appended to the existing file (path) as a new sheet. If a sheet with the same sheet_name exists, an exception is thrown. See update. Please also note that writing is slightly slower if TRUE. Default is FALSE. Ignore if `x` is a list of data frames
#' @param update logical, TRUE indicates that the sheet with sheet_name in the existing file (path) should be updated with the content of x. If a sheet with sheet_name does not exist, an exception is thrown. Please also note that writing is slightly slower if TRUE. Default is FALSE. Ignore if `x` is a list of data frames
#' @param row_names logical, TRUE indicates that row names of x are to be included in the sheet. Default is FALSE
#' @param col_names logical, TRUE indicates that column names of x are to be included in the sheet. Default is TRUE
#' @param na_as_string logical, TRUE indicates that NAs are written as string; FALSE indicates that NAs are written as empty cells
#' @param padding logical, TRUE indicates that the sheet is padded with repeated empty cells to the maximum size, either 2^20 x 1024 (if the number of columns of `x` is less than or equal 1024) or 2^20 x 16,384 (otherwise). This is the default behaviour of Microsoft Excel. Default is FALSE
#' @return A (F)ODS file written to the file path location specified by the user. The value of \code{path} is also returned invisibly
#' @details This function emulates [writexl::write_xlsx()] and [openxlsx::write.xlsx()] except in the handling of list columns. The expected behaviour for this is undefined and the two functions behave differently. This function handles list columns by converting them to character vectors of R code (similar to the output of [dput()]), which is probably not ideal.
#' @author Detlef Steuer <steuer@@hsu-hh.de>, Thomas J. Leeper <thosjleeper@@gmail.com>, John Foster <john.x.foster@@nab.com.au>, Chung-hong Chan <chainsawtiney@@gmail.com>
#' @examples
#' \dontrun{
#' # preserve the row names
#' write_ods(mtcars, "mtcars.ods", row_names = TRUE)
#' # append a sheet to an existing file
#' write_ods(PlantGrowth, "mtcars.ods", append = TRUE, sheet = "plant")
#' # This is however faster
#' write_ods(list("Sheet1" = mtcars, "plant" = PlantGrowth), "mtcars.ods", row_names = TRUE)
#' # write flat ODS file
#' write_fods(mtcars, "mtcars.fods", sheet = "mtcars")
#' }
#' @export
write_ods <- function(x, path = tempfile(fileext = ".ods"), sheet = "Sheet1", append = FALSE, update = FALSE, row_names = FALSE, col_names = TRUE, na_as_string = FALSE, padding = FALSE) {
    .write_ods(x = x, path = path, sheet = sheet, append = append, update = update, row_names = row_names, col_names = col_names,
               na_as_string = na_as_string, padding = padding, flat = FALSE)
}

#' @rdname write_ods
#' @export
write_fods <- function(x, path = tempfile(fileext = ".fods"), sheet = "Sheet1", append = FALSE, update = FALSE, row_names = FALSE, col_names = TRUE, na_as_string = FALSE, padding = FALSE) {
    .write_ods(x = x, path = path, sheet = sheet, append = append, update = update, row_names = row_names, col_names = col_names,
               na_as_string = na_as_string, padding = padding, flat = TRUE)
}

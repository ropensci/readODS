.zip_tmp_to_path <- function(temp_ods_dir, path, overwrite = TRUE) {
    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(temp_ods_dir)
    zip::zip(basename(path), include_directories = FALSE, recurse = TRUE, files = dir(), mode = "cherry-pick")
    setwd(wd)
    file.copy(file.path(temp_ods_dir, basename(path)), path, overwrite = overwrite)
}

.write_as_utf8 <- function(text, con) {
    writeLines(enc2utf8(text), con = con, sep = "", useBytes = TRUE)
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

.escape_xml <- function(x) {
    stringi::stri_replace_all_fixed(str = stringi::stri_enc_toutf8(x), pattern = c("&", "\"", "<", ">", "'"), replacement = c("&amp;", "&quot;", "&lt;", "&gt;", "&apos;"), vectorize_all = FALSE)
}

.cell_out <- function(type, value, con) {
    .write_as_utf8(stringi::stri_join("<table:table-cell office:value-type=\"", type, sep = ""), con)
    if (type != "string") {
        .write_as_utf8(stringi::stri_join("\" office:value=\"", value, sep = ""), con)
    }
    .write_as_utf8(stringi::stri_join("\" table:style-name=\"ce1\"><text:p>", value,
                                      "</text:p></table:table-cell>",
                                      sep = ""), con)
}

## CREATION OF sysdata
## .CONTENT <- readLines("benchmark/header.xml")
## .FOOTER <- readLines("benchmark/footer.xml")
## usethis::use_data(.CONTENT, .FOOTER, internal = TRUE, overwrite = TRUE)

.gen_sheet_tag <- function(sheet = "Sheet1", cols = 1024) {
    sprintf('<table:table table:name="%s" table:style-name="ta1"><table:table-column table:style-name="co1" table:number-columns-repeated="%d" table:default-cell-style-name="ce1"/>', .escape_xml(sheet), cols)
}

.flatten <- function(x, column_type) {
    if (column_type == "string") {
        return(.escape_xml(as.character(x)))
    }
    as.character(x)
}

.write_sheet_con <- function(x, con, sheet = "Sheet1", row_names = FALSE, col_names = FALSE, na_as_string = FALSE, padding = FALSE) {
    cmax <- force(if(ncol(x) > 1024) { 16384 } else { 1024 })
    types <- ifelse(unlist(lapply(x, function(x) class(x)[1])) %in% c("integer", "numeric"), "float", "string")
    x_list <- mapply(.flatten, x = x, column_type = types, SIMPLIFY = FALSE)
    colj <- seq_len(NCOL(x))
    cols <- ncol(x)
    if (row_names) {
        rownames_x <- .escape_xml(rownames(x))
        cols <- cols + 1
    }
    rows <- nrow(x)
    if (col_names) {
        colnames_x <- .escape_xml(colnames(x))
        rows <- rows + 1
    }
    if (padding) {
        .write_as_utf8(.gen_sheet_tag(sheet = sheet, cols = cmax), con)
    } else {
        .write_as_utf8(.gen_sheet_tag(sheet = sheet, cols = cols), con)
    }
    # add data
    if (col_names) {
        .write_as_utf8("<table:table-row table:style-name=\"ro1\">", con)
        if (row_names) {
            .cell_out("string", value = "", con = con)
        }
        for (j in colj) {
            .cell_out(type = "string", value = colnames_x[j], con = con)
        }
        if (cols < cmax && padding) {
            .write_as_utf8(stringi::stri_join("<table:table-cell table:number-columns-repeated=\"", as.character(cmax - cols), "\"/>", sep = ""), con)
        }
        .write_as_utf8("</table:table-row>", con)
    }
    for (i in seq_len(NROW(x))) {
        ## create a row
        .write_as_utf8("<table:table-row table:style-name=\"ro1\">", con)
        if (row_names) {
            .cell_out(type = "string", value = rownames_x[i], con = con)
        }
        for (j in colj) {
            value <- x_list[[j, drop = TRUE]][i, drop = TRUE]
            type <- types[j]
            if (!is.na(value)) {
                .cell_out(type = type, value = value, con = con)
                next
            }
            ## NA processing from now
            if (!na_as_string) {
                .write_as_utf8("<table:table-cell/>", con)
                next
            }
            .cell_out(type = "string", value = "NA", con = con)
            ## end
        }
        if (cols < cmax && padding) {
            .write_as_utf8(stringi::stri_join("<table:table-cell table:number-columns-repeated=\"", as.character(cmax - cols), "\"/>", sep = ""), con)
        }
        .write_as_utf8("</table:table-row>", con)
    }
    if (rows < 2^20 && padding) {
        .write_as_utf8(stringi::stri_join("<table:table-row table:style-name=\"ro1\" table:number-rows-repeated=\"", 2^20 - rows, "\"><table:table-cell table:number-columns-repeated=\"", cmax, "\"/></table:table-row>", sep = ""), con)
    }
    .write_as_utf8("</table:table>", con)
    return(invisible(con))
}

.write_sheet_con_cpp <- function(x, con, sheet = "Sheet1", row_names = FALSE, col_names = FALSE, na_as_string = FALSE, padding = FALSE, header = "", footer = "") {
    cmax <- force(if(ncol(x) > 1024) { 16384 } else { 1024 })
    column_types <- ifelse(unlist(lapply(x, function(x) class(x)[1])) %in% c("integer", "numeric"), "float", "string")
    x_list <- mapply(.flatten, x = x, column_type = column_types, SIMPLIFY = FALSE)
    #colj <- seq_len(NCOL(x))
    ##cols <- ncol(x)
    if (row_names) {
        rownames_x <- .escape_xml(rownames(x))
        ##cols <- cols + 1
    } else {
        rownames_x <- c(NA_character_)
    }
    rows <- nrow(x)
    if (col_names) {
        colnames_x <- .escape_xml(colnames(x))
        ##rows <- rows + 1
    } else {
        colnames_x <- c(NA_character_)
    }
    write_sheet_(filename = con, x_list = x_list,
                 column_types = column_types, sheet = .escape_xml(sheet),
                 ## rows = rows, cols = cols,
                 cmax = cmax,
                 row_names = row_names, col_names = col_names,
                 rownames_x = rownames_x, colnames_x = colnames_x,
                 na_as_string = na_as_string, padding = padding, header = header, footer = footer)
    return(invisible(con))
}


.convert_df_to_sheet <- function(x, sheet = "Sheet1", row_names = FALSE, col_names = FALSE, na_as_string = FALSE, padding = FALSE) {
    throwaway_xml_file <- tempfile(fileext = ".xml")
    con <- file(file.path(throwaway_xml_file), open="w+", encoding = "native.enc")
    .write_sheet_con(x = x, con = con, sheet = sheet, row_names = row_names, col_names = col_names,
                     na_as_string = na_as_string, padding = padding)
    close(con)
    return(file.path(throwaway_xml_file))
}

## https://github.com/ropensci/readODS/issues/88
.vfwrite_ods <- function(x, temp_ods_dir, sheet = "Sheet1", row_names = FALSE, col_names = TRUE, na_as_string = FALSE, padding = FALSE) {
    templatedir <- system.file("template", package = "readODS")
    file.copy(dir(templatedir, full.names = TRUE), temp_ods_dir, recursive = TRUE, copy.mode = FALSE)
    con <- file.path(temp_ods_dir, "content.xml")
    ## con <- file(file.path(temp_ods_dir, "content.xml"), open="w+", encoding = "native.enc")
    ## .write_as_utf8(.CONTENT[1], con)
    ## .write_as_utf8(.CONTENT[2], con)
    .write_sheet_con_cpp(x = x, con = con, sheet = sheet, row_names = row_names, col_names = col_names,
                         na_as_string = na_as_string, padding = padding,
                         header = paste0(.CONTENT[1], .CONTENT[2]),
                         footer = .FOOTER)
    ## .write_as_utf8(.FOOTER, con)
    ## close(con)
    return(con)
}

#' Write Data to ODS File
#' @description
#' Function to write a single data.frame to an ods file.
#'
#' @param x a data.frame
#' @param path Path to the ods file to write
#' @param sheet Name of the sheet
#' @param append logical, TRUE indicates that x should be appended to the existing file (path) as a new sheet. If a sheet with the same sheet_name exists, an exception is thrown. See update. Please also note that writing is slower if TRUE. Default is FALSE.
#' @param update logical, TRUE indicates that the sheet with sheet_name in the existing file (path) should be updated with the content of x. If a sheet with sheet_name does not exist, an exception is thrown. Please also note that writing is slower if TRUE. Default is FALSE.
#' @param row_names logical, TRUE indicates that row names of x are to be included in the sheet. Default is FALSE.
#' @param col_names logical, TRUE indicates that column names of x are to be included in the sheet. Default is TRUE.
#' @param na_as_string logical, TRUE indicates that NAs are written as string; FALSE indicates that NAs are written as empty cells.
#' @param padding logical, TRUE indicates that the sheet is padded with repeated empty cells to the maximum size, either 2^20 x 1024 (if the number of columns of `x` is less than or equal 1024) or 2^20 x 16,384 (otherwise). This is the default behaviour of Microsoft Excel. Default is FALSE
#' @return An ODS file written to the file path location specified by the user. The value of \code{path} is also returned invisibly.
#' @author Detlef Steuer <steuer@@hsu-hh.de>, Thomas J. Leeper <thosjleeper@@gmail.com>, John Foster <john.x.foster@@nab.com.au>, Chung-hong Chan <chainsawtiney@@gmail.com>
#' @examples
#' \dontrun{
#' # preserve the row names
#' write_ods(mtcars, "mtcars.ods", row_names = TRUE)
#' # append a sheet to an existing file
#' write_ods(PlantGrowth, "mtcars.ods", append = TRUE, sheet = "plant")
#' }
#' @export
write_ods <- function(x, path = tempfile(fileext = ".ods"), sheet = "Sheet1", append = FALSE, update = FALSE, row_names = FALSE, col_names = TRUE, na_as_string = FALSE, padding = FALSE) {
    ## setup temp directory
    ## one can't just use tempdir() because it is the same in the same session
    temp_ods_dir <- file.path(tempdir(), stringi::stri_rand_strings(1, 20, pattern = "[A-Za-z0-9]"))
    dir.create(temp_ods_dir)
    on.exit(unlink(temp_ods_dir))
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
    if (!file.exists(path) || (!append && !update)) {
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

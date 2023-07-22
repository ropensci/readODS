.zip_tmp_to_path <- function(temp_ods_dir, path, overwrite) {
    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(temp_ods_dir)
    zip::zip(basename(path), files = dir())
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

.escape_xml <- function(x) {
    x_utf8 <- stringi::stri_enc_toutf8(x)
    x_no_amp <- stringi::stri_replace_all_fixed(str = x_utf8, pattern = c("&"), replacement = c("&amp;"), vectorize_all = FALSE)
    stringi::stri_replace_all_fixed(str = x_no_amp, pattern = c("\"", "<", ">", "'"), replacement = c("&quot;", "&lt;", "&gt;", "&apos;"), vectorize_all = FALSE)

}

.cell_out <- function(type, value, con) {
    escaped_value <- .escape_xml(value)
    cat("<table:table-cell office:value-type=\"", type,
        "\" office:value=\"", escaped_value,
        "\" table:style-name=\"ce1\"><text:p>", escaped_value,
        "</text:p></table:table-cell>", 
        sep = "",
        file = con)
}

## CREATION OF sysdata
## .CONTENT <- readLines("benchmark/header.xml")
## .FOOTER <- readLines("benchmark/footer.xml")
## usethis::use_data(.CONTENT, .FOOTER, internal = TRUE, overwrite = TRUE)

.gen_sheet_tag <- function(sheet = "Sheet1") {
    sprintf('<table:table table:name="%s" table:style-name="ta1"><table:table-column table:style-name="co1" table:number-columns-repeated="16384" table:default-cell-style-name="ce1"/>', .escape_xml(sheet))
}

.write_sheet_con <- function(x, con, sheet = "Sheet1", row_names = FALSE, col_names = FALSE, na_as_string = FALSE) {
    cat(.gen_sheet_tag(sheet), file = con)
    types <- unlist(lapply(x, class))
    types <- ifelse(types %in% c("integer", "numeric"), "float", "string")
    colj <- seq_len(NCOL(x))
    # add data
    if (col_names) {
        cat("<table:table-row>", file = con)
        if (row_names) {
            .cell_out("string", value = "", con = con)
        }
        for (j in colj) {
            .cell_out(type = "string", value = colnames(x)[j], con = con)
        }
        cat("</table:table-row>", file = con)
    }
    for (i in seq_len(NROW(x))) {
        ## create a row
        cat("<table:table-row>", file = con)
        if (row_names) {
            .cell_out(type = "string", value = rownames(x)[i], con = con)
        }
        for (j in colj) {
            value <- as.character(x[i, j, drop = TRUE])
            if (is.na(value) && na_as_string) {
                type <- "string"
            } else {
                type <- types[j]
            }
            .cell_out(type = type, value = value, con = con)
        }
        cat("</table:table-row>", file = con)
    }
    cat("</table:table>", file = con)
    return(invisible(con))
}

.convert_df_to_sheet <- function(x, sheet = "Sheet1", row_names = FALSE, col_names = FALSE, na_as_string = FALSE) {
    throwaway_xml_file <- tempfile(fileext = ".xml")
    con <- file(file.path(throwaway_xml_file), open="w", encoding = "UTF-8")
    .write_sheet_con(x = x, con = con, sheet = sheet, row_names = row_names, col_names = col_names, na_as_string = na_as_string)
    close(con)
    return(file.path(throwaway_xml_file))
}

## https://github.com/ropensci/readODS/issues/88
.vfwrite_ods <- function(x, temp_ods_dir, sheet = "Sheet1", row_names = FALSE, col_names = FALSE, na_as_string = FALSE) {
    templatedir <- system.file("template", package = "readODS")
    file.copy(dir(templatedir, full.names = TRUE), temp_ods_dir, recursive = TRUE, copy.mode = FALSE)
    con <- file(file.path(temp_ods_dir, "content.xml"), open="w", encoding = "UTF-8")
    cat(.CONTENT[1], file = con)
    cat(.CONTENT[2], file = con)
    .write_sheet_con(x = x, con = con, sheet = sheet, row_names = row_names, col_names = col_names, na_as_string = na_as_string)
    cat(.FOOTER, file = con)
    close(con)
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
#' @param col_names logical, TRUE indicates that column names of x are to be included in the sheet. Default is FALSE.
#' @param na_as_string logical, TRUE indicates that NAs are written as string. Default is `option("write_ods_na")` (will change to TRUE in the next version).
#' @param verbose logical, deprecated
#' @param overwrite logical, deprecated.
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
write_ods <- function(x, path, sheet = "Sheet1", append = FALSE, update = FALSE, row_names = FALSE, col_names = TRUE, na_as_string = getOption("write_ods_na", default = FALSE), verbose = NULL, overwrite = NULL) {
    ## setup temp directory
    ## one can't just use tempdir() because it is the same in the same session
    temp_ods_dir <- file.path(tempdir(), stringi::stri_rand_strings(1, 20, pattern = "[A-Za-z0-9]"))
    dir.create(temp_ods_dir)
    on.exit(unlink(temp_ods_dir))
    if (!is.null(overwrite)) {
        warning("overwrite is deprecated. Future versions will always set it to TRUE.")
    } else {
        overwrite <- TRUE
    }
    if (!is.null(verbose)) {
        warning("verbose is deprecated. Future versions will always set it to FALSE.")
    } else {
        verbose <- FALSE
    }
    if (!is.data.frame(x)) {
        stop("x must be a data.frame.", call. = FALSE)
    }
    if (!file.exists(path) | (!append & !update)) {
        .vfwrite_ods(x = x, temp_ods_dir = temp_ods_dir, sheet = sheet, row_names = row_names, col_names = col_names, na_as_string = na_as_string)
    } else {
        ## The file must be there.
        utils::unzip(path, exdir = temp_ods_dir)
        contentfile <- file.path(temp_ods_dir, "content.xml")
        content <- xml2::read_xml(contentfile)
        spreadsheet_node <- xml2::xml_children(xml2::xml_children(content)[[which(!is.na(xml2::xml_find_first(xml2::xml_children(content),"office:spreadsheet")))]])[[1]]
        sheet_node <- .find_sheet_node_by_sheet(spreadsheet_node, sheet)
        if ((!is.null(sheet_node) & append & !update) | (!is.null(sheet_node) & !update)) {
            ## Sheet exists so we cannot append
            stop(paste0("Sheet ", sheet, " exists. Set update to TRUE is you want to update this sheet."), call. = FALSE)
        }
        if (is.null(sheet_node) & update) {
            stop(paste0("Sheet ", sheet, " does not exist. Cannot update."), call. = FALSE)
        }
        if (!is.null(sheet_node) & update) {
            ## clean up the sheet
            xml2::xml_remove(xml2::xml_children(sheet_node)[2:length(xml2::xml_children(sheet_node))])
        }
        if (is.null(sheet_node) & append) {
            ## Add a new sheet
            sheet_node <- xml2::xml_add_child(spreadsheet_node, .silent_add_sheet_node(sheet))
        }
        throwaway_xml_file <- .convert_df_to_sheet(x = x, sheet = sheet, row_names = row_names, col_names = col_names,
                                                       na_as_string = na_as_string)
        xml2::xml_replace(sheet_node, .silent_read_xml(throwaway_xml_file))
        ## write xml to contentfile
        xml2::write_xml(content, contentfile)
    }
    ## zip up ODS archive
    .zip_tmp_to_path(temp_ods_dir, path, overwrite)
    invisible(path)
}

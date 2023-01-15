.zip_tmp_to_path <- function(temp_ods_dir, path, overwrite, verbose) {
    if (verbose) {
        zip_flags <- "-r9X"
    } else {
        zip_flags <- "-r9Xq"
    }
    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(temp_ods_dir)
    utils::zip(basename(path), dir(), flags = zip_flags)
    setwd(wd)
    file.copy(file.path(temp_ods_dir, basename(path)), path, overwrite = overwrite)
}

.find_named_sheet <- function(ss, name) {
    sheet <- NULL
    for (i in seq(2, length(xml2::xml_children(ss)))) {
        if (!is.na(xml2::xml_attr(xml2::xml_children(ss)[[i]], "name") == name) &&
            xml2::xml_attr(xml2::xml_children(ss)[[i]], "name") == name) {
            sheet <- xml2::xml_children(ss)[[i]]
        }
    }
    return(sheet)
}

.silent_read_xml <- function(x) {
    suppressWarnings({
        return(xml2::read_xml(x))
    })
}

.silent_add_sheet_node <- function(sheet) {
    .silent_read_xml(sprintf('<table:table table:name="%s" table:style-name="ta1"><table:table-column table:style-name="co1" table:number-columns-repeated="16384" table:default-cell-style-name="ce1"/></table:table>', sheet))
}

.cell_out <- function(type, value, con) {
    cat("<table:table-cell office:value-type=\"", type,
        "\" office:value=\"", value,
        "\" table:style-name=\"ce1\"><text:p>", value,
        "</text:p></table:table-cell>",
        sep = "",
        file = con)
}

## CREATION OF sysdata
## .content <- readLines("benchmark/header.xml")
## .footer <- readLines("benchmark/footer.xml")
## usethis::use_data(.content, .footer, internal = TRUE, overwrite = TRUE)

.gen_sheet_tag <- function(sheet = "Sheet1") {
    sprintf('<table:table table:name="%s" table:style-name="ta1"><table:table-column table:style-name="co1" table:number-columns-repeated="16384" table:default-cell-style-name="ce1"/>', sheet)
}

.write_sheet_con <- function(x, con, sheet = "Sheet1", row_names = FALSE, col_names = FALSE) {
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
            .cell_out(type = types[j], value = as.character(x[i, j, drop = TRUE]), con = con)
        }
        cat("</table:table-row>", file = con)
    }
    cat("</table:table>", file = con)
    return(invisible(con))
}

.convert_df_to_sheet <- function(x, sheet = "Sheet1", row_names = FALSE, col_names = FALSE) {
    throwaway_xml_file <- tempfile(fileext = ".xml")
    con <- file(file.path(throwaway_xml_file), open="w")
    .write_sheet_con(x = x, con = con, sheet = sheet, row_names = row_names, col_names = col_names)
    close(con)
    return(file.path(throwaway_xml_file))
}

## https://github.com/ropensci/readODS/issues/88
.vfwrite_ods <- function(x, temp_ods_dir, sheet = "Sheet1", row_names = FALSE, col_names = FALSE) {
    templatedir <- system.file("template", package = "readODS")
    file.copy(dir(templatedir, full.names = TRUE), temp_ods_dir, recursive = TRUE)
    con <- file(file.path(temp_ods_dir, "content.xml"), open="w")
    cat(.content[1], file = con)
    cat(.content[2], file = con)
    .write_sheet_con(x = x, con = con, sheet = sheet, row_names = row_names, col_names = col_names)
    cat(.footer, file = con)
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
#' @param verbose logical, if messages should be displayed. Default is FALSE.
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
write_ods <- function(x, path, sheet = "Sheet1", append = FALSE, update = FALSE, row_names = FALSE, col_names = TRUE, verbose = FALSE, overwrite = NULL) {
    if (!is.null(overwrite)) {
        warning("overwrite is deprecated. Future versions will always set it to TRUE.")
    } else {
        overwrite <- TRUE
    }
    if (!is.data.frame(x)) {
        stop("x must be a data.frame.", call. = FALSE)
    }
    ## setup temp directory
    ## one can't just use tempdir() because it is the same in the same session
    temp_ods_dir <- file.path(tempdir(), sample(seq_len(1000000), 1))
    dir.create(temp_ods_dir)
    tryCatch({
        if (!file.exists(path) | (!append & !update)) {
            .vfwrite_ods(x = x, temp_ods_dir = temp_ods_dir, sheet = sheet, row_names = row_names, col_names = col_names)
        } else {
            ## The file must be there.
            utils::unzip(path, exdir = temp_ods_dir)
            contentfile <- file.path(temp_ods_dir, "content.xml")
            content <- xml2::read_xml(contentfile)
            spreadsheet <- xml2::xml_children(xml2::xml_children(content)[[which(!is.na(xml2::xml_find_first(xml2::xml_children(content),"office:spreadsheet")))]])[[1]]
            sn <- .find_named_sheet(spreadsheet, sheet)
            if ((!is.null(sn) & append & !update) | (!is.null(sn) & !update)) {
                ## Sheet exists so we cannot append
                stop(paste0("Sheet ", sheet, " exists. Set update to TRUE is you want to update this sheet."), call. = FALSE)
            }
            if (is.null(sn) & update) {
                stop(paste0("Sheet ", sheet, " does not exist. Cannot update."), call. = FALSE)
            }
            if (!is.null(sn) & update) {
                ## clean up the sheet
                xml2::xml_remove(xml2::xml_children(sn)[2:length(xml2::xml_children(sn))])
            }
            if (is.null(sn) & append) {
                ## Add a new sheet
                sn <- xml2::xml_add_child(spreadsheet, .silent_add_sheet_node(sheet))
            }
            throwaway_xml_file <- .convert_df_to_sheet(x = x, sheet = sheet, row_names = row_names, col_names = col_names)
            xml2::xml_replace(sn, .silent_read_xml(throwaway_xml_file))
            ## write xml to contentfile
            xml2::write_xml(content, contentfile)
        }
        ## zip up ODS archive
        .zip_temp_ods_dir_to_path(temp_ods_dir, path, overwrite, verbose)
    },
    finally =  {
        unlink(temp_ods_dir)
    })
    invisible(path)
}

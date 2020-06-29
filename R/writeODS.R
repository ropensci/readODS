.convert_df_to_sheet <- function(x, sheet, row_names, col_names) {
    # identify variable types
    types <- unlist(lapply(x, class))
    types <- ifelse(types %in% c("integer", "numeric"), "float", "string")
    
    rowi <- if (col_names) c(0, seq_len(nrow(x))) else seq_len(nrow(x)) 
    colj <- if (row_names) c(0, seq_along(x)) else seq_along(x)    
    # add data
    for (i in rowi) {
        # create a row
        thisrow <- xml2::xml_add_child(sheet, "table:table-row")
        for (j in colj) {
            if (i == 0) {
                # get column name
                value <- ifelse(j == 0, "", names(x)[j])
            } else if (j == 0) {
                
                value <- rownames(x)[i]
            } else {
                # get value
                value <- as.character(x[i, j, drop = TRUE])
            }
            
            # add value to row
            thiscell <- xml2::xml_add_child(thisrow, "table:table-cell")
            xml2::xml_attr(thiscell, "office:value-type") <- if (i == 0 || j == 0) "string" else types[j]
            xml2::xml_attr(thiscell, "office:value") <- value
            xml2::xml_attr(thiscell, "table:style-name") <- "ce1"
            thistext <- xml2::xml_add_child(thiscell, "text:p")
            xml2::xml_text(thistext) <- value
        }
    }
}

.make_temp_dir <- function() {
    tmp <- tempfile()
    dir.create(tmp)
    return (tmp)
}

.zip_tmp_to_path <- function(tmp, path, overwrite, verbose) {
    if (verbose) {
        zip_flags <- "-r9X"
    } else {
        zip_flags <- "-r9Xq"
    }
    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(tmp)
    zip(basename(path), dir(), flags = zip_flags)
    setwd(wd)
    file.copy(file.path(tmp, basename(path)), path, overwrite = overwrite)
}

.find_named_sheet <- function(ss, name) {
    sheet <- NULL
    sapply(2:length(xml2::xml_children(ss)),
           function(i) {
               if (xml2::xml_attr(xml2::xml_children(ss)[[i]], "name") == name) {
                   sheet <<- xml2::xml_children(ss)[[i]]
               }
           })
    return (sheet)
}

.silent_add_sheet_node <- function(sheet_name) {
    suppressWarnings({
        return(xml2::read_xml(sprintf('<table:table table:name="%s" table:style-name="ta1"><table:table-column table:style-name="co1" table:number-columns-repeated="16384" table:default-cell-style-name="ce1"/></table:table>', sheet_name)))
    })
}

#' write data to ods file
#' @description 
#' Function to write a single data.frame to an ods file.
#' 
#' @param x a data.frame
#' @param path Path to the ods file to write
#' @param sheet_name Name of the sheet
#' @param row_names logical, TRUE indicates that row names of x are to be included in the sheet
#' @param col_names logical, TRUE indicates that column names of x are to be included in the sheet
#' @param append logical, TRUE indicates that x should be appended to the existing file (path) as a new sheet. If a sheet with the same sheet_name exists, an exception is thrown. See update.
#' @param update logical, TRUE indicates that the sheet with sheet_name in the existing file (path) should be updated with the content of x. If a sheet with sheet_name does not exist, an exception is thrown.
#' @param verbose logical, if messages should be displayed
#' @param overwrite logical, depreciated.
#' @return the value of \code{path} invisibly.
#' @author Thomas J. Leeper <thosjleeper@gmail.com>, John Foster <john.x.foster@nab.com.au>, Chung-hong Chan <chainsawtiney@gmail.com>
#' @importFrom xml2 read_xml xml_children xml_add_child xml_add_sibling write_xml xml_ns xml_set_attr xml_attr xml_remove
#' @importFrom utils zip
#' @importFrom assertthat assert_that
#' @export
write_ods <- function(x, path, sheet_name = "Sheet1", append = FALSE, update = FALSE, row_names = FALSE, col_names = TRUE, verbose = FALSE, overwrite = NULL) {
    if (!is.null(overwrite)) {
        warning("overwrite is depreciated. Future versions will always set it to TRUE.")
    } else {
        overwrite <- TRUE
    }
    assertthat::assert_that(is.data.frame(x))    
    # setup temp directory
    tmp <- .make_temp_dir()
    tryCatch({
        if (!file.exists(path) | (!append & !update)) {
            ## The file doesn't exist, no need to consider overwrite or append
            templatedir <- system.file("template", package = "readODS")
            file.copy(dir(templatedir, full.names = TRUE), tmp, recursive = TRUE)
            contentfile <- file.path(tmp, "content.xml")
            content <- xml2::read_xml(contentfile)
            spreadsheet <- xml2::xml_children(xml2::xml_children(content)[[3]])[[1]]
            sheet <- xml2::xml_children(spreadsheet)[[2]]
            xml2::xml_set_attr(sheet, "table:name", sheet_name)
            .convert_df_to_sheet(x, sheet, row_names, col_names)
        } else {
            ## The file must be there.
            unzip(path, exdir = tmp)
            contentfile <- file.path(tmp, "content.xml")
            content <- xml2::read_xml(contentfile)
            spreadsheet <- xml2::xml_children(xml2::xml_children(content)[[3]])[[1]]
            sn <- .find_named_sheet(spreadsheet, sheet_name)
            if ((!is.null(sn) & append & !update) | (!is.null(sn) & !update)) {
                ## Sheet exists so we cannot append
                stop(paste0("Sheet ", sheet_name, " exists. Set update to TRUE is you want to update this sheet."))
            }
            if (is.null(sn) & update) {
                stop(paste0("Sheet ", sheet_name, " does not exist. Cannot update."))
            }
            if (!is.null(sn) & update) {
                ## clean up the sheet
                xml2::xml_remove(xml2::xml_children(sn)[2:length(xml2::xml_children(sn))])
            }
            if (is.null(sn) & append) {
                ## Add a new sheet
                sn <- xml2::xml_add_child(spreadsheet, .silent_add_sheet_node(sheet_name))
            }
            .convert_df_to_sheet(x, sn, row_names, col_names)        
        }
        ## write xml to contentfile
        xml2::write_xml(content, contentfile)
        ## zip up ODS archive
        .zip_tmp_to_path(tmp, path, overwrite, verbose)
    },
    finally =  {
        unlink(tmp)
    })
    invisible(path)
}

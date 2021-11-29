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
            is_string <- i == 0 || j == 0
            xml2::xml_attr(thiscell, "office:value-type") <- if (is_string) "string" else types[j]
            if (!is_string) xml2::xml_attr(thiscell, "office:value") <- value
            xml2::xml_attr(thiscell, "table:style-name") <- "ce1"
            thistext <- xml2::xml_add_child(thiscell, "text:p")
            xml2::xml_text(thistext) <- value
        }
    }
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
    utils::zip(basename(path), dir(), flags = zip_flags)
    setwd(wd)
    file.copy(file.path(tmp, basename(path)), path, overwrite = overwrite)
}

.find_named_sheet <- function(ss, name) {
    sheet <- NULL
    sapply(2:length(xml2::xml_children(ss)),
           function(i) {
               if (!is.na(xml2::xml_attr(xml2::xml_children(ss)[[i]], "name") == name) & xml2::xml_attr(xml2::xml_children(ss)[[i]], "name") == name) {
                   sheet <<- xml2::xml_children(ss)[[i]]
               }
           })
    return (sheet)
}

.silent_add_sheet_node <- function(sheet) {
    suppressWarnings({
        return(xml2::read_xml(sprintf('<table:table table:name="%s" table:style-name="ta1"><table:table-column table:style-name="co1" table:number-columns-repeated="16384" table:default-cell-style-name="ce1"/></table:table>', sheet)))
    })
}

#' Write Data to ODS File
#' @description 
#' Function to write a single data.frame to an ods file.
#' 
#' @param x a data.frame
#' @param path Path to the ods file to write
#' @param sheet Name of the sheet
#' @param append logical, TRUE indicates that x should be appended to the existing file (path) as a new sheet. If a sheet with the same sheet_name exists, an exception is thrown. See update. Default is FALSE.
#' @param update logical, TRUE indicates that the sheet with sheet_name in the existing file (path) should be updated with the content of x. If a sheet with sheet_name does not exist, an exception is thrown. Default is FALSE.
#' @param row_names logical, TRUE indicates that row names of x are to be included in the sheet. Default is FALSE.
#' @param col_names logical, TRUE indicates that column names of x are to be included in the sheet. Default is FALSE.
#' @param verbose logical, if messages should be displayed. Default is FALSE.
#' @param overwrite logical, deprecated.
#' @return An ODS file written to the file path location specified by the user. The value of \code{path} is also returned invisibly.
#' @author Thomas J. Leeper <thosjleeper@@gmail.com>, John Foster <john.x.foster@@nab.com.au>, Chung-hong Chan <chainsawtiney@@gmail.com>
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
        stop("x must be a data.frame.")
    }
    ##setup temp directory
    ## one can't just use tempdir() because it is the same in the same session
    tmp <- file.path(tempdir(), sample(1:1000000, 1))
    dir.create(tmp)    
    tryCatch({
        if (!file.exists(path) | (!append & !update)) {
            ## The file doesn't exist, no need to consider overwrite or append
            templatedir <- system.file("template", package = "readODS")
            file.copy(dir(templatedir, full.names = TRUE), tmp, recursive = TRUE)
            contentfile <- file.path(tmp, "content.xml")
            content <- xml2::read_xml(contentfile)
            spreadsheet <- xml2::xml_children(xml2::xml_children(content)[[3]])[[1]]
            target_sheet <- xml2::xml_children(spreadsheet)[[2]]
            xml2::xml_set_attr(target_sheet, "table:name", sheet)
            .convert_df_to_sheet(x, target_sheet, row_names, col_names)
        } else {
            ## The file must be there.
            utils::unzip(path, exdir = tmp)
            contentfile <- file.path(tmp, "content.xml")
            content <- xml2::read_xml(contentfile)
            spreadsheet <- xml2::xml_children(xml2::xml_children(content)[[which(!is.na(xml2::xml_find_first(xml2::xml_children(content),"office:spreadsheet")))]])[[1]]
            sn <- .find_named_sheet(spreadsheet, sheet)
            if ((!is.null(sn) & append & !update) | (!is.null(sn) & !update)) {
                ## Sheet exists so we cannot append
                stop(paste0("Sheet ", sheet, " exists. Set update to TRUE is you want to update this sheet."))
            }
            if (is.null(sn) & update) {
                stop(paste0("Sheet ", sheet, " does not exist. Cannot update."))
            }
            if (!is.null(sn) & update) {
                ## clean up the sheet
                xml2::xml_remove(xml2::xml_children(sn)[2:length(xml2::xml_children(sn))])
            }
            if (is.null(sn) & append) {
                ## Add a new sheet
                sn <- xml2::xml_add_child(spreadsheet, .silent_add_sheet_node(sheet))
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

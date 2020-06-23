df_to_sheet <- function(x, sheet, row_names, col_names)
{
    # identify variable types
    types <- unlist(lapply(x, class))
    types <- ifelse(types %in% c("integer", "numeric"), "float", "string")
    
    rowi <- if (col_names) c(0, seq_len(nrow(x))) else seq_len(nrow(x)) 
    colj <- if (row_names) c(0, seq_along(x)) else seq_along(x)    
    # add data
    for (i in rowi) {
        # create a row
        thisrow <- xml_add_child(sheet, "table:table-row")
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
            thiscell <- xml_add_child(thisrow, "table:table-cell")
            xml_attr(thiscell, "office:value-type") <- if (i == 0 || j == 0) "string" else types[j]
            xml_attr(thiscell, "office:value") <- value
            xml_attr(thiscell, "table:style-name") <- "ce1"
            thistext <- xml_add_child(thiscell, "text:p")
            xml_text(thistext) <- value
        }
    }
}
make_temp_dir <- function()
{
    tmp <- tempfile()
    dir.create(tmp)
    return (tmp)
}
zip_it_up <- function(tmp, path, overwrite)
{
    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(tmp)
    zip(basename(path), dir())
    setwd(wd)
    file.copy(file.path(tmp, basename(path)), path, overwrite = overwrite)
}
find_named_sheet <- function(ss, name)
{
    sheet <- NULL
    sapply(2: length(xml_children(ss)),
           function(i) {
               
               if (xml_attr(xml_children(ss)[[i]], "name") == name)
               {
                   sheet <<- xml_children(ss)[[i]]
               }
           })
    return (sheet)
}


#' write data to ods file
#' 
#' @description 
#' Function to write a single data.frame to an ods file.
#' 
#' @param x a data.frame
#' @param path Path to the ods file to write.
#' @param overwrite logical, whether to overwrite an existing file, if available.
#' @param sheet_name Name of the sheet
#' @param row_names TRUE iff row names are to be included in the sheet
#' @param col_names TRUE iff the column names are to be included in the sheet
#' @return the value of \code{path} invisibly.
#' @author Thomas J. Leeper <thosjleeper@gmail.com>, Chung-hong Chan <chainsawtiney@gmail.com>
#' @importFrom xml2 read_xml xml_children xml_add_child xml_add_sibling write_xml xml_ns xml_set_attr xml_attr
#' @importFrom utils zip
#' @importFrom assertthat assert_that
#' @export
write_ods <- function(x, path, overwrite = TRUE, sheet_name = "Sheet1", row_names = FALSE, col_names = TRUE) {
    
    assert_that(is.data.frame(x))
    
    # setup temp directory
    tmp <- make_temp_dir()
    tryCatch({
        
        templatedir <- system.file("template", package = "readODS")
        file.copy(dir(templatedir, full.names = TRUE), tmp, recursive = TRUE)
        
        # open template document
        contentfile <- file.path(tmp, "content.xml")
        content <- read_xml(contentfile)
        
        # spreadsheet contents
        spreadsheet <- xml_children(xml_children(content)[[3]])[[1]]
        
        # read sheet
        sheet <- xml_children(spreadsheet)[[2]]

        xml_set_attr(sheet, "table:name", sheet_name)
        df_to_sheet(x, sheet, row_names, col_names)
        
        # write xml to contentfile
        write_xml(content, contentfile)
        
        # zip up ODS archive
        zip_it_up(tmp, path, overwrite)
    },
    finally = 
    {
        unlink(tmp)
        
    })
    invisible(path)
}
#' write data to ods file in a named sheet. 
#' @description 
#' Function to write a single data.frame to an ods file.
#' 
#' @details 
#' The logic table below explains behaviour:
#' 
#' Logic table:
#' \tabular{rrrrr}{
#' \strong{File exists} \tab \strong{Sheet exists} \tab \strong{overwrite_file} \tab \strong{overwrite_sheet} \tab \strong{Result} \cr
#' F \tab F \tab X \tab X \tab New file with named sheet \cr
#' T \tab X \tab T \tab X \tab Overwrite file with named sheet \cr
#' T \tab F \tab F \tab X \tab New sheet added to file \cr
#' T \tab T \tab F \tab F \tab Exception thrown \cr
#' T \tab T \tab F \tab T \tab Sheet replaced in file
#' }
#' 
#' 
#' @param x a data.frame
#' @param path Path to the ods file to write.
#' @param sheet_name Name of the sheet
#' @param overwrite_file logical, whether to overwrite an existing file, if available. See logic table.
#' @param overwrite_sheet TRUE iff named sheet to be overwritten. See logic table.
#' @param row_names TRUE iff row names are to be included in the sheet
#' @param col_names TRUE iff the column names are to be included in the sheet
#' @return the value of \code{path} invisibly. Throws an exception if named sheet exists and overwrite_sheet is FALSE.
#' @author J Foster <john.x.foster@nab.com.au>
#' @importFrom xml2 read_xml xml_children xml_add_child xml_add_sibling write_xml xml_ns xml_set_attr xml_attr xml_remove
#' @importFrom utils zip
#' @importFrom assertthat assert_that
#' @export
write_ods_2 <- function(x, path, sheet_name,
                        overwrite_file = FALSE, overwrite_sheet = FALSE, 
                        row_names = FALSE, col_names = TRUE)
{
    assert_that(is.data.frame(x))
    
    if (overwrite_file || !file.exists(path))
    {
        return (write_ods(x, path, overwrite = overwrite_file, sheet_name = sheet_name, row_names = row_names, col_names = col_names))
    }
    
    # setup temp directory
    tmp <- make_temp_dir()
    tryCatch({
        
        unzip(path, exdir = tmp)
        contentfile <- file.path(tmp, "content.xml")
        content <- read_xml(contentfile)
        
        # spreadsheet contents
        spreadsheet <- xml_children(xml_children(content)[[3]])[[1]]
    
        sn <- find_named_sheet(spreadsheet, sheet_name)
        
        if (is.null(sn))
        {
            # Add a new sheet
            sn <- xml_add_child(spreadsheet, read_xml(sprintf('<table:table table:name="%s" table:style-name="ta1"><table:table-column table:style-name="co1" table:number-columns-repeated="16384" table:default-cell-style-name="ce1"/></table:table>', sheet_name)))
        }
        else if (!overwrite_sheet)
        {
            stop("Sheet exists and overwrite is FALSE", call. = FALSE)
        }
        else if (length(xml_children(sn)) > 1)
        {
            # Truncate existing sheet
            xml_remove(xml_children(sn)[2:length(xml_children(sn))])
        }

        df_to_sheet(x, sn, row_names, col_names)
        
        # write xml to contentfile
        write_xml(content, contentfile)
        
        # zip up ODS archive
        zip_it_up(tmp, path, TRUE)
    },
    finally = 
    {
        unlink(tmp)
            
    })
    invisible(path)
}

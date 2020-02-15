#' write data to ods file
#' 
#' @description 
#' Function to write a single data.frame to an ods file.
#' 
#' @param x a data.frame
#' @param path Path to the ods file to write.
#' @param overwrite logical, whether to overwrite an existing file, if available.
#' @return the value of \code{path} invisibly.
#' @author Thomas J. Leeper <thosjleeper@gmail.com>, Chung-hong Chan <chainsawtiney@gmail.com>
#' @importFrom xml2 read_xml xml_children xml_add_child xml_add_sibling write_xml xml_ns
#' @importFrom utils zip
#' @export
write_ods <- function(x, path, overwrite = TRUE) {
    # setup temp directory
    tmp <- tempfile()
    dir.create(tmp)
    on.exit(unlink(tmp))
    templatedir <- system.file("template", package = "readODS")
    file.copy(dir(templatedir, full.names = TRUE), tmp, recursive = TRUE)
    
    # open template document
    contentfile <- file.path(tmp, "content.xml")
    content <- read_xml(contentfile)
    
    # spreadsheet contents
    spreadsheet <- xml_children(xml_children(content)[[3]])[[1]]
    
    # read sheet
    sheet <- xml_children(spreadsheet)[[2]]
    
    # identify variable types
    types <- unlist(lapply(x, class))
    types <- ifelse(types %in% c("integer", "numeric"), "float", "string")
                    
    # add data
    for (i in c(0,seq_len(nrow(x)))) {
        # create a row
        thisrow <- xml_add_child(sheet, "table:table-row")
        for (j in seq_along(x)) {
            if (i == 0) {
                # get column name
                value <- names(x)[j]
            } else {
                # get value
                value <- as.character(x[i, j, drop = TRUE])
            }
            
            # add value to row
            thiscell <- xml_add_child(thisrow, "table:table-cell")
            xml_attr(thiscell, "office:value-type") <- if (i == 0) "string" else types[j]
            xml_attr(thiscell, "office:value") <- value
            xml_attr(thiscell, "table:style-name") <- "ce1"
            thistext <- xml_add_child(thiscell, "text:p")
            xml_text(thistext) <- value
        }
    }
    
    # write xml to contentfile
    write_xml(content, contentfile)
    
    # zip up ODS archive
    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(tmp)
    zip(basename(path), dir())
    setwd(wd)
    file.copy(file.path(tmp, basename(path)), path, overwrite = overwrite)
    invisible(path)
}

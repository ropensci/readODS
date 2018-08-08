#' write multiple data to ods file in separate sheets
#' 
#' @description 
#' Function to write a vector of data.frame to separate sheets in an ods file and return a data frame vector. 
#' 
#' @param dataframes_vector a vector of data.frame
#' @param path Path to the ods file to write.
#' @return the value of \code{path} invisibly.
#' @author Julien Cochennec <julien.cochennec.it@gmail.com>
#' @importFrom xml2 read_xml xml_children xml_add_child xml_add_sibling write_xml xml_ns
#' @importFrom utils zip
#' @export
multiple_write_ods <- function(dataframes_vector, path) {
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
  
  # read empty sheet from template
  emptysheet <- xml_children(spreadsheet)[[2]]
  
  # remove empty sheet from content
  xml_remove(spreadsheet)
  
  for(dataframe_name in names(dataframe_vector)){
    dataframe = dataframes_vector[dataframe_name]
    thissheet = copy(emptysheet)
    xml_attr(thissheet, "table:name") <- "Sheet1"
    # identify variable types
    types <- unlist(lapply(dataframe, class))
    types <- ifelse(types %in% c("integer", "numeric"), "float", "string")
    
    # add data
    for (i in c(0,seq_len(nrow(dataframe)))) {
      # create a row
      thisrow <- xml_add_child(new_sheet, "table:table-row")
      for (j in seq_along(dataframe)) {
        if (i == 0) {
          # get column name
          value <- names(dataframe)[j]
        } else {
          # get value
          value <- as.character(dataframe[i, j, drop = TRUE])
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
  }
  
  # write xml to contentfile
  write_xml(content, contentfile)
  
  # zip up ODS archive
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(tmp)
  zip(basename(path), dir())
  setwd(wd)
  file.copy(file.path(tmp, basename(path)), path)
  invisible(path)
}

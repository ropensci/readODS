## dependency

#' @import xml2
#' @import cellranger
#' @import readr
NULL



## ' numberToLetters
## ' 
## ' @keywords internal
## ' @description
## ' converts numbers to microplate row names and Excel & ODS column names
## ' 
## ' @param listOfNumbers the numbers you want to convert to chars
## ' @details
## ' 1=A
## ' 26=Z
## ' 27=ZA
## ' 702=ZZ
## ' 703=AAA
## ' 
## ' supports lists of numbers!
## ' 
## ' numberToLetters(1:1000)
## ' 
numbers_to_letters <- function(listOfNumbers=NULL){
  returnValue <- NULL
  for(i in 1:length(listOfNumbers)){
      remainder <- listOfNumbers[[i]]
      returnLetters=""
      while(TRUE){
          if(remainder==0){
              break
          }
          if(remainder%%26!=0){
              returnLetters=paste(LETTERS[remainder%%26],returnLetters,sep = "")
              remainder=remainder%/%26  
          }else{
              returnLetters=paste("Z",returnLetters,sep = "")
              remainder=(remainder%/%26)-1
          }
      }
      returnValue[[i]]=returnLetters
  }
  return(returnValue)
}




### return a parsed XML tree from an ODS file
parse_ods_file <- function(file = NULL){
    if(is.null(file)) {
        stop("no filename given")
    }
    if(!file.exists(file)) {
        stop("file does not exist")
    }
    con <- unz(file,filename="content.xml")
    parsed_ods<- read_xml(con)
    return(parsed_ods)
}

extract_namespace <- function(parsed_ods) {
    ods_ns <- xml_ns(parsed_ods)
    return(ods_ns)
}

parse_sheets <- function(parsed_ods, ods_ns) {
    parsed_sheets <- xml_find_all(parsed_ods, ".//office:body/office:spreadsheet/table:table", ods_ns)
    return(parsed_sheets)
}


check_cell_repeat <- function(cell, ods_ns) {
    if (xml_has_attr(cell, "table:number-columns-repeated", ods_ns)) {
        return(as.numeric(xml_attr(cell, "table:number-columns-repeated", ods_ns)))
    }
    return(1)
}

check_cell_with_textp <- function(cell, ods_ns) {
    return(length(xml_find_all(cell, ".//text:p", ods_ns)) != 0)
}

parse_single_cell <- function(cell, ods_ns, formula_as_formula = FALSE, use_office_value = TRUE) {
    cell_value <- xml_text(xml_find_all(cell, ".//text:p", ods_ns))
    if (cell_value == "" & use_office_value & xml_has_attr(cell, "office:value", ods_ns)) {
        cell_value <- xml_attr(cell, "office:value", ods_ns)
    }
    if (formula_as_formula & xml_has_attr(cell, "table:formula", ods_ns)) {
        cell_value <- xml_attr(cell, "table:formula", ods_ns)
    }
    return(cell_value)
}

parse_rows <- function(parsed_sheet, ods_ns, formula_as_formula, skip = 0) {
    rows <- xml_find_all(parsed_sheet, ".//table:table-row", ods_ns)
    if (skip > 0 & skip >= length(rows)) {
        warning("skip value >=  number of rows, ignore the skip setting")
        skip <- 0
    }
    if (skip > 0) {
        rows <- rows[(skip+1):length(rows)]
    }
    cell_values <- data.frame()
    current_row <- 0
    for (row in rows) {
        current_row <- current_row + 1
        if (xml_has_attr(row, "table:number-rows-repeated", ods_ns)) {
            ## empty row, just bump the current_row
            current_row <- current_row + as.numeric(xml_attr(row, "table:number-rows-repeated", ods_ns)) - 1
        } else {
            ##parse the value in each column
            current_col <- 0
            for (cell in xml_find_all(row, ".//table:table-cell", ods_ns)) {
                bump_cell <- check_cell_repeat(cell, ods_ns)
                cell_with_textp <- check_cell_with_textp(cell, ods_ns)
                current_col <- current_col + 1
                if (cell_with_textp) {
                    ## non_empty cell, get the value
                    cell_value <- parse_single_cell(cell, ods_ns, formula_as_formula = formula_as_formula)
                    cell_values <- rbind(cell_values, data.frame(row_id = current_row, col_id = current_col, cell_value = cell_value, stringsAsFactors = FALSE))
                }
                if (bump_cell > 1 & !cell_with_textp) {
                    current_col <- current_col + bump_cell - 1
                }
                if (bump_cell > 1 & cell_with_textp) {
                    for (bump in 1:(bump_cell-1)) {
                        current_col <- current_col + 1
                        cell_values <- rbind(cell_values, data.frame(row_id = current_row, col_id = current_col, cell_value = cell_value ,stringsAsFactors = FALSE))
                    }
                }
            }
        }
    }
    return(cell_values)
}


### steal from rio
change_df_with_header <- function(x) {
    colnames(x) <- x[1,]
    g <- x[2:nrow(x),]
    if (class(g) != "data.frame") {
        g <- data.frame(g, stringsAsFactors = FALSE)
        colnames(g) <- x[1,]
    }
    rownames(g) <- seq(from = 1, to = nrow(g))
    return(g)
}

### ugly version
to_data_frame <- function(cell_values, header = FALSE, na) {
    if (nrow(cell_values) == 0) {
        warning("empty sheet, return empty data frame.")
        return(data.frame())
    }
    res <- data.frame(matrix(data = "", nrow = max(cell_values[,1]) ,ncol= max(cell_values[,2])), stringsAsFactors = FALSE)
    for(i in 1:nrow(cell_values)){
        res[cell_values[i, 1], cell_values[i, 2]] <- cell_values[i, 3]
    }
    if (header) {
        res <- change_df_with_header(res)
    } else {
        colnames(res) <- numbers_to_letters(1:ncol(res))
    }
    ## clean-up na
    if (!is.null(na)) {
        res[res == na] <- NA
    }
    return(res)
}

parse_ods_to_sheets <- function(file) {
    parsed_ods <- parse_ods_file(file)
    ods_ns <- extract_namespace(parsed_ods)
    sheets <- parse_sheets(parsed_ods, ods_ns)
    return(list(sheets, ods_ns))
}

select_sheet <- function(sheets, ods_ns, which_sheet) {
    if (is.numeric(which_sheet) & which_sheet > length(sheets)) {
        stop("sheet larger than number of sheets in the ods file.")
    }
    if (is.character(which_sheet)) {
        sheet_names <- sapply(sheets, function(x) xml_attr(x, "table:name", ods_ns))
        if (which_sheet %in% sheet_names) {
            which_sheet <- which(sheet_names == which_sheet)
        } else {
            stop(paste0("No sheet named ", which_sheet, " in the ods file."))
        }
    }
    return(sheets[which_sheet])
}

select_range <- function(raw_sheet, range) {
    range_select <- cellranger::as.cell_limits(range)
    selected_sheet <- raw_sheet[range_select$ul[1]:range_select$lr[1], range_select$ul[2]:range_select$lr[2]]
    return(selected_sheet)
}

############### REAL DEALS #####################

#' read data from ods files
#' 
#' @description 
#' Funtion to read a single sheet from a ods file and return a data frame. 
#' 
#' @param path Path to the ods file.
#' @param sheet sheet to read. Either a string (the sheet name), or an integer sheet number. The default is 1.
#' @param col_names indicating whether the file contains the names of the variables as its first line.
#' @param col_types Either NULL to guess from the spreadsheet or a character vector containing "blank", "numeric", "date" or "text". NA will return a data frame with all columns being "text".
#' @param na Missing value. By default readODS converts blank cells to missing data.
#' @param skip the number of lines of the data file to skip before beginning to read data.
#' @param formula_as_formula a switch to display formulas as formulas "SUM(A1:A3)" or as the resulting value "3"... or "8"..
#' @param range selection of rectangle using Excel-like cell range, such as \code{range = "D12:F15"} or \code{range = "R1C12:R6C15"}. Cell range processing is handled by the \code{\link[=cellranger]{cellranger}} package.
#' @return a data frame (\code{data.frame}) containing a representation of data in the ods file.
#' @note Currently, ods files that linked to external data source cannot be read. Merged cells cannot be parsed correctly.
#' @author Chung-hong Chan <chainsawtiney@gmail.com>
#' @export
read_ods <- function(path = NULL, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, formula_as_formula = FALSE, range = NULL) {
    res <- parse_ods_to_sheets(path)
    ods_ns <- res[[2]]
    sheets <- res[[1]]
    target_sheet <- select_sheet(sheets, ods_ns = ods_ns, which_sheet = sheet)
    cell_values <- parse_rows(target_sheet, ods_ns, formula_as_formula = formula_as_formula, skip = skip)
    parsed_df <- to_data_frame(cell_values = cell_values, header = col_names, na = na)
    if (is.null(col_types)) {
        raw_sheet <- readr::type_convert(df = parsed_df)
    }
    if (is.na(NA)) {
        raw_sheet <- parsed_df
    } else {
        raw_sheet <- readr::type_convert(df = parsed_df, col_types = col_types)
    }
    if (!is.null(range)) {
        res <- select_range(raw_sheet, range)
    } else {
        res <- raw_sheet
    }
    return(res)
}

#' read data from ods files (depreciated)
#' 
#' @description 
#' returns a list of data frames with one data frame per sheet. This is a wrapper to read_ods  for backward compatibile with previous version of readODS. Please use read_ods if possible.
#' 
#' @param file Path to the ods file.
#' @param sheet default to NULL with all sheets being read as a list of data.frame. If a number is given, the sheet as a single data.frame is returned.
#' @param formulaAsFormula Logical, a switch to display formulas as formulas "SUM(A1:A3)" or as the resulting value "3"... or "8".. 
#' 
#' @details 
#' the data.frame contains all strings (not factors)
#' @author Chung-hong Chan <chainsawtiney@gmail.com>, Gerrit-Jan Schutten <phonixor@gmail.com>
#' @export
read.ods <- function(file = NULL, sheet = NULL, formulaAsFormula = FALSE) {
    if (!is.null(sheet)) {
        return(read_ods(path = file, sheet = sheet, col_names = FALSE, formula_as_formula = formulaAsFormula, skip = 0, na = NULL, col_types = NA))
    } else {
        return(lapply(ods_sheets(file), function(x) read_ods(path = file, sheet = x, col_names = FALSE, formula_as_formula = formulaAsFormula, skip = 0, na = NULL, col_types = NA)))
    }
}


#' @name get_num_sheet_in_ods
#' @rdname get_num_sheet_in_ods
#' @title get the number of sheets in an ods file
#' 
#' @description
#' get the number of sheets in an ods file
#' @param file the name of the ods file
#' @author Chung-hong Chan <chainsawtiney@gmail.com>, Gerrit-Jan Schutten <phonixor@gmail.com>
#' @seealso
#' use \code{\link{read_ods}} to read the data
NULL

#' @rdname get_num_sheet_in_ods
#' @export
get_num_sheet_in_ods <- function(file) {
    sheets <- parse_ods_to_sheets(file)[[1]]
    return(length(sheets))
}

#' @rdname get_num_sheet_in_ods
#' @export
getNrOfSheetsInODS <- function(file) {
    return(get_num_sheet_in_ods(file))
}

#' List all sheets in an ods file.
#'
#' @description
#' List all sheets in an ods file.
#' @param path Path to the ods file
#' @export
ods_sheets <- function(path) {
    res <- parse_ods_to_sheets(path)
    return(sapply(res[[1]], function(x) xml_attr(x, "table:name", res[[2]])))
}

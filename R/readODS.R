##http://docs.oasis-open.org/office/v1.2/os/OpenDocument-v1.2-os-part1.html#__RefHeading__1420324_253892949







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
numbers_to_letters=function(listOfNumbers=NULL){
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


## #' lettersToNumber
## #' 
## #' @keywords internal
## #' @description
## #' converts microplate row names and Excel & ODS column names into numbers
## #' 
## #' @param listOfStrings the strings you want to convert to numbers
## #' 
## #' @details
## #' supports lists of chars!
## #' ignores case
## #' 
## #' A=1
## #' Z=26
## #' ZA=27
## #' ZZ=702
## #' AAA=703 
## #' 
## #' lettersToNumber("AAa")
## lettersToNumber=function( listOfStrings=NULL){
##   total=NULL
##   for(i in 1:length(listOfStrings)){ # for each string
##     total[i]=0
##     for (j in 1:nchar(listOfStrings[i])){ # for each char in the string
##       current=match(casefold(substring(listOfStrings[i],j,j)),letters)
##       total[i]=total[i]+(current*26^(nchar(listOfStrings[i])-j))
##     }
##   }
##   return(total)
## }


### return a parsed XML tree from an ODS file
parse_ods_file= function(file = NULL){
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

parse_rows <- function(parsed_sheet, ods_ns, formula_as_formula) {
    rows <- xml_find_all(parsed_sheet, ".//table:table-row", ods_ns)
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
    rownames(g) <- seq(from = 1, to = nrow(g))
    return(g)
}

### ugly version
to_data_frame <- function(cell_values, header = FALSE) {
    res <- data.frame(matrix(data = "", nrow = max(cell_values[,1]) ,ncol= max(cell_values[,2])), stringsAsFactors = FALSE)
    for(i in 1:nrow(cell_values)){
        res[cell_values[i, 1], cell_values[i, 2]] <- cell_values[i, 3]
    }
    if (header) {
        res <- change_df_with_header(res)
    } else {
        colnames(res) <- numbers_to_letters(1:ncol(res))
    }
    return(res)
}

parse_ods_to_sheets <- function(file) {
    parsed_ods <- parse_ods_file(file)
    ods_ns <- extract_namespace(parsed_ods)
    sheets <- parse_sheets(parsed_ods, ods_ns)
    return(list(sheets, ods_ns))
}

############### REAL DEALS #####################

#' read data from ods files
#' 
#' @description 
#' Funtion to read a single sheet from a ods file and return a data frame. 
#' 
#' @param file the name of the ods file which the data are to be read from.
#' @param sheet numeric, the sheet number to be read from. The default is 1.
#' @param header logical,  indicating whether the file contains the names of the variables as its first line. 
#' @param formula_as_formula logical, a switch to display formulas as formulas "SUM(A1:A3)" or as the resulting value "3"... or "8"..
#' @return a data frame (\code{data.frame}) containing a representation of data in the ods file. All data are read as characters.
#' @author Chung-hong Chan <chainsawtiney@gmail.com>
#' @import xml2
#' @export
read_ods <- function(file = NULL, sheet = 1, header = FALSE, formula_as_formula = FALSE) {
    res <- parse_ods_to_sheets(file)
    ods_ns <- res[[2]]
    sheets <- res[[1]]
    if (!is.null(sheet)) {
        cell_values <- parse_rows(sheets[sheet], ods_ns, formula_as_formula = formula_as_formula)
        return(to_data_frame(cell_values, header))
    } else {
        return(lapply(sheets, function(x) to_data_frame(parse_rows(x, ods_ns, formula_as_formula = formula_as_formula), header)))
    }
}

#' read data from ods files (depreciated)
#' 
#' @description 
#' returns a list of data frames with one data frame per sheet. This is a wrapper to read_ods  for backward compatibile with previous version of readODS. Please use read_ods if possible.
#' 
#' @param file the name of the ods file which the data are to be read from.
#' @param sheet default to NULL with all sheets being read as a list of data.frame. If a number is given, the sheet as a single data.frame is returned.
#' @param formulaAsFormula Logical, a switch to display formulas as formulas "SUM(A1:A3)" or as the resulting value "3"... or "8".. 
#' 
#' @details 
#' the data.frame contains all strings (not factors)
#' @author Chung-hong Chan <chainsawtiney@gmail.com>, Gerrit-Jan Schutten <phonixor@gmail.com>
#' @export
read.ods <- function(file=NULL, sheet=NULL, formulaAsFormula=F) {
    return(read_ods(file = file, sheet = sheet, header = FALSE, formula_as_formula = formulaAsFormula))
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

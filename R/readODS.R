#' @importFrom utils unzip

## ' @keywords internal
## ' @description
## ' converts numbers to microplate row names and Excel & ODS column names
## ' 
## ' @param list_of_letter the numbers you want to convert to chars
## ' @details
## ' 1=A
## ' 26=Z
## ' 27=ZA
## ' 702=ZZ
## ' 703=AAA
## ' 
## ' supports lists of numbers!
## ' 
## ' .convert_numbers_to_letters(1:1000)
## ' 
.convert_numbers_to_letters <- function(list_of_numbers = NULL) {
  return_value <- NULL
  for(i in seq_len(length(list_of_numbers))) {
      remainder <- list_of_numbers[[i]]
      return_letters <- ""
      while(TRUE) {
          if(remainder == 0) {
              break
          }
          if(remainder %% 26 != 0) {
              return_letters <- paste(LETTERS[remainder %% 26],return_letters, sep = "")
              remainder <- remainder %/% 26  
          } else {
              return_letters <- paste("Z", return_letters,sep = "")
              remainder <- (remainder %/% 26) - 1
          }
      }
      return_value[[i]] <- return_letters
  }
  return(return_value)
}

.unzip_ods <- function(file) {
    exdir <- tempdir()
    unzip(file, files = "content.xml", exdir = exdir)
    return(file.path(exdir, "content.xml"))
}

### return a parsed XML tree from an ODS file
.parse_ods_file <- function(file = NULL){
    if(is.null(file)) {
        stop("no filename given", call. = FALSE)
    }
    if(!file.exists(file)) {
        stop("file does not exist", call. = FALSE)
    }
    ## con <- unz(file,filename="content.xml")
    con <- .unzip_ods(file)
    parsed_ods <- xml2::read_xml(con, options = c("NOBLANKS", "HUGE"))
    return(parsed_ods)
}

.extract_namespace <- function(parsed_ods) {
    ods_ns <- xml2::xml_ns(parsed_ods)
    return(ods_ns)
}

.parse_sheets <- function(parsed_ods, ods_ns) {
    parsed_sheets <- xml2::xml_find_all(parsed_ods, ".//office:body/office:spreadsheet/table:table", ods_ns)
    return(parsed_sheets)
}

.check_cell_repeat <- function(cell, ods_ns) {
    if (xml2::xml_has_attr(cell, "table:number-columns-repeated", ods_ns)) {
        return(as.numeric(xml2::xml_attr(cell, "table:number-columns-repeated", ods_ns)))
    }
    return(1)
}

.check_cell_with_textp <- function(cell, ods_ns) {
    return(length(xml2::xml_find_all(cell, ".//text:p", ods_ns)) != 0)
}

.parse_textp <- function(cell, ods_ns) {
    textp <- xml2::xml_find_all(cell, "./text:p", ods_ns)
    purrr::map_chr(textp, .parse_p, ods_ns = ods_ns)    
}

### this function parses cell but with consideration of <text:s>
### make it extensible through here
.parse_p <- function(ppart, ods_ns) {
    p_content <- xml2::xml_contents(ppart)
    output <- ""
    for (x in p_content) {
        if (xml2::xml_name(x, ods_ns) == "text:s") {
            rep_space <- as.numeric(xml2::xml_attr(x, "text:c", ns = ods_ns))
            if (is.na(rep_space)) {
                rep_space <- 0
            }
            output <- paste0(output, paste0(rep(" ", rep_space), collapse = ""))
        } else {
            output <- paste0(output, xml2::xml_text(x))
        }
    }
    return(output)
}

.parse_single_cell <- function(cell, ods_ns, formula_as_formula = FALSE, use_office_value = TRUE) {
    cell_value <- paste0(.parse_textp(cell, ods_ns), collapse = "\n") ## handle multiline values, #23
    if (xml2::xml_has_attr(cell, "office:value-type", ods_ns) &&
        xml2::xml_attr(cell, "office:value-type", ods_ns) %in% c("float", "currency", "percentage")) {
      cell_value <- xml2::xml_attr(cell, "office:value", ods_ns)
    }
    if (cell_value == "" && use_office_value & xml2::xml_has_attr(cell, "office:value", ods_ns)) {
        cell_value <- xml2::xml_attr(cell, "office:value", ods_ns)
    }
    if (formula_as_formula && xml2::xml_has_attr(cell, "table:formula", ods_ns)) {
        cell_value <- xml2::xml_attr(cell, "table:formula", ods_ns)
    }
    return(cell_value)
}

.parse_rows <- function(parsed_sheet, ods_ns, formula_as_formula, skip = 0) {
    rows <- xml2::xml_find_all(parsed_sheet, ".//table:table-row", ods_ns)
    cell_values <- new.env(hash = TRUE)
    if (skip > 0 && skip >= length(rows)) {
        return(cell_values)
    }
    if (skip > 0) {
        rows <- rows[(skip + 1):length(rows)]
    }
    current_row <- 0
    for (row in rows) {
        
        if (xml2::xml_has_attr(row, "table:number-rows-repeated", ods_ns)) {
            ## number of repeats
            row_repeats <- as.numeric(xml2::xml_attr(row, "table:number-rows-repeated", ods_ns))
        } else {
            ## if no repeat
            row_repeats <- 1
        }
        
        for (rep_row in seq_len(row_repeats)) {
            current_row <- current_row + 1
            
            current_col <- 0
            
            for (cell in xml2::xml_find_all(row, ".//table:table-cell", ods_ns)) {
                bump_cell <- .check_cell_repeat(cell, ods_ns)
                cell_with_textp <- .check_cell_with_textp(cell, ods_ns)
                current_col <- current_col + 1
                if (cell_with_textp) {
                    ## non_empty cell, get the value
                    cell_value <- .parse_single_cell(cell, ods_ns, formula_as_formula = formula_as_formula)
                    cell_values[[paste0(current_row, ",", current_col)]] <- cell_value
                }
                if (bump_cell > 1 && !cell_with_textp) {
                    current_col <- current_col + bump_cell - 1
                }
                if (bump_cell > 1 && cell_with_textp) {
                    for (bump in seq_len(bump_cell - 1)) {
                        current_col <- current_col + 1
                        cell_values[[paste0(current_row, ",", current_col)]] <- cell_value
                    }
                }
            }
        }
        
    }
    return(cell_values)
}


.change_df_with_col_row_header <- function(x, col_header, row_header, range) {
    if (!is.null(range)) {
        x <- .select_range(x, range)
    }
    irow <- ifelse(col_header, 2, 1)  
    jcol <- ifelse(row_header, 2, 1)
    
    g <- x[irow:nrow(x), jcol:ncol(x), drop=FALSE] # maintain as dataframe for single column
    rownames(g) <- if (row_header) x[seq(irow, nrow(x)), 1] else NULL # dont want character row headers given by 1:nrow(g)
    colnames(g) <- if (col_header) x[1, seq(jcol, ncol(x))] else .convert_numbers_to_letters(seq_len(ncol(g)))
    return(g)
}

.convert_to_data_frame <- function(cell_values, header = FALSE, na = NULL, row_header = FALSE, range) {
    cv_keys <- ls(cell_values)
    if (length(cv_keys) == 0) {
        warning("empty sheet, return empty data frame.", call. = FALSE)
        return(data.frame())
    }
    row_id <- purrr::map_dbl(strsplit(cv_keys, ","), ~as.numeric(.[1]))
    col_id <- purrr::map_dbl(strsplit(cv_keys, ","), ~as.numeric(.[2]))
    res <- data.frame(matrix(data = "", nrow = max(row_id) ,ncol= max(col_id)), stringsAsFactors = FALSE)
    if (is.null(na)) { 
        for(key in cv_keys){
            pos <- as.numeric(strsplit(key, ',')[[1]])
            res[pos[1], pos[2]] <- get(key, envir = cell_values)
        }
    } else {
        for(key in cv_keys){
            pos <- as.numeric(strsplit(key, ',')[[1]])
            value <- get(key, envir = cell_values)
            res[pos[1], pos[2]] <- ifelse(value %in% na, NA, value)
        }   
    }
    res <- .change_df_with_col_row_header(res, header, row_header, range)
    return(res)
}

.parse_ods_to_sheets <- function(file) {
    parsed_ods <- .parse_ods_file(file)
    ods_ns <- .extract_namespace(parsed_ods)
    sheets <- .parse_sheets(parsed_ods, ods_ns)
    return(list(sheets, ods_ns))
}

.select_sheet <- function(sheets, ods_ns, which_sheet) {
    if (is.numeric(which_sheet) && which_sheet > length(sheets)) {
        stop("sheet larger than number of sheets in the ods file.", call. = FALSE)
    }
    if (is.character(which_sheet)) {
        sheet_names <- purrr::map_chr(sheets, function(x) xml2::xml_attr(x, "table:name", ods_ns))
        is_in_sheet_names <- stringi::stri_cmp(which_sheet, sheet_names)==0
        if (any(is_in_sheet_names)) {
            which_sheet <- which(is_in_sheet_names)
        } else {
            stop(paste0("No sheet named ", which_sheet, " in the ods file."))
        }
    }
    return(sheets[which_sheet])
}

.select_range <- function(raw_sheet, range) {
    range_select <- cellranger::as.cell_limits(range)
    selected_sheet <- raw_sheet[range_select$ul[1]:range_select$lr[1], range_select$ul[2]:range_select$lr[2]]
    return(selected_sheet)
}

.convert_strings_to_factors <- function(df) {
    i <- purrr::map_lgl(df, is.character)
    df[i] <- lapply(df[i], as.factor)
    return (df)
}

.silent_type_convert <- function(x, verbose = TRUE, na = c("", "NA")) {
    if (verbose) {
        res <- readr::type_convert(df = x, na = na)
    } else {
        suppressMessages({
            res <- readr::type_convert(df = x, na = na)
        })
    }
    return(res)
}

#' Read Data From ODS File
#' 
#' read_ods is a function to read a single sheet from an ods file and return a data frame.
#' read.ods always returns a list of data frames with one data frame per sheet. This is a wrapper to read_ods for backward compatibility with previous version of readODS. Please use read_ods if possible.
#'
#' @aliases read_ods read.ods
#' @param path path to the ods file.
#' @param sheet sheet to read. Either a string (the sheet name), or an integer sheet number. The default is 1.
#' @param col_names logical, indicating whether the file contains the names of the variables as its first line. Default is TRUE.
#' @param col_types Either NULL to guess from the spreadsheet or refer to [readr::type_convert()] to specify cols specification. NA will return a data frame with all columns being "characters".
#' @param na Character vector of strings to use for missing values. By default read_ods converts blank cells to missing data. It can also be set to
#' NULL, so that empty cells are treated as NA.
#' @param skip the number of lines of the data file to skip before beginning to read data. If this parameter is larger than the total number of lines in the ods file, an empty data frame is returned.
#' @param formula_as_formula logical, a switch to display formulas as formulas "SUM(A1:A3)" or as the resulting value "3"... or "8".. . Default is FALSE.
#' @param range selection of rectangle using Excel-like cell range, such as \code{range = "D12:F15"} or \code{range = "R1C12:R6C15"}. Cell range processing is handled by the \code{\link[=cellranger]{cellranger}} package.
#' @param file for read.ods only, path to the ods file.
#' @param formulaAsFormula for read.ods only, a switch to display formulas as formulas "SUM(A1:A3)" or as the resulting value "3"... or "8"..
#' @param row_names logical, indicating whether the file contains the names of the rows as its first column. Default is FALSE.
#' @param strings_as_factors logical, if character columns to be converted to factors. Default is FALSE.
#' @param verbose logical, if messages should be displayed. Default is FALSE.
#' @return A data frame (\code{data.frame}) containing a representation of data in the ods file.
#' @note Currently, ods files that linked to external data source cannot be read. Merged cells cannot be parsed correctly.
#' @author Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @examples
#' \dontrun{
#' # Read a file
#' read_ods("starwars.ods")
#' # Read a specific sheet, e.g. the 2nd sheet
#' read_ods("starwars.ods", sheet = 2)
#' # Read a specific range, e.g. A1:C11
#' read_ods("starwars.ods", sheet = 2, range = "A1:C11")
#' }
#' @export
read_ods <- function(path, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, formula_as_formula = FALSE, range = NULL,
                     row_names = FALSE, strings_as_factors = FALSE, verbose = FALSE) {
    if (missing(path)) {
        stop("No file path was provided for the 'path' argument. Please provide a path to a file to import.")
    }
    res <- .parse_ods_to_sheets(path)
    ods_ns <- res[[2]]
    sheets <- res[[1]]
    target_sheet <- .select_sheet(sheets, ods_ns = ods_ns, which_sheet = sheet)
    cell_values <- .parse_rows(target_sheet, ods_ns, formula_as_formula = formula_as_formula, skip = skip)
    parsed_df <- .convert_to_data_frame(cell_values = cell_values, header = col_names, na = na, row_header = row_names, range = range)
    ## emulate readxl to first select range.
    ## Kill unknown col_types
    if (class(col_types) == 'col_spec') {
        res <- readr::type_convert(df = parsed_df, col_types = col_types, na = na)
    } else if (length(col_types) == 0 & is.null(col_types)) {
        res <- .silent_type_convert(x = parsed_df, verbose = verbose, na = na)
    } else if (length(col_types) == 1 & is.na(col_types[1])) {
        res <- parsed_df
    } else {
        stop("Unknown col_types. Can either be a class col_spec, NULL or NA.", call. = FALSE)
    }
    if (strings_as_factors) {
        res <- .convert_strings_to_factors(res)
    }
    return(res)
}

#' @rdname read_ods
#' @export
read.ods <- function(file = NULL, sheet = NULL, formulaAsFormula = FALSE) {
    warning("read.ods will be deprecated in the next version. Use read_ods instead.")
    if (!is.null(sheet)) {
        return(read_ods(path = file, sheet = sheet, col_names = FALSE, formula_as_formula = formulaAsFormula, skip = 0, na = NULL, col_types = NA))
    } else {
        return(lapply(list_ods_sheets(file), function(x) read_ods(path = file, sheet = x, col_names = FALSE, formula_as_formula = formulaAsFormula, skip = 0, na = NULL, col_types = NA)))
    }
}


#' Get the Number of Sheets in an ODS File
#' 
#' Get the number of sheets in an ods file
#'
#' @param path path to the ods file
#' @return Number of sheets
#' @author Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @seealso
#' use \code{\link{read_ods}} to read the data
#' @export
get_num_sheets_in_ods <- function(path) {
    sheets <- .parse_ods_to_sheets(path)[[1]]
    return(length(sheets))
}

#' @rdname get_num_sheets_in_ods
#' @export
getNrOfSheetsInODS <- function(path) {
    warning("getNrOfSheetsInODS will be deprecated in the next version. Use get_num_sheets_in_ods instead.")
    return(get_num_sheets_in_ods(path))
}

#' List All Sheets in an ODS File
#'
#' List all sheets in an ods file.
#'
#' @param path Path to the ods file
#' @return A character vector of sheet names.
#' @export
list_ods_sheets <- function(path) {
    res <- .parse_ods_to_sheets(path)
    return(purrr::map_chr(res[[1]], function(x) xml2::xml_attr(x, "table:name", res[[2]])))
}

#' @rdname list_ods_sheets
#' @export
ods_sheets <- function(path) {
    warning("ods_sheets will be deprecated in the next version. Use list_ods_sheets instead.")
    list_ods_sheets(path)
}

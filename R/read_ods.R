.change_df_with_col_row_header <- function(x, col_header, row_header){
    if((nrow(x) < 2 && col_header )|| (ncol(x) < 2 && row_header)){
        warning("Cannot make column/row names if this would cause the dataframe to be empty.", call. = FALSE)
        return(x)
    }
    irow <- ifelse(col_header, 2, 1)
    jcol <- ifelse(row_header, 2, 1)

    g <- x[irow:nrow(x), jcol:ncol(x), drop=FALSE] # maintain as dataframe for single column
    rownames(g) <- if(row_header) x[seq(irow, nrow(x)), 1] else NULL # don't want character row headers given by 1:nrow(g)
    colnames(g) <- if(col_header) x[1, seq(jcol, ncol(x))] else cellranger::num_to_letter(seq_len(ncol(g)))
    return(g)
}




## Based on readxl, although the implementation is different.
## If max row is -1, read to end of row. 
## Row and column-numbers are 1-based
.standardise_limits <- function(range, skip) {
    if(is.null(range)){
        skip <- check_nonnegative_integer(skip, "skip")
        limits <- c(
            min_row = skip + 1,
            max_row = -1,
            min_col = 1,
            max_col = -1
        )
    } else {
        if(skip != 0){
            warning("Range and non-zero value for skip given. Defaulting to range.", call. = FALSE)
        }
        tryCatch({
        limits <- cellranger::as.cell_limits(range)
        }, error = function(e){
            stop("Invalid `range`")
        })
        limits <- c(
            min_row = limits[["ul"]][1],
            max_row = limits[["lr"]][1],
            min_col = limits[["ul"]][2],
            max_col = limits[["lr"]][2]
        )
    }
    return(limits)
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

.convert_strings_to_factors <- function(df) {
    i <- purrr::map_lgl(df, is.character)
    df[i] <- lapply(df[i], as.factor)
    return (df)
}

.check_read_args <- function(path,
                        sheet = 1,
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0,
                        formula_as_formula = FALSE,
                        range = NULL,
                        row_names = FALSE,
                        strings_as_factors = FALSE,
                        check_names = FALSE,
                        verbose = FALSE){
    if (missing(path) || !is.character(path)){
        stop("No file path was provided for the 'path' argument. Please provide a path to a file to import.", call. = FALSE)
    }
    if (!file.exists(path)){
        stop("file does not exist", call. = FALSE)
    }
    if (!is.logical(col_names)){
        stop("col_names must be of type `boolean`", call. = FALSE)
    }
    if (!is.logical(formula_as_formula)){
        stop("formula_as_formula must be of type `boolean`", call. = FALSE)
    }
    if (!is.logical(row_names)){
        stop("row_names must be of type `boolean`", call. = FALSE)
    }
    if (!is.logical(strings_as_factors)){
        stop("strings_as_factors must be of type `boolean`", call. = FALSE)
    }
    if (!is.logical(check_names)){
        stop("check_names must be of type `boolean`", call. = FALSE)
    }
    if (!is.logical(verbose)){
        stop("verbose must be of type `boolean`", call. = FALSE)
    }
}


#' Read Data From ODS File
#'
#' read_ods is a function to read a single sheet from an ods file and return a data frame.
#'
#' @param path path to the ods file.
#' @param sheet sheet to read. Either a string (the sheet name), or an integer sheet number. The default is 1.
#' @param col_names logical, indicating whether the file contains the names of the variables as its first line. Default is TRUE.
#' @param col_types Either NULL to guess from the spreadsheet or refer to [readr::type_convert()] to specify cols specification. NA will return a data frame with all columns being "characters".
#' @param na Character vector of strings to use for missing values. By default read_ods converts blank cells to missing data. It can also be set to
#' NULL, so that empty cells are treated as NA.
#' @param skip the number of lines of the data file to skip before beginning to read data. If this parameter is larger than the total number of lines in the ods file, an empty data frame is returned.
#' @param formula_as_formula logical, a switch to display formulas as formulas "SUM(A1:A3)" or as the resulting value "3"... or "8".. . Default is FALSE.
#' @param range selection of rectangle using Excel-like cell range, such as \code{range = "D12:F15"} or \code{range = "R1C12:R6C15"}. Cell range processing is handled by the \code{\link[=cellranger]{cellranger}} package.
#' @param row_names logical, indicating whether the file contains the names of the rows as its first column. Default is FALSE.
#' @param strings_as_factors logical, if character columns to be converted to factors. Default is FALSE.
#' @param check_names logical, passed down to base::data.frame(). Default is FALSE.
#' @param verbose logical, if messages should be displayed. Default is FALSE.
#' @return A data frame (\code{data.frame}) containing a representation of data in the ods file.
#' @note For flat ods files (.fods or .xml), use (\code{read_fods}).
#' @author Peter Brohan <peter.brohan+cran@@gmail.com>, Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
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
read_ods <- function(path,
                        sheet = 1,
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0,
                        formula_as_formula = FALSE,
                        range = NULL,
                        row_names = FALSE,
                        strings_as_factors = FALSE,
                        check_names = FALSE,
                        verbose = FALSE

){
    ## Should use match.call but there's a weird bug if one of the variable names is 'file'
    .read_ods(path,
        sheet,
        col_names,
        col_types,
        na,
        skip,
        formula_as_formula,
        range,
        row_names,
        strings_as_factors,
        check_names,
        verbose,
        flat = FALSE)
}



.read_ods <- function(path,
                        sheet = 1,
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0,
                        formula_as_formula = FALSE,
                        range = NULL,
                        row_names = FALSE,
                        strings_as_factors = FALSE,
                        check_names = FALSE,
                        verbose = FALSE,
                        flat = FALSE){
    .check_read_args(path,
        sheet,
        col_names,
        col_types,
        na,
        skip, 
        formula_as_formula,
        range,
        row_names,
        strings_as_factors,
        check_names,
        verbose)
    # Get cell range info
    limits <- .standardise_limits(range, skip)
    # Get sheet number.
    if (flat){
        sheets <- get_flat_sheet_names_(path, TRUE)
    } else {
        sheets <- get_sheet_names_(path, TRUE)
    }
    sheet_name <- cellranger::as.cell_limits(range)[["sheet"]]
    if(!is.null(range) && !is.na(sheet_name)){
        if(sheet != 1){
            warning("Sheet suggested in range and using sheet argument. Defaulting to range",
                call. = FALSE)
        }
        is_in_sheet_names <- stringi::stri_cmp(sheet_name, sheets) == 0
        if(any(is_in_sheet_names)){
            sheet <- which(is_in_sheet_names)
        } else {
            stop(paste0("No sheet found with name '", sheet_name, "'", sep = ""),
                call. = FALSE)
        }
    } else {
        is_in_sheet_names <- stringi::stri_cmp(sheet, sheets) == 0
        if (!is.numeric(sheet) && any(is_in_sheet_names)){
            sheet <- which(is_in_sheet_names)
        } else if (!is.numeric(sheet)) {
            stop(paste0("No sheet found with name '", sheet, "'", sep = ""), 
                call. = FALSE)
        }
        if (sheet > length(sheets)){
            stop(paste0("File contains only ", length(sheets), " sheets. Sheet index out of range.",
                call. = FALSE))
        }
    }

    if(flat){
    strings <- read_flat_ods_(path,
        limits["min_row"],
        limits["max_row"],
        limits["min_col"],
        limits["max_col"],
        sheet,
        formula_as_formula)
    } else {
    strings <- read_ods_(path,
        limits["min_row"],
        limits["max_row"],
        limits["min_col"],
        limits["max_col"],
        sheet,
        formula_as_formula)
    }
    if(strings[1] == 0 || strings[2] == 0){
        warning("empty sheet, return empty data frame.", call. = FALSE)
        return(data.frame())
    }
    res <- as.data.frame(
            matrix(
                strings[-1:-2],
                ncol = strtoi(strings[1]),
        byrow = TRUE),
        stringsAsFactors = FALSE)
    res <- .change_df_with_col_row_header(res, col_names, row_names)
    res <- data.frame(res, check.names = check_names)
    if (inherits(col_types, 'col_spec')){
        res <- readr::type_convert(df = res, col_types = col_types, na = na)
    } else if (length(col_types) == 0 && is.null(col_types)){
        res <- .silent_type_convert(x = res, verbose = verbose, na = na)
    } else if (length(col_types) == 1 && is.na(col_types[1])) {
        {} #Pass
    } else {
        stop("Unknown col_types. Can either be a class col_spec, NULL or NA.",
            call. = FALSE)
    }

    if (strings_as_factors) {
        res <- .convert_strings_to_factors(res)
    }

    return(res)

}
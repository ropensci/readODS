#' Read Data From FODS File
#'
#' read_flat_ods is a function to read a single sheet from a flat ods file and return a data frame. 
#'
#' @param path path to the fods file.
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
#' @note For packaged ods files (.ods), use (\code{read_ods})
#' @author Peter Brohan <peter.brohan+cran@@gmail.com>, Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @examples
#' \dontrun{
#' # Read a file
#' read_fods("starwars.fods")
#' # Read a specific sheet, e.g. the 2nd sheet
#' read_fods("starwars.fods", sheet = 2)
#' # Read a specific range, e.g. A1:C11
#' read_fods("starwars.fods", sheet = 2, range = "A1:C11")
#' }
#' @export
read_fods <- function(path,
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
        flat = TRUE)
}
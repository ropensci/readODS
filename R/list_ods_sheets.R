#' List all sheets in an ODS File
#' 
#' List all sheets in an ods file.
#' 
#' @param path Path to the ods file
#' @param include_external_data A boolean value to show or hide sheets containing linked data (default false)
#' @return A character vector of sheet names.
#' @author Peter Brohan <peter.brohan+cran@@gmail.com>, Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @seealso
#' use \code{\link{read_ods}} to read the data
#' @export
list_ods_sheets <- function(path, include_external_data = FALSE) {
    return(ods_get_sheet_names_(path, include_external_data))
}

#' Get the Number of Sheets in an ODS File
#'
#' Get the number of sheets in an ods file
#'
#' @param path path to the ods file
#' @param include_external_data A boolean value declaring if external data sheets should be counted
#' @return Number of sheets
#' @author Peter Brohan <peter.brohan+cran@@gmail.com>, Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @seealso
#' use \code{\link{read_ods}} to read the data
#' @export
get_num_sheets_in_ods <- function(path, include_external_data = FALSE) {
    sheets <- ods_get_sheet_names_(path, include_external_data)
    return(length(sheets))
}
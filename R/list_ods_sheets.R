#' List all sheets in an ODS File
#' 
#' List all sheets in an ods file.
#' 
#' @param path Path to the ods file
#' @param include_external_data A boolean value to show or hide sheets containing archived linked data (default false)
#' @return A character vector of sheet names.
#' @author Peter Brohan <peter.brohan+cran@@gmail.com>, Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @examples
#' \dontrun{
#' # Get the list of names of sheets
#' list_ods_sheets("starwars.ods")
#' }
#' @seealso
#' use \code{\link{read_ods}} to read the data
#' @export
list_ods_sheets <- function(path, include_external_data = FALSE) {
    return(get_sheet_names_(path, include_external_data))
}

#' Get the Number of Sheets in an ODS File
#'
#' Get the number of sheets in an ods file
#'
#' @param path path to the ods file
#' @param include_external_data A boolean value declaring if sheets holding archived linked data should be included
#' @return Number of sheets
#' @author Peter Brohan <peter.brohan+cran@@gmail.com>, Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @examples
#' \dontrun{
#' # Get the number of sheets
#' get_num_sheets_in_ods("starwars.ods")
#' }
#' @seealso
#' use \code{\link{read_ods}} to read the data
#' @export
get_num_sheets_in_ods <- function(path, include_external_data = FALSE) {
    sheets <- get_sheet_names_(path, include_external_data)
    return(length(sheets))
}

#' List all sheets in an FODS File
#' 
#' List all sheets in an fods file.
#' 
#' @param path Path to the fods file
#' @param include_external_data A boolean value to show or hide sheets containing archived linked data (default false)
#' @return A character vector of sheet names.
#' @author Peter Brohan <peter.brohan+cran@@gmail.com>, Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @examples
#' \dontrun{
#' # Get the list of names of sheets
#' list_fods_sheets("starwars.fods")
#' }
#' @seealso
#' use \code{\link{read_fods}} to read the data
#' @export
list_fods_sheets <- function(path, include_external_data = FALSE) {
    return(get_flat_sheet_names_(path, include_external_data))
}

#' Get the Number of Sheets in an FODS File
#'
#' Get the number of sheets in an fods file
#'
#' @param path path to the fods file
#' @param include_external_data A boolean value declaring if sheets holding archived linked data should be included
#' @return Number of sheets
#' @author Peter Brohan <peter.brohan+cran@@gmail.com>, Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @examples
#' \dontrun{
#' # Get the number of sheets
#' get_num_sheets_in_fods("starwars.fods")
#' }
#' @seealso
#' use \code{\link{read_fods}} to read the data
#' @export
get_num_sheets_in_fods <- function(path, include_external_data = FALSE) {
    sheets <- get_flat_sheet_names_(path, include_external_data)
    return(length(sheets))
}
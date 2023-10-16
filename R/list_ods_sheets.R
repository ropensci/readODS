.list_ods_sheets <- function(path, include_external_data = FALSE, .list_function) {
    if (!file.exists(path)) {
        stop(path, " does not exist", call. = FALSE)
    }
    return(.list_function(file = normalizePath(path, mustWork = FALSE), include_external_data = include_external_data))
}

#' Get information in an (F)ODS File
#'
#' `list_(f)ods_sheets` lists all sheets in an (f)ods file.
#'
#' @param path Path to the (f)ods file
#' @param include_external_data A boolean value to show or hide sheets containing archived linked data (default false)
#' @return A character vector of sheet names
#' @details The default "include_external_data" for `ods_sheets` is TRUE to maintain compatibility with version 1 of readODS. It will change to `TRUE` in version 3.
#' @author Peter Brohan <peter.brohan+cran@@gmail.com>, Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @examples
#' \dontrun{
#' # Get the list of names of sheets
#' list_ods_sheets("starwars.ods")
#' list_fods_sheets("starwars.fods")
#' }
#' @seealso
#' use \code{\link{read_ods}} to read the data
#' @export
list_ods_sheets <- function(path, include_external_data = FALSE) {
    .list_ods_sheets(path, include_external_data = include_external_data, .list_function = get_sheet_names_)
}

#' @rdname list_ods_sheets
#' @export
list_fods_sheets <- function(path, include_external_data = FALSE) {
    .list_ods_sheets(path, include_external_data = include_external_data, .list_function = get_flat_sheet_names_)
}

#' @rdname list_ods_sheets
#' @export
ods_sheets <- function(path) {
    list_ods_sheets(path, include_external_data = TRUE)
}

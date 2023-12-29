.list_ods_sheets <- function(path, include_external_data = FALSE, .list_function) {
    if (!file.exists(path)) {
        stop(path, " does not exist", call. = FALSE)
    }
    return(.list_function(file = normalizePath(path, mustWork = FALSE), include_external_data = include_external_data))
}

#' Get information in an (F)ODS File
#'
#' `list_ods_sheets` lists all sheets in an (f)ods file. The function can be used for listing sheets in both ods and flat ods files. (\code{list_fods_sheets}) is also available, which can only list sheets in flat ods files.
#'
#' @param path Path to the (f)ods file
#' @param include_external_data A boolean value to show or hide sheets containing archived linked data (default false)
#' @return A character vector of sheet names
#' @details The default "include_external_data" for `ods_sheets` is TRUE to maintain compatibility with version 1 of readODS. It will change to `TRUE` in version 3.
#' @author Peter Brohan <peter.brohan+cran@@gmail.com>, Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @inheritParams read_ods
#' @examples
#' \dontrun{
#' # Get the list of names of sheets
#' list_ods_sheets("starwars.ods")
#' list_ods_sheets("starwars.fods")
#' # Using list_fods_sheets, although you don't have to
#' list_fods_sheets("starwars.fods")
#' }
#' @seealso
#' use \code{\link{read_ods}} to read the data
#' @export
list_ods_sheets <- function(path, include_external_data = FALSE, ods_format = c("auto", "ods", "fods"), guess = FALSE) {
    ods_format <- .determine_ods_format(path = path, guess = guess, ods_format = match.arg(ods_format))
    if (ods_format == "ods") {
        return(.list_ods_sheets(path, include_external_data = include_external_data, .list_function = get_sheet_names_))
    }
    .list_ods_sheets(path, include_external_data = include_external_data, .list_function = get_flat_sheet_names_)
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

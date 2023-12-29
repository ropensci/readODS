.return_zerorow <- function(x, row_header, .name_repair) {
    jcol <- ifelse(row_header, 2, 1)
    col_n <- vctrs::vec_as_names(as.character(x[1,jcol:ncol(x)]), repair = .name_repair)
    g <- data.frame(matrix(data = NA_character_, ncol = length(col_n), nrow = 0))
    colnames(g) <- col_n
    return(g)
}

.change_df_with_col_row_header <- function(x, col_header, row_header, .name_repair) {
    if (nrow(x) == 1 && col_header && isFALSE(getOption("readODS.v200", FALSE))) {
        return(.return_zerorow(x, row_header, .name_repair))
    }
    if (((nrow(x) < 2 && col_header ) || (ncol(x) < 2 && row_header)) &&
        isTRUE(getOption("readODS.v200", FALSE))) {
        ## 2.0.0 behavior
        warning("Cannot make column/row names if this would cause the dataframe to be empty.", call. = FALSE)
        return(x)
    }
    irow <- ifelse(col_header, 2, 1)
    jcol <- ifelse(row_header, 2, 1)

    g <- x[irow:nrow(x), jcol:ncol(x), drop = FALSE] # maintain as dataframe for single column


    rownames(g) <- if(row_header) x[seq(irow, nrow(x)), 1] else NULL # don't want character row headers given by 1:nrow(g)
    cols <- ncol(x)
    if (row_header) {
        cols <- cols - 1
    }
    col_n <- if(col_header) x[1, seq(jcol, ncol(x))] else c(rep("", cols))
    colnames(g) <- vctrs::vec_as_names(unlist(col_n), repair = .name_repair)
    return(g)
}

## Based on readxl, although the implementation is different.
## If max row is -1, read to end of row.
## Row and column-numbers are 1-based
.standardise_limits <- function(range, skip, n_max) {
    if(is.null(range)) {
        skip <- check_nonnegative_integer(x = skip, argument = "skip")
        n_max <- check_nonnegative_integer(x = n_max, argument = "n_max")
        if (n_max == Inf) {
            max_row <- -1
        } else {
            max_row <- n_max + 1
        }
        limits <- c(
            min_row = skip + 1,
            max_row = max_row,
            min_col = 1,
            max_col = -1
        )
    } else {
        if(skip != 0 || n_max != Inf) {
            warning("Range and non-default value for skip or n_max given. Defaulting to range.", call. = FALSE)
        }
        tryCatch({
            limits <- cellranger::as.cell_limits(range)
        }, error = function(e) {
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

.convert_strings_to_factors <- function(df) {
    i <- vapply(df, is.character, logical(1))
    df[i] <- lapply(df[i], as.factor)
    return(df)
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
                        verbose = FALSE,
                        as_tibble = TRUE,
                        trim_ws = TRUE,
                        n_max = Inf) {
    if (!file.exists(path)) {
        stop("file does not exist", call. = FALSE)
    }
    if (!is.logical(col_names)) {
        stop("col_names must be of type `boolean`", call. = FALSE)
    }
    if (!is.logical(formula_as_formula)) {
        stop("formula_as_formula must be of type `boolean`", call. = FALSE)
    }
    if (!is.logical(row_names)) {
        stop("row_names must be of type `boolean`", call. = FALSE)
    }
    if (!is.logical(strings_as_factors)) {
        stop("strings_as_factors must be of type `boolean`", call. = FALSE)
    }
    if (!is.logical(verbose)) {
        stop("verbose must be of type `boolean`", call. = FALSE)
    }
    if (!is.logical(as_tibble)) {
        stop("as_tibble must be of type `boolean", call. = FALSE)
    }
    if (!is.logical(trim_ws)) {
        stop("trim_ws must be of type `boolean", call. = FALSE)
    }
    if (row_names && as_tibble) {
        stop("Tibbles do not support row names. To use row names, set as_tibble to false", call. = FALSE)
    }
    if (!inherits(col_types, "col_spec") &&
        isFALSE(is.na(col_types)) &&
        isFALSE(is.null(col_types)) &&
        isFALSE(is.character(col_types)) &&
        isFALSE(is.list(col_types))) {
        stop("Unknown col_types. Can either be a class col_spec, list, character, NULL or NA.",
             call. = FALSE)
    }
    if (!is.numeric(n_max)) {
        stop("`n_max` must be numeric.", call. = FALSE)
    }
}

.return_empty <- function(as_tibble = FALSE) {
    if (getOption("readODS.v200", FALSE)) {
        warning("empty sheet, return empty data frame.", call. = FALSE)
    }
    if (as_tibble) {
        return(tibble::tibble())
    }
    return(data.frame())
}

.type_convert <- function(df, col_types = NULL, verbose = TRUE, na = c("", "NA"), trim_ws = TRUE) {
    if (verbose) {
        res <- readr::type_convert(df = df, col_types, na = na)
    } else {
        suppressMessages({
            res <- readr::type_convert(df = df, col_types, na = na, trim_ws = trim_ws)
        })
    }
    return(res)
}

.handle_col_types <- function(res, col_types, verbose, na, trim_ws) {
    if (isTRUE(is.na(col_types)) || nrow(res) == 0) {
        return(res)
    }
    .type_convert(df = res, col_types = col_types, verbose = verbose, na = na, trim_ws = trim_ws)
}

## standardise `sheet` parameter as a number, i.e. sheet_index
.standardise_sheet <- function(sheet, sheet_names, range) {
    sheet_from_range <- cellranger::as.cell_limits(range)[["sheet"]]
    if (!is.null(range) && !is.na(sheet_from_range)) {
        if (sheet != 1) {
            warning("Sheet suggested in range and using sheet argument. Defaulting to range",
                    call. = FALSE)
        }
        sheet <- sheet_from_range ## override; should be a sheet_name
    }
    is_in_sheet_names <- stringi::stri_cmp(e1 = sheet, e2 = sheet_names) == 0
    if (!is.numeric(sheet) && !any(is_in_sheet_names)) {
        stop(paste0("No sheet found with name '", sheet, "'", sep = ""),
             call. = FALSE)
    }
    if (is.numeric(sheet) && sheet > length(sheet_names)) {
        stop(paste0("File contains only ", length(sheet_names), " sheets. Sheet index out of range.",
                    call. = FALSE))
    }
    if (!is.numeric(sheet)) {
        sheet_index <- which(is_in_sheet_names)
    } else {
        sheet_index <- sheet
    }
    return(sheet_index)
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
                      verbose = FALSE,
                      as_tibble = TRUE,
                      .name_repair = "unique",
                      flat = FALSE,
                      trim_ws = TRUE,
                      n_max = Inf) {
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
        verbose,
        as_tibble,
        trim_ws,
        n_max)
    path <- normalizePath(path)
    if (flat) {
        .get_sheet_names_func <- get_flat_sheet_names_
        .read_ods_func <- read_flat_ods_
    } else {
        .get_sheet_names_func <- get_sheet_names_
        .read_ods_func <- read_ods_
    }
    ## Get cell range info
    limits <- .standardise_limits(range, skip, n_max)
    sheet_index <- .standardise_sheet(sheet = sheet, sheet_names = .get_sheet_names_func(file = path, include_external_data = TRUE),
                                      range = range)
    strings <- .read_ods_func(file = path,
                              start_row = limits["min_row"],
                              stop_row = limits["max_row"],
                              start_col = limits["min_col"],
                              stop_col = limits["max_col"],
                              sheet_index = sheet_index,
                              formula_as_formula = formula_as_formula)

    if (((strings[1] == 0 || strings[2] == 0)) &&
        isTRUE(getOption("readODS.v200", FALSE))) {
        return(.return_empty(as_tibble = as_tibble))
    }
    if (((strings[1] == 0 || strings[2] == 0) || (strings[1] == 1 && row_names)) &&
        isFALSE(getOption("readODS.v200", FALSE))) {
        return(.return_empty(as_tibble = as_tibble))
    }
    res <- as.data.frame(
        matrix(
            strings[-1:-2],
            ncol = strtoi(strings[1]),
            byrow = TRUE),
        stringsAsFactors = FALSE)
    res <- .change_df_with_col_row_header(x = res, col_header = col_names, row_header = row_names, .name_repair = .name_repair)
    res <- .handle_col_types(res, col_types = col_types, verbose = verbose, na = na, trim_ws = trim_ws)
    if (strings_as_factors) {
        res <- .convert_strings_to_factors(df = res)
    }
    if (as_tibble) {
        res <- tibble::as_tibble(x = res, .name_repair = .name_repair)
    }
    return(res)
}

.determine_ods_format <- function(path, guess = FALSE, ods_format = "auto") {
    if (missing(path) || !is.character(path)) {
        stop("No file path was provided for the 'path' argument. Please provide a path to a file to import.", call. = FALSE)
    }
    if (ods_format != "auto") {
        return(ods_format)
    }
    ext <- tolower(tools::file_ext(path))
    formats <- c(
        ods = "ods",
        fods = "fods",
        xml = "fods"
    )
    if (!isTRUE(guess)) {
        ext <- unname(formats[ext])
        if (is.na(ext)) {
            return("ods")
        }
        return(ext)
    }
    zip_sig <- as.raw(c(
        "0x50", "0x4B", "0x03", "0x04"
    ))
    if (identical(zip_sig, readBin(path, n = 4, what = "raw"))) {
        return("ods")
    }
    return("fods")
}

#' Read Data From (F)ODS File
#'
#' read_ods is a function to read a single sheet from an (f)ods file and return a data frame. The function can be used for reading both ods and flat ods files.
#' (\code{read_fods}) is also available, which can only read flat ods files.
#'
#' @param path path to the (f)ods file.
#' @param sheet sheet to read. Either a string (the sheet name), or an integer sheet number. The default is 1.
#' @param col_names logical, indicating whether the file contains the names of the variables as its first line. Default is TRUE.
#' @param col_types Either NULL to guess from the spreadsheet or refer to [readr::type_convert()] to specify cols specification. It can also be a shorthand such as "ccf" ("character", "character", "factor"), a list, or an object created by [readr::cols()]. NA will return a data frame with all columns being "characters". Please note that it will not speed up the reading by a lot by specifying this parameter explicitly. It is more for accuracy.
#' @param na Character vector of strings to use for missing values. By default read_ods converts blank cells to missing data. It can also be set to
#' NULL, so that empty cells are treated as NA.
#' @param skip the number of lines of the data file to skip before beginning to read data. If this parameter is larger than the total number of lines in the ods file, an empty data frame is returned.
#' @param formula_as_formula logical, a switch to display formulas as formulas "SUM(A1:A3)" or as the resulting value "3"... or "8".. . Default is FALSE.
#' @param range selection of rectangle using Excel-like cell range, such as \code{range = "D12:F15"} or \code{range = "R1C12:R6C15"}. Cell range processing is handled by the \code{\link[=cellranger]{cellranger}} package. If sheet name is in the range, such as \code{range = "Sheet2!A2:B7"}, this sheet name is used instead of the provided `sheet`. If `sheet` is not the default value (1), a warning is given.
#' @param row_names logical, indicating whether the file contains the names of the rows as its first column. Default is FALSE.
#' @param strings_as_factors logical, if character columns to be converted to factors. Default is FALSE.
#' @param verbose logical, if messages should be displayed. Default is FALSE.
#' @param as_tibble logical, if the output should be a tibble (as opposed to a data.frame). Default is TRUE.
#' @param .name_repair A string or function passed on as `.name_repair` to [tibble::as_tibble()]
#'  - `"minimal"`: No name repair
#'  - `"unique"` : Make sure names are unique and not empty
#'  - `"check_unique"`: Check names are unique, but do not repair
#'  - `"universal"` : Checks names are unique and valid R variables names in scope
#'  - A function to apply custom name repair.
#'
#'  Default is `"unique"`.
#'
#' @param ods_format character, must be "auto", "ods" or "fods". The default "auto" is to determine the format automatically. By default, the format is determined by file extension, unless `guess` is `FALSE`.
#' @param guess logical, If the file extension is absent or not recognized, this
#'   controls whether we attempt to guess format based on the file signature or
#'   "magic number".
#' @param trim_ws logical, should leading and trailing whitespace be trimmed?
#' @param n_max numeric, Maximum number of data rows to read. Ignored if `range` is given.
#' @return A tibble (\code{tibble}) or data frame (\code{data.frame}) containing a representation of data in the (f)ods file.
#' @author Peter Brohan <peter.brohan+cran@@gmail.com>, Chung-hong Chan <chainsawtiney@@gmail.com>, Gerrit-Jan Schutten <phonixor@@gmail.com>
#' @examples
#' \dontrun{
#' # Read an ODS file
#' read_ods("starwars.ods")
#' # Read a specific sheet, e.g. the 2nd sheet
#' read_ods("starwars.ods", sheet = 2)
#' # Read a specific range, e.g. A1:C11
#' read_ods("starwars.ods", sheet = 2, range = "A1:C11")
#' # Read an FODS file
#' read_ods("starwars.fods")
#' # Read a specific sheet, e.g. the 2nd sheet
#' read_ods("starwars.fods", sheet = 2)
#' # Read a specific range, e.g. A1:C11
#' read_ods("starwars.fods", sheet = 2, range = "A1:C11")
#' # Give a warning and read from Sheet1 (not 2)
#' read_ods("starwars.fods", sheet = 2, range = "Sheet1!A1:C11")
#' # Specifying col_types as shorthand, the third column as factor; other by guessing
#' read_ods("starwars.ods", col_types = "??f")
#' # Specifying col_types as list
#' read_ods("starwars.ods", col_types = list(species = "f"))
#' # Using read_fods, although you don't have to
#' read_ods("starwars.fods")
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
                     verbose = FALSE,
                     as_tibble = TRUE,
                     .name_repair = "unique",
                     ods_format = c("auto", "ods", "fods"),
                     guess = FALSE,
                     trim_ws = TRUE,
                     n_max = Inf) {
    ods_format <- .determine_ods_format(path = path, guess = guess, ods_format = match.arg(ods_format))
    ## Should use match.call but there's a weird bug if one of the variable names is 'file'
    .read_ods(path = path,
        sheet = sheet,
        col_names = col_names,
        col_types = col_types,
        na = na,
        skip = skip,
        formula_as_formula = formula_as_formula,
        range = range,
        row_names = row_names,
        strings_as_factors = strings_as_factors,
        verbose = verbose,
        as_tibble = as_tibble,
        .name_repair = .name_repair,
        flat = ods_format == "fods",
        trim_ws = trim_ws,
        n_max = n_max)
}

#' @rdname read_ods
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
                      verbose = FALSE,
                      as_tibble = TRUE,
                      .name_repair = "unique",
                      trim_ws = TRUE,
                      n_max = Inf) {
    ## Should use match.call but there's a weird bug if one of the variable names is 'file'
    .read_ods(path = normalizePath(path, mustWork = FALSE),
              sheet = sheet,
              col_names = col_names,
              col_types = col_types,
              na = na,
              skip = skip,
              formula_as_formula = formula_as_formula,
              range = range,
              row_names = row_names,
              strings_as_factors = strings_as_factors,
              verbose = verbose,
              as_tibble = as_tibble,
              .name_repair = .name_repair,
              flat = TRUE,
              trim_ws = trim_ws,
              n_max = n_max)
}

check_nonnegative_integer <- function(x, argument) {
    if(length(x) != 1 || !is.numeric(x) || floor(x) != x || is.na(x) || x < 0) {
        stop(paste0(argument, " must be a positive integer"), call. = FALSE)
    }
    return(x)
}

## To use inside cpp

.escape_xml <- function(x) {
    stringi::stri_replace_all_fixed(str = stringi::stri_enc_toutf8(x), pattern = c("&", "\"", "<", ">", "'"),
                                    replacement = c("&amp;", "&quot;", "&lt;", "&gt;", "&apos;"), vectorize_all = FALSE)
}

## for single column, so `column_type`

.sanitize <- function(x, column_type) {
    if (column_type == "string") {
        return(.escape_xml(as.character(x)))
    }
    as.character(x)
}
.sanitize_df <- function(x, column_types) {
    mapply(.sanitize, x = x, column_type = column_types, SIMPLIFY = FALSE)
}

.get_sanitized_dimnames <- function(x, cols = TRUE) {
    if (cols) {
        return(.escape_xml(colnames(x)))
    }
    return(.escape_xml(rownames(x)))
}

.get_column_types <- function(x) {
    ## if ncol == 0, without as.character would return `logical(0)`
    as.character(ifelse(unlist(lapply(x, function(x) class(x)[1])) %in% c("integer", "numeric"), "float", "string"))
}

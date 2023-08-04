# Taken from readxl

# Called only from C++ code, but currently needs to be implemented in R.
zip_buffer <- function(zip_path, file_path) {
    files <- zip::zip_list(zip_path)
    indx <- match(file_path, files$filename)
    if (is.na(indx)) {
        stop("Couldn't find '", file_path, "' in '", zip_path, "'", call. = FALSE)
    }
    size <- files$uncompressed_size[indx]
    con <- unz(zip_path, file_path, open = "rb")
    on.exit(close(con), add = TRUE)
    readBin(con, raw(), n = size)
}

zip_has_file <- function(zip_path, file_path) {
    file_path %in% zip::zip_list(zip_path)$filename
}

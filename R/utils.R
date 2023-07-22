check_nonnegative_integer <- function(x, argument){
    if(length(x) != 1 || !is.numeric(x) || floor(x) != x || is.na(x) || x < 0){
        stop(paste0(argument, " must be a positive integer"), call. = FALSE)
    }
    return(x)
}
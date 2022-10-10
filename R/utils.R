INVALID_FILENAME_CHARACTERS <- c("/", "\\", "?", "%", "*", ":", "|", "\"", "<", ">", ".", ",", ";", "=", " ")

#' @importFrom stringr str_detect fixed
#' @importFrom purrr map_lgl
validate_name <- function(name) {
    if (any(map_lgl(INVALID_FILENAME_CHARACTERS, function(c) str_detect(name, fixed(c))))) {
        msg <- sprintf("name '%s' should not have the following characters: ",
                       name,
                       paste(INVALID_FILENAME_CHARACTERS, collapse = " "))
        stop(msg)
    }
}

expr_to_chr <- function(val, collapse = "\n") {
    if(missing(val)) ""
    else paste(deparse(val), collapse = collapse)
}


is_subset <- function(small, large) {
    setequal(intersect(small, large), small)
}

time_to_df <- function(time) {
    time <- unname(unclass(time))
    data.frame(process = time[1], real = time[2])
}

#' @importFrom stringr str_pad
pad_max <- function(chrs, side = "right") {
    str_pad(chrs, max(nchar(chrs)), side = side)
}

MISSING <- new.env(parent = emptyenv())

is_missing <- function(val) {
    identical(val, MISSING)
}

is_chr <- function(val, size = 1, na = FALSE) {
    is.character(val) && length(val) == size && (!any(is.na(val)) || na)
}

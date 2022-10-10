.invalid_chrs <- c("/", "\\", "?", "%", "*", ":", "|",
                   "\"", "<", ">", ".", ",", ";", "=", " ")

#' @importFrom stringr str_detect fixed
#' @importFrom purrr keep
validate_name <- function(name) {
    chrs <- keep(.invalid_chrs,
                 function(c) str_detect(name, fixed(c)))
    if (length(chrs) > 0) {
        chrs <- paste(chrs, collapse = ",")
        msg <- sprintf("rule name '%s' cannot contain %s", name, chrs)
        stop(msg)
    }
}

expr_to_chr <- function(val, collapse = "\n") {
    if (missing(val)) ""
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

.missing_val <- new.env(parent = emptyenv())

missing_val <- function() .missing_val

is_missing <- function(val) {
    identical(val, .missing_val)
}

is_chr <- function(val, size = 1, na = FALSE) {
    is.character(val) && length(val) == size && (!any(is.na(val)) || na)
}

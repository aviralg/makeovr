

#' Construct path to a file or directory starting from rule's data directory
#'
#' @param rule The rule object.
#' @param ... character vectors, if any values are NA, the result will also be
#'   NA. The paths follow the recycling rules used in the tibble package,
#'   namely that only length 1 arguments are recycled.
#' @param ext An optional extension to append to the generated path.
#'
#' @export
#'
#' @importFrom fs path
rule_path <- function(rule, ..., ext = "") {
    dir <- rule_data_dir(rule)
    path(dir, ..., ext = ext)
}

#' Read and write data
#'
#' @description
#' These functions first construct a file path relative to rule's data directory
#' and read or write content.
#'
#' * `rule_read()` reads data.
#' * `rule_write()` writes data.
#'
#' @param rule The rule object.
#' @param ... character vectors, if any values are NA, the result will also be
#'   NA. The paths follow the recycling rules used in the tibble package,
#'   namely that only length 1 arguments are recycled.
#' @param ext An optional extension to append to the generated path.
#' @param content Content to write to file
#'
#' @export
#' @name read
rule_read <- function(rule, ..., ext = "") {
    path <- rule_path(rule, ..., ext = ext)
    ext_read(path)
}

#' @export
#' @rdname read
rule_write <- function(rule, content, ..., ext = "") {
    path <- rule_path(rule, ..., ext = ext)
    ext_write(path, content)
    content
}

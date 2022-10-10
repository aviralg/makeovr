
#' @importFrom fs path
hist_file <- function(rule, version) {
    hist_dir <- rule_hist_dir(rule)
    hist_file <- path(hist_dir, version, ext = "rds")
}

hist_write <- function(rule, version, ...) {
    f <- hist_file(rule, version)
    content <- list(...)
    ext_write(f, content)
}

#' @importFrom fs path path_file path_ext_remove
hist_read <- function(rule, version) {
    f <- hist_file(rule, version)

    list(version = version,
         content = ext_read(f))
}

#' @importFrom fs dir_ls path_ext_remove path_file
#' @importFrom fs file_info
hist_tbl <- function(rule) {
    hist_dir <- rule_hist_dir(rule)

    paths <- dir_ls(hist_dir, type = "file", recurse = FALSE)

    versions <- path_ext_remove(path_file(paths))

    path_info <- file_info(paths)

    origin <- path_info$birth_time

    indices <- order(origin)

    n <- length(paths)

    data.frame(
        dir     = rep.int(hist_dir, n),
        version = versions[indices],
        path    = paths[indices],
        origin  = origin[indices]
    )
}

#' Read history of rule executions.
#'
#' @param rule Rule.
#' @param version Version specification.
#' @export
#' @importFrom purrr map
#' @importFrom fs path_file path_ext_remove
rule_hist <- function(rule, version = NULL) {
    hist_dir <- rule_hist_dir(rule)
    name <- rule_name(rule)

    versions <- normalize_versions(rule, version)

    res <- map(versions, function(version) hist_read(rule, version))
    names(res) <- versions
    res
}

normalize_version_helper <- function(version, len) {

    if(is.double(version)) {

        ver <- as.integer(version)

        if(ver != version) {
            stop(sprintf("history version '%d' should be integer or a whole number", version))
        }

        version <- ver
    }

    if(version == 0) {
        stop(sprintf("history version index cannot be 0"))
    }

    if(version < 0) {
        version <- as.integer(len + version + 1)
    }

    if(version > len) {
        stop(sprintf("cannot access history index %d from %d histories", version, len))
    }

    version
}

#' @importFrom purrr map_int
normalize_versions <- function(rule, version = NULL) {
    if(is.character(version)) {
        version
    }

    else if(is.numeric(version) || is.null(version)) {
        tbl <- hist_tbl(rule)

        if(!is.null(version)) {
            n <- nrow(tbl)
            version <- map_int(version, normalize_version_helper, n)

            tbl$version[version]
        }

        tbl$version
    }
}

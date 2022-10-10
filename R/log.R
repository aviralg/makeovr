
#' Logging inside rules
#'
#' @description
#' These functions can be used for logging during rule execution.
#' The logs are printed on the console and retained as part of the rule's
#' execution history.
#'
#' @param rule The rule
#' @param type Log type
#' @param fmt  A character vector of format strings.
#' @param ...  values to be passed into ‘fmt’.  Only logical, integer, real and
#' character vectors are supported, but some coercion will be done.
#'
#' @export
#' @name log
rule_log <- function(rule, type, fmt, ...) {
    msg <- sprintf(fmt, ...)
    time <- format(Sys.time(), "%Y-%b-%d-%I-%p-%M-%S")
    rule_logs_add(rule, type, time, msg)

    cat(sprintf("[%s]-[%s]-%s\n", type, time, msg))

    invisible(NULL)
}

#' @export
#' @rdname log
rule_warn <- function(rule, fmt, ...) {
    rule_log(rule, "warn", fmt, ...)
}

#' @export
#' @rdname log
rule_msg <- function(rule, fmt, ...) {
    rule_log(rule, "msg", fmt, ...)
}

#' @export
#' @rdname log
rule_dbg <- function(rule, fmt, ...) {
    rule_log(rule, "dbg", fmt, ...)
}

#' @export
#' @rdname log
rule_err <- function(rule, fmt, ...) {
    rule_log(rule, "err", fmt, ...)
}

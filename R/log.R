
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
#' @name log
rule_log <- function(rule, type, msg) {
    time <- format(Sys.time(), "%Y-%b-%d-%I-%p-%M-%S")

    rule_logs_add(rule, type, time, msg)

    invisible(NULL)
}

#' @importFrom cli cli_inform format_message
#' @export
#' @rdname log
rule_msg <- function(rule,
                     message,
                     ...,
                     .envir = parent.frame()) {

    msg <- format_message(message, .envir = .envir)
    rule_log(rule, "msg", msg)

    cli_inform(message, ..., .envir = .envir)
}

#' @importFrom cli cli_inform format_message
#' @export
#' @rdname log
rule_dbg <- function(rule,
                     message,
                     ...,
                     .envir = parent.frame()) {

    msg <- format_message(message, .envir = .envir)
    rule_log(rule, "dbg", msg)

    if (rule$debug == TRUE) {
        cli_inform(message, ..., .envir = .envir)
    }
}

#' @importFrom cli cli_abort format_error
#' @export
#' @rdname log
rule_err <- function(rule,
                     message,
                     ...,
                     call = .envir,
                     .envir = parent.frame(),
                     .frame = .envir) {

    msg <- format_error(message, .envir = .envir)
    rule_log(rule, "err", msg)

    cli_abort(message, ..., call = call, .envir = .envir, .frame = .frame)
}

#' @importFrom cli cli_warn format_warning
#' @export
#' @rdname log
rule_warn <- function(rule,
                      message,
                      ...,
                      .envir = parent.frame()) {

    msg <- format_warning(message, .envir = .envir)
    rule_log(rule, "warn", msg)

    cli_warn(message, ..., .envir = .envir)
}

.global_options <- list(
    make_option(c("--help", "-h"),
                action = "store_true",
                default = FALSE,
                help = "Print this message and exit."),

    make_option("--ignore-deps",
                action = "store_true",
                default = FALSE,
                help = "Ignore dependencies and run rule directly."),

    make_option("--debug",
                action = "store_true",
                default = FALSE,
                help = "Print various types of debugging information."),

    make_option("--dry-run",
                action = "store_true",
                default = FALSE,
                help = "Print execution sequence without running the rules."),

    make_option("--port",
                action = "store",
                help = "Display web interface on the specified port."),

    make_option("--store",
                action = "store",
                default = NA_character_,
                help = "Use this directory for data and history."),

    make_option("--version",
                action = "store",
                default = "<current-date-time>",
                help = "Use this name for execution history version.")
)


#' @importFrom optparse make_option
par_opt <- function(par, name, desc) {

    help <- desc[[name]]

    help <- sprintf("[%s] %s", name, help)

    make_option(paste0("--", par),
                action  = "store",
                help    = help)
}

rule_pars <- function(rule) {
    target <- rule_target(rule)
    target_pars <- names(formals(target))

    action <- rule_action(rule)
    action_pars <- names(formals(action))

    c(target_pars, action_pars)
}

#' @importFrom purrr map
rule_options <- function(rules) {
    pars <- setdiff(unique(unlist(map(rules, rule_pars))), "self")

    par_to_option <- function(par) {
        make_option(paste0("--", par),
                    action  = "store")
    }

    c(.global_options, map(pars, par_to_option))
}

default_arg_parser <- function(arg) {
    parsed <- NULL

    tryCatch(
        parsed <- parse(text = arg)[[1]],
        error = function(e) {
            parsed <<- arg
        })

    if (is.symbol(parsed)) as.character(parsed) else eval(parsed)
}

parse_options <- function(options, arg_parser) {

    if (is.null(arg_parser)) {
        arg_parser <- default_arg_parser
    }

    glob_opts <- unlist(map(.global_options,
                            function(opt) c(opt@short_flag, opt@long_flag)))

    rule_opts <- setdiff(names(options), glob_opts)

    for (name in rule_opts) {
        options[[name]] <- arg_parser(options[[name]])
    }

    options
}

#' Command line interface
#'
#' @description
#' `rule_cmd()` constructs a commmand-line interface for the rule tree.
#' This interface enables the rules to be executed with arguments supplied as
#' command-line parameters.
#'
#' @param rule Rule object.
#' @param args Command-line arguments.
#' @param arg_parser Function to parse command-line arguments.
#'
#' @importFrom optparse OptionParser parse_args2
#' @importFrom purrr map
#' @export
rule_cmd <- function(rule,
                     args = commandArgs(trailingOnly = TRUE),
                     arg_parser = NULL) {

    rules <- rule_sort(rule)

    parser <- OptionParser(option_list = rule_options(rules),
                           add_help_option = FALSE)

    params <- NULL
    names  <- NULL
    error  <- NULL

    tryCatch({
        options <- parse_args2(parser,
                               args,
                               print_help_and_exit = FALSE)
        params <- parse_options(options$options, arg_parser)
        names <- options$args
    }, error = function(e) {
        error <<- e$message
    })

    if (!is.null(error)) {
        cat(error, "\n")
        rule_help(rules)
    } else if (params$help) {
        rule_help(rules)
    } else if (!is.null(params$port)) {
        rule_web(rule, as.integer(params$port))
    } else {
        rule_run_all(rules, params, names)
    }

    invisible(NULL)
}


#' @importFrom glue glue
#' @importFrom cli col_br_green cli_inform
dbg_h1 <- function(trace, msg, .envir = parent.frame()) {
    if (trace) {
        dbat <- style_bold("❯")
        msg <- glue(msg, .envir = .envir)
        n <- nchar(msg)
        dashes <- rep.int("─", max(40 - n, 0))
        dashes <- paste0(dashes, collapse = "")
        cli_inform(col_br_green("{dbat}──── {msg} {dashes}"))
    }
}

#' @importFrom glue glue
#' @importFrom cli col_br_red cli_inform
dbg_h2 <- function(trace, msg, .envir = parent.frame()) {
    if (trace) {
        dbat <- style_bold("❯❯")
        msg <- glue(msg, .envir = .envir)
        n <- nchar(msg)
        dashes <- rep.int("─", max(40 - n, 0))
        dashes <- paste0(dashes, collapse = "")
        cli_inform(col_br_red("{dbat}─── {msg} {dashes}"))
    }
}

#' @importFrom glue glue
#' @importFrom cli col_br_blue cli_inform
dbg_h3 <- function(trace, msg, .envir = parent.frame()) {
    if (trace) {
        dbat <- style_bold("❯❯❯")
        msg <- glue(msg, .envir = .envir)
        cli_inform(col_br_blue("{dbat}── {msg}"))
    }
}

#' @importFrom bench bench_time
rule_run_helper <- function(rule, params) {

    ## make sure self is available to target and action
    params$self <- rule

    target <- rule_target(rule)

    target_pars <- names(formals(target))

    target_args <- params[target_pars]

    done <- FALSE

    time <- NULL

    error <- NULL

    name <- style_bold(rule_name(rule))

    dbg_h2(params$trace, "Begin target {name}")

    tryCatch({
        done <- do.call(target, target_args)
        dbg_h3(params$trace, "Target returned {toString(done)}.")
    }, error = function(e) {
        error <<- e
        dbg_h3(params$trace, "Target failed with error: {e$msg}.")
    })

    dbg_h2(params$trace, "End target {name}")

    if (!is.null(error)) {
        return(list(time = NULL, error = error))
    }

    if (done) {
        return(NULL)
    }

    action <- rule_action(rule)

    action_pars <- names(formals(action))

    action_args <- params[action_pars]

    dbg_h2(params$trace, "Begin action {name}")

    time <- bench_time({
        tryCatch({
            do.call(action, action_args)
            dbg_h3(params$trace, "Action executed successfully.")
        }, error = function(e) {
            error <<- e
            dbg_h3(params$trace, "Action failed with error: {e$msg}.")
        })
    })

    dbg_h2(params$trace, "End action {name}")

    list(time = time_to_df(time), error = error)
}

#' @importFrom fs dir_create path
initialize <- function(rule, params) {

    store <- params$store

    if (is_chr(store)) {
        name <- rule_name(rule)

        data_dir <- path(store, name)
        dir_create(data_dir, recurse = TRUE)

        hist_dir <- path(store, ".hist", name)
        dir_create(hist_dir, recurse = TRUE)

        rule_dir_set(rule, data_dir, hist_dir)
    }

    rule_logs_init(rule)

    debug <- params$debug

    if (typeof(debug) == "logical" && debug == TRUE) {
        rule$debug <- TRUE
    } else {
        rule$debug <- FALSE
    }
}

#' @importFrom fs dir_create path
deinitialize <- function(rule) {
    rule_dir_set(rule, NULL, NULL)
    rule_logs_rem(rule)
    rule$debug <- FALSE
}

#' @importFrom fs path
#' @importFrom withr with_dir
#' @importFrom purrr walk map_chr
#' @importFrom cli col_yellow cli_inform
rules_run <- function(rules, params) {

    ## this function implements a topological sort to first get a linear
    ## ordering of rule tree. Then they are all initialized. Even if the
    ## rules' dependencies are not be run, the rules will still use their
    ## data, so initializing them is important. Its also important to note
    ## that initialization cannot be done one rule at a time since when a
    ## rule is run, it will try to access its dependencies' data. So all
    ## rules have to be initialized first before continuing.

    rules_with_deps <- rule_sort(rules)
    ## if we don't ignore deps, then do topo sort and run,
    ## otherwise, just run the rule
    rules <- if (params$ignore_deps) rules else rules_with_deps

    if (params$trace) {
        exec_seq <- paste(map_chr(rules, rule_name), collapse = " ➜ ")
        cli_inform(col_yellow("⬤──── Evaluating rules: {exec_seq}"))
    }

    store <- params$store
    version <- params$version

    ## all rules have to be initialized before running because a rule
    ## should be able to access the data of its dependencies.
    walk(rules_with_deps, initialize, params)

    res <- NULL

    for (r in rules) {

        dbg_h1(params$trace, "Begin rule {style_bold(rule_name(r))}")

        ## make sure that wd is reset even if error happens
        res <- with_dir(getwd(), rule_run_helper(r, params))

        dbg_h1(params$trace, "End rule {style_bold(rule_name(r))}")

        ## is res is null, it means rule was not required to be executed
        if (is.null(res)) {
            next
        }

        if (!is.null(res$time) && is_chr(store)) {
            hist_write(r,
                       version,
                       time = res$time,
                       log = rule_logs_df(r))
        }

        if (!is.null(res$error)) {
            break
        }
    }

    walk(rules_with_deps, deinitialize)

    if (!is.null(res$error)) {
        stop(res$error)
    }

    invisible(NULL)
}

#' TODO
#'
#' @param rule Rule.
#' @param params Parameters.
#'
#' @export
rule_run <- function(rule, params) {
    rules_run(list(rule), params)
}

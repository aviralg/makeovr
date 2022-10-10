
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

    tryCatch(done <- do.call(target, target_args),
             error = function(e) {
                 error <<- e
             })

    if (!is.null(error)) {
        list(time = time, error = error)
    }

    if (done) {
        return(NULL)
    }

    action <- rule_action(rule)

    action_pars <- names(formals(action))

    action_args <- params[action_pars]

    time <- bench_time({
        tryCatch(do.call(action, action_args),
                 error = function(e) {
                     error <<- e
                 })
    })

    list(time = time_to_df(time), error = error)
}

#' @importFrom fs dir_create path
initialize <- function(rule, store, version) {

    if (is_chr(store)) {
        name <- rule_name(rule)

        data_dir <- path(store, name)
        dir_create(data_dir, recurse = TRUE)

        hist_dir <- path(store, ".hist", name)
        dir_create(hist_dir, recurse = TRUE)

        rule_dir_set(rule, data_dir, hist_dir)
    }

    rule_logs_init(rule)
}

#' @importFrom fs dir_create path
deinitialize <- function(rule) {
    rule_dir_set(rule, NULL, NULL)
    rule_logs_rem(rule)
}

#' TODO
#'
#' @param rule Rule.
#' @param params Parameters.
#'
#' @export
#' @importFrom fs path
#' @importFrom withr with_dir
#' @importFrom purrr walk
rule_run <- function(rule, params) {

    print(params)

    ## if we don't ignore deps, then do topo sort and run,
    ## otherwise, just run the rule
    rules <- if (params$ignore_deps) list(rule) else rule_sort(rule)

    store <- params$store
    version <- params$version

    walk(rules, initialize, store, version)

    res <- NULL

    for (r in rules) {
        ## make sure that wd is reset even if error happens
        res <- with_dir(getwd(), rule_run_helper(r, params))

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

    walk(rules, deinitialize)

    if (!is.null(res$error)) {
        stop(res$error)
    }

    invisible(NULL)
}

#' @importFrom purrr detect
rule_run_all <- function(rules, params, names) {

    for (name in names) {

        r <- detect(rules, function(r) rule_name(r) == name)

        if (is.null(r)) {
            stop(sprintf("invalid rule name '%s'", name))
        } else {
            rule_run(r, params)
        }
    }

    invisible(NULL)
}

.state <- new.env(parent = emptyenv())
.state$id <- -1

get_id <- function() {
    .state$id <- .state$id + 1
    .state$id
}

#' Make a new rule.
#'
#' @param name   Rule name.
#' @param desc   Rule description.
#' @param target Rule target function.
#' @param action Rule action function.
#' @param ...    Rule dependencies.
#'
#' @examples
#' rule("slicer", "Slice a df", function(self) FALSE, function(self, df, len = 10) df[1:len, ])
#'
#' @export
#' @importFrom purrr map_chr
rule <- function(name, desc, target, action, ...) {

    validate_name(name)

    deps <- list(...)

    names(deps) <- map_chr(deps, rule_name)

    obj <- new.env(parent = emptyenv())

    obj$id        <- get_id()
    obj$name      <- name
    obj$desc      <- desc
    obj$target    <- target
    obj$action    <- action
    obj$deps      <- deps
    obj$data_dir  <- NULL
    obj$hist_dir  <- NULL
    obj$logs      <- NULL

    class(obj) <- "rule"

    obj
}

rule_id <- function(rule) {
    rule$id
}

#' Query Rule objects
#'
#' @description
#'
#' These functions can be used to query the fields of rule objects.
#'
#' * `rule_name()` returns the rule name.
#' * `rule_desc()` returns the rule description.
#' * `rule_deps()` returns a named list of rule's dependencies.
#' * `rule_dep()` returns rule's dependency with the given name.
#' * `rule_target()` returns the rule's target function.
#' * `rule_action()` returns the rule's action function.
#'
#' @param rule The rule object.
#' @param name Rule's dependency name.
#'
#' @examples
#' q <- rule("q", "Desc Q", function(self) FALSE, function(self) print("q"))
#' r <- rule("r", "Desc R", function(self) FALSE, function(self) print("r"), q)
#' rule_name(r)
#' rule_desc(r)
#' rule_deps(r)
#' rule_dep(r, "q")
#' rule_target(r)
#' rule_action(r)
#' @export
#' @rdname queryrule
rule_name <- function(rule) {
    rule$name
}

#' @export
#' @rdname queryrule
rule_desc <- function(rule) {
    rule$desc
}

#' @export
#' @rdname queryrule
rule_deps <- function(rule) {
    rule$deps
}

#' @export
#' @rdname queryrule
rule_dep <- function(rule, name) {
    deps <- rule_deps(rule)
    deps[[name]]
}

#' @export
#' @rdname queryrule
rule_target <- function(rule) {
    rule$target
}

#' @export
#' @rdname queryrule
rule_action <- function(rule) {
    rule$action
}

rule_dir_set <- function(rule, data_dir, hist_dir) {
    rule$data_dir <- data_dir
    rule$hist_dir <- hist_dir
}

rule_data_dir <- function(rule) {
    dir <- rule$data_dir
    if(is.null(dir)) {
        name <- rule_name(rule)
        stop(sprintf("no data directory associated with rule %s", name))
    }
    dir
}

rule_hist_dir <- function(rule) {
    dir <- rule$hist_dir
    if(is.null(dir)) {
        name <- rule_name(rule)
        stop(sprintf("no history directory associated with rule %s", name))
    }
    dir
}

rule_logs_init <- function(rule) {
    logs <- new.env(parent = emptyenv())

    logs$ind <- 1
    cap <- 10
    logs$cap <- cap

    logs$types <- character(cap)
    logs$times <- character(cap)
    logs$msgs <- character(cap)

    rule$logs <- logs
}

rule_logs_rem <- function(rule) {
    rule$logs <- NULL
}

rule_logs_add <- function(rule, type, time, msg) {
    logs <- rule$logs

    ind <- logs$ind
    cap <- logs$cap

    if(ind > cap) {
        logs$types <- c(logs$types, character(cap))
        logs$times <- c(logs$times, character(cap))
        logs$msgs <- c(logs$msgs, character(cap))
        logs$cap <- 2 * cap
    }

    logs$types[ind] <- type
    logs$times[ind] <- time
    logs$msgs[ind] <- msg

    logs$ind <- ind + 1
}

rule_logs_df <- function(rule) {
    logs <- rule$logs

    ind <- logs$ind - 1
    indices <- seq_len(ind)

    data.frame(type = logs$types[indices],
               time = logs$times[indices],
               msg  = logs$msgs[indices])
}

## topological sort
rule_sort <- function(rule) {
    temp_mark <- character(0)

    perm_mark <- character(0)

    order <- list()

    visit <- function(r) {
        name <- rule_name(r)

        if(name %in% perm_mark) {
            return(NULL)
        }

        if(name %in% temp_mark) {
            stop(sprintf("rules should not have a cycle"))
        }

        temp_mark <<- c(temp_mark, name)

        for(d in rule_deps(r)) {
            visit(d)
        }

        temp_mark <<- temp_mark[1:length(temp_mark) - 1]

        perm_mark <<- c(perm_mark, name)

        order <<- c(order, list(r))
    }

    visit(rule)

    order
}

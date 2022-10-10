
#' @export
print.rule <- function(x, ...) {
    print(rule_to_expr(x))
    invisible(x)
}

## #' TODO
## #' @export
## #' @importFrom cli cli_code
## rule_show <- function(rule) {
##     cli_code(format(rule_to_expr(rule)))
##     invisible(rule)
## }

rule_to_expr <- function(rule) {


    name <- rule_name(rule)
    desc <- rule_desc(rule)
    target <- rule_target(rule)
    action <- rule_action(rule)

    expr <- substitute(rule(NAME, DESC, TARGET, ACTION),
                       list(NAME = name, DESC = desc,
                            TARGET = target, ACTION = action))


    deps <- rule_deps(rule)

    for(d in deps) {
        name <- rule_name(d)
        expr[[length(expr) + 1]] <- as.name(name)
    }

    expr
}

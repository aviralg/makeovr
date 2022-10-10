
#' @export
#' @importFrom cli cli_code
print.rule <- function(x, ...) {
    expr <- rule_to_expr(x)
    cli_code(format(expr))
    invisible(x)
}

rule_to_expr <- function(rule) {
    name <- rule_name(rule)
    desc <- rule_desc(rule)
    target <- rule_target(rule)
    action <- rule_action(rule)

    expr <- substitute(rule(NAME, DESC, TARGET, ACTION),
                       list(NAME = name, DESC = desc,
                            TARGET = target, ACTION = action))


    deps <- rule_deps(rule)

    for (d in deps) {
        name <- rule_name(d)
        expr[[length(expr) + 1]] <- as.name(name)
    }

    expr
}


par_dflt <- function(par_name, pars) {

    dflt <- ""

    if (par_name %in% names(pars)) {
        dflt <- pars[[par_name]]
        dflt <- expr_to_chr(dflt)
    }

    dflt
}

#' @importFrom stringr str_ends fixed
par_desc <- function(name, desc) {
    desc <- desc[[name]]
    desc <- if (is.null(desc)) ""
            else if (str_ends(desc, fixed("."))) desc
            else paste0(desc, ".")
    desc
}

#' @importFrom stringr str_replace_all fixed
par_name <- function(name) {
    name2 <- str_replace_all(name, fixed("_"), "-")
    if (name2 == name) {
        paste0("--", name)
    } else {
        paste0("--", name, "/", "--", name2)
    }
}

#' @importFrom purrr map_chr
rule_par_tbl <- function(rule) {

    desc <- rule_desc(rule)
    target_pars <- formals(rule_target(rule))
    action_pars <- formals(rule_action(rule))

    par_names <- c(names(target_pars), names(action_pars))
    par_names <- setdiff(unique(par_names), "self")

    target_dflts <- map_chr(par_names, par_dflt, target_pars)
    action_dflts <- map_chr(par_names, par_dflt, action_pars)
    descs <- map_chr(par_names, par_desc, desc)
    names <- map_chr(par_names, par_name)

    res <- data.frame(name = names,
                      target_dflt = target_dflts,
                      action_dflt = action_dflts,
                      desc = descs)
    res
}

global_par_tbl <- function() {

    op_name <- function(op) {
        short <- op@short_flag
        long  <- op@long_flag

        if (is.na(short)) long
        else if (is.na(long)) short
        else paste0(short, "/", long)
    }

    names <- map_chr(.global_options, op_name)
    dflts <- map_chr(.global_options, function(op) expr_to_chr(op@default))
    descs <- map_chr(.global_options, function(op) op@help)

    res <- data.frame(name = names,
                      target_dflt = dflts,
                      action_dflt = character(length(names)),
                      desc = descs)
    res
}

#' @importFrom cli col_cyan col_red col_green
colorize_par <- function(name, target_dflt, action_dflt, desc) {
    n <- nchar(name)
    val <- ""

    if (target_dflt != "" && action_dflt == "") {
        val <- sprintf("=%s", col_cyan(target_dflt))
        n <- n + 1 + nchar(target_dflt)
    } else if (target_dflt == "" && action_dflt != "") {
        val <- sprintf("=%s", col_cyan(action_dflt))
        n <- n + 1 + nchar(action_dflt)
    } else if (target_dflt != "" && action_dflt != "") {
        val <- sprintf("=%s/%s", col_cyan(target_dflt), col_cyan(action_dflt))
        n <- n + 1 + nchar(target_dflt) + 1 + nchar(action_dflt)
    }

    name_dflt <- sprintf("%s%s", col_red(name), val)

    desc <- col_green(desc)

    data.frame(name_dflt = name_dflt, desc = desc, n = n)
}

#' @importFrom purrr pwalk pmap_dfr
#' @importFrom cli cli_h2 style_bold ansi_strwrap
#' @importFrom cli col_blue col_yellow
#' @importFrom stringr str_pad
show_help <- function(name, desc, pars) {
    cat("\n")

    cli_h2(col_blue(style_bold("[{name}]")))

    if (!is.null(desc)) {
        desc <- ansi_strwrap(col_yellow(style_bold(desc)),
                             indent = 4,
                             exdent = 4,
                             simplify = TRUE)
        cat(desc, sep = "\n")
        cat("\n")
    }

    if (nrow(pars) > 0) {
        pars <- pmap_dfr(pars, colorize_par)

        max_n <- max(pars$n)

        pwalk(pars,
              function(name_dflt, desc, n) {
                  spaces <- paste(rep.int(" ", 2 + max_n - n), collapse = "")
                  cat("    ", name_dflt, spaces, desc, "\n", sep = "")
              })
    }
}

show_rule_help <- function(rule) {
    name <- rule_name(rule)
    desc <- rule_desc(rule)

    pars <- rule_par_tbl(rule)

    show_help(name, desc[[1]], pars)
}

#' @importFrom purrr walk map_chr
rule_help <- function(rules, file = "./file") {

    names <- paste0(map_chr(rules, rule_name), collapse = ",")

    usage <- sprintf("./<script> [global options] {%s} [local options]", names)
    cat("Usage:", usage, "\n")

    show_help("<global>", NULL, global_par_tbl())
    walk(rules, show_rule_help)

    cat("\n")

    invisible(NULL)
}


par_dflt <- function(par_name, target_pars, action_pars) {

    helper <- function(name, pars) {
        dflt <- MISSING

        if(name %in% names(pars)) {
            dflt <- pars[[name]]
            dflt <- expr_to_chr(dflt)
            dflt <- if(dflt == "") MISSING else dflt
        }

        dflt
    }

    target_dflt <- helper(par_name, target_pars)
    action_dflt <- helper(par_name, action_pars)

    res <- if(is_missing(target_dflt) && is_missing(action_dflt)) ""
           else if(is_missing(target_dflt)) action_dflt
           else if(is_missing(action_dflt)) target_dflt
           else paste(target_dflt, "/", action_dflt, sep = "")

    res
}

#' @importFrom stringr str_ends fixed
par_desc <- function(name, desc) {
    desc <- desc[[name]]
    desc <- if(is.null(desc)) ""
            else if(str_ends(desc, fixed("."))) desc
            else paste0(desc, ".")
    desc
}

#' @importFrom stringr str_replace_all fixed
par_name <- function(name) {
    name2 <- str_replace_all(name, fixed("_"), "-")
    if(name2 == name) {
        paste0("--", name)
    }
    else {
        paste0("--", name, "/", "--", name2)
    }
}

#' @importFrom purrr map2_chr
par_tbl <- function(names, dflts, descs) {
    par_dflt <- map2_chr(names,
                         dflts,
                         function(name, dflt) {
                             if (dflt == "") name else sprintf("%s=%s", name, dflt)
                         })

    res <- data.frame(par_dflt = par_dflt,
                      desc = descs)
    res
}

#' @importFrom purrr map_chr
rule_par_tbl <- function(rule) {

    name <- rule_name(rule)
    desc <- rule_desc(rule)
    target_pars <- formals(rule_target(rule))
    action_pars <- formals(rule_action(rule))

    par_names <- setdiff(unique(c(names(target_pars), names(action_pars))), "self")

    dflts <- map_chr(par_names, par_dflt, target_pars, action_pars)
    descs <- map_chr(par_names, par_desc, desc)
    names <- map_chr(par_names, par_name)

    par_tbl(names, dflts, descs)
}

global_par_tbl <- function() {

    op_name <- function(op) {
        short <- op@short_flag
        long  <- op@long_flag

        if(is.na(short)) long
        else if(is.na(long)) short
        else paste0(short, "/", long)
    }

    names <- map_chr(GLOBAL_OPTIONS, op_name)
    dflts <- map_chr(GLOBAL_OPTIONS, function(op) expr_to_chr(op@default))
    descs <- map_chr(GLOBAL_OPTIONS, function(op) op@help)

    par_tbl(names, dflts, descs)
}


#' @importFrom purrr walk2
show_help <- function(name, desc, pars) {
    cat("\n")
    cat(sprintf("    [%s]", name), "\n")

    if(!is.null(desc)) {
        cat(paste("    ", desc, collapse = "\n", sep=""))
        cat("\n")
    }

    if(nrow(pars) > 0) {
        cat("\n")
        walk2(pad_max(pars$par_dflt),
              pad_max(pars$desc),
              function(par_dflt, desc) {
                  line <- sprintf("%s  %s", par_dflt, desc)
                  cat(paste("    ", line, "\n", sep = ""))
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

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log.R
\name{log}
\alias{log}
\alias{rule_log}
\alias{rule_msg}
\alias{rule_dbg}
\alias{rule_err}
\alias{rule_warn}
\title{Logging inside rules}
\usage{
rule_log(rule, type, msg)

rule_msg(rule, message, ..., .envir = parent.frame())

rule_dbg(rule, message, ..., .envir = parent.frame())

rule_err(
  rule,
  message,
  ...,
  call = .envir,
  .envir = parent.frame(),
  .frame = .envir
)

rule_warn(rule, message, ..., .envir = parent.frame())
}
\arguments{
\item{rule}{The rule}

\item{type}{Log type}

\item{...}{values to be passed into ‘fmt’.  Only logical, integer, real and
character vectors are supported, but some coercion will be done.}

\item{fmt}{A character vector of format strings.}
}
\description{
These functions can be used for logging during rule execution.
The logs are printed on the console and retained as part of the rule's
execution history.
}

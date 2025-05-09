% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_multi_dynamic.R
\name{get_multi_dyna_residuals}
\alias{get_multi_dyna_residuals}
\title{Residuals of multiple dynamic predictions}
\usage{
get_multi_dyna_residuals(
  this_p,
  experiment_data,
  known_pars,
  sec_model_names,
  logbase_mu = logbase_logN,
  logbase_logN = 10
)
}
\arguments{
\item{this_p}{named vector of model parameters}

\item{experiment_data}{a nested list with the experimental data. Each entry describes
one experiment as a list with two elements: data and conditions. \code{data} is a tibble
with two columns: time and logN. \code{conditions} is a tibble with one column named time
and as many additional columns as environmental factors.}

\item{known_pars}{named vector of known model parameters}

\item{sec_model_names}{named character vector with names of the
environmental conditions and values of the secondary model (see secondary_model_data).}

\item{logbase_mu}{Base of the logarithm the growth rate is referred to.
By default, the same as logbase_logN. See vignette about units for details.}

\item{logbase_logN}{Base of the logarithm for the population size. By default,
10 (i.e. log10). See vignette about units for details.}
}
\value{
an instance of \code{modCost}.
}
\description{
Function for calculating residuals of dynamic predictions under
different conditions for the same model parameters according
to the requirements of \code{\link[FME:modFit]{FME::modFit()}}.
}

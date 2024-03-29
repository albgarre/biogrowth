% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_stochastic_growth.R
\name{predict_stochastic_growth}
\alias{predict_stochastic_growth}
\title{Deprecated isothermal growth with parameter uncertainty}
\usage{
predict_stochastic_growth(
  model_name,
  times,
  n_sims,
  pars,
  corr_matrix = diag(nrow(pars)),
  check = TRUE
)
}
\arguments{
\item{model_name}{Character describing the primary growth model.}

\item{times}{Numeric vector of storage times for the simulations.}

\item{n_sims}{Number of simulations.}

\item{pars}{A tibble describing the parameter uncertainty (see details).}

\item{corr_matrix}{Correlation matrix of the model parameters. Defined in the
same order as in \code{pars}. An identity matrix by default
(uncorrelated parameters).}

\item{check}{Whether to do some tests. \code{FALSE} by default.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

\code{\link[=predict_stochastic_growth]{predict_stochastic_growth()}} was renamed \code{\link[=predict_growth_uncertainty]{predict_growth_uncertainty()}} because
the original function name may be misleading, as this is not a stochastic
differential equation
}

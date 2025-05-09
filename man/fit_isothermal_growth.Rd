% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_iso_growth.R
\name{fit_isothermal_growth}
\alias{fit_isothermal_growth}
\title{Fit primary growth models}
\usage{
fit_isothermal_growth(
  fit_data,
  model_name,
  starting_point,
  known_pars,
  ...,
  check = TRUE,
  formula = logN ~ time,
  logbase_mu = logbase_logN,
  logbase_logN = 10
)
}
\arguments{
\item{fit_data}{Tibble of data for the fit. It must have two columns, one with
the elapsed time (\code{time} by default) and another one with the decimal logarithm
of the populatoin size (\code{logN} by default). Different column names can be
defined using the \code{formula} argument.}

\item{model_name}{Character defining the primary growth model}

\item{starting_point}{Named vector of initial values for the model parameters.}

\item{known_pars}{Named vector of known model parameters (not fitted).}

\item{...}{Additional arguments passed to \code{\link[FME:modFit]{FME::modFit()}}.}

\item{check}{Whether to do some basic checks (TRUE by default).}

\item{formula}{an object of class "formula" describing the x and y variables.
\code{logN ~ time} as a default.}

\item{logbase_mu}{Base of the logarithm the growth rate is referred to.
By default, the same as logbase_logN. See vignette about units for details.}

\item{logbase_logN}{Base of the logarithm for the population size. By default,
10 (i.e. log10). See vignette about units for details.}
}
\value{
An instance of \code{\link[=FitIsoGrowth]{FitIsoGrowth()}}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}

The function \code{\link[=fit_isothermal_growth]{fit_isothermal_growth()}} has been superseded by the top-level
function \code{\link[=fit_growth]{fit_growth()}}, which provides a unified approach for growth modelling.

Nonetheless, it can still fit a primary growth model to data obtained under static
environmental conditions.
}
\examples{

## Some dummy data

library(tibble)

my_data <- tibble(time = c(0, 25, 50, 75, 100),
    logN = c(2, 2.5, 7, 8, 8))

## Choose the model

my_model <- "Baranyi"

## Initial values for the model parameters

start = c(logNmax = 8, lambda = 25, logN0 = 2)

## Any model parameter can be fixed

known <- c(mu = .2)

## Now, we can call the function

static_fit <- fit_isothermal_growth(my_data, my_model, start, known)

summary(static_fit)

## We can plot the fitted model against the observations

plot(static_fit)

}

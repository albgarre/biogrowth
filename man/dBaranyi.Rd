% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baranyi_model.R
\name{dBaranyi}
\alias{dBaranyi}
\title{Baranyi growth model}
\usage{
dBaranyi(time, state, pars, env_func, sec_models)
}
\arguments{
\item{time}{numeric vector (length 1) of storage time}

\item{state}{named numeric vector with two components: Q and N}

\item{pars}{named numeric vector of model parameters (Nmax and mu_opt)}

\item{env_func}{named list of functions returning the values of
the environmental conditions for time (t)}

\item{sec_models}{named list of parameters of the secondary model}
}
\value{
A numeric vector of two components according to the requirements of
\code{\link[deSolve:ode]{deSolve::ode()}}.
}
\description{
Microbial growth model as defined in Baranyi and Roberts (1994). It has
been implemented according to the requirements of
\code{\link[deSolve:ode]{deSolve::ode()}}. For consistency in the function for isothermal growth,
calculations are done assuming the user input for mu is in log10 scale. In other words,
the input is multiplied by ln(10).
}

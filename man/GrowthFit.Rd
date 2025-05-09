% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GrowthFit_class.R, R/predictMCMC.R
\name{GrowthFit}
\alias{GrowthFit}
\alias{print.GrowthFit}
\alias{coef.GrowthFit}
\alias{summary.GrowthFit}
\alias{predict.GrowthFit}
\alias{residuals.GrowthFit}
\alias{vcov.GrowthFit}
\alias{deviance.GrowthFit}
\alias{fitted.GrowthFit}
\alias{logLik.GrowthFit}
\alias{AIC.GrowthFit}
\alias{plot.GrowthFit}
\alias{predictMCMC.GrowthFit}
\title{GrowthFit class}
\usage{
\method{print}{GrowthFit}(x, ...)

\method{coef}{GrowthFit}(object, ...)

\method{summary}{GrowthFit}(object, ...)

\method{predict}{GrowthFit}(object, times = NULL, env_conditions = NULL, ...)

\method{residuals}{GrowthFit}(object, ...)

\method{vcov}{GrowthFit}(object, ...)

\method{deviance}{GrowthFit}(object, ...)

\method{fitted}{GrowthFit}(object, ...)

\method{logLik}{GrowthFit}(object, ...)

\method{AIC}{GrowthFit}(object, ..., k = 2)

\method{plot}{GrowthFit}(
  x,
  y = NULL,
  ...,
  add_factor = NULL,
  line_col = "black",
  line_size = 1,
  line_type = 1,
  point_col = "black",
  point_size = 3,
  point_shape = 16,
  ylims = NULL,
  label_y1 = NULL,
  label_y2 = add_factor,
  label_x = "time",
  line_col2 = "black",
  line_size2 = 1,
  line_type2 = "dashed"
)

\method{predictMCMC}{GrowthFit}(
  model,
  times,
  env_conditions,
  niter,
  newpars = NULL,
  formula = . ~ time
)
}
\arguments{
\item{x}{The object of class \link{GrowthFit} to plot.}

\item{...}{ignored.}

\item{object}{an instance of GrowthFit}

\item{times}{Numeric vector of storage times for the predictions.}

\item{env_conditions}{Tibble with the (dynamic) environmental conditions
during the experiment. It must have one column named 'time' with the
storage time and as many columns as required with the environmental conditions.}

\item{k}{penalty for the parameters (k=2 by default)}

\item{y}{ignored}

\item{add_factor}{whether to plot also one environmental factor.
If \code{NULL} (default), no environmental factor is plotted. If set
to one character string that matches one entry of x$env_conditions,
that condition is plotted in the secondary axis. Ignored if \code{environment="constant"}}

\item{line_col}{Aesthetic parameter to change the colour of the line geom in the plot, see: \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}}

\item{line_size}{Aesthetic parameter to change the thickness of the line geom in the plot, see: \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}}

\item{line_type}{Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}}

\item{point_col}{Aesthetic parameter to change the colour of the point geom, see: \code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}}

\item{point_size}{Aesthetic parameter to change the size of the point geom, see: \code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}}

\item{point_shape}{Aesthetic parameter to change the shape of the point geom, see: \code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}}

\item{ylims}{A two dimensional vector with the limits of the primary y-axis.
\code{NULL} by default}

\item{label_y1}{Label of the primary y-axis.}

\item{label_y2}{Label of the secondary y-axis. Ignored if \code{environment="constant"}}

\item{label_x}{Label of the x-axis}

\item{line_col2}{Same as lin_col, but for the environmental factor. Ignored if \code{environment="constant"}}

\item{line_size2}{Same as line_size, but for the environmental factor. Ignored if \code{environment="constant"}}

\item{line_type2}{Same as lin_type, but for the environmental factor. Ignored if \code{environment="constant"}}

\item{model}{An instance of \link{GrowthFit}}

\item{niter}{Number of iterations.}

\item{newpars}{A named list defining new values for the some model parameters.
The name must be the identifier of a model already included in the model.
These parameters do not include variation, so defining a new value for a fitted
parameters "fixes" it. \code{NULL} by default (no new parameters).}

\item{formula}{A formula stating the column named defining the elapsed time in
\code{env_conditions}. By default, . ~ time.}
}
\value{
An instance of \link{MCMCgrowth}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

The \code{GrowthFit} class contains a growth model fitted to data under
static or dynamic conditions. Its constructor is \code{\link[=fit_growth]{fit_growth()}}.

It is a subclass of list with the items:
\itemize{
\item environment: type of environment as in \code{\link[=fit_growth]{fit_growth()}}
\item algorithm: type of algorithm as in \code{\link[=fit_growth]{fit_growth()}}
\item data: data used for model fitting
\item start: initial guess of the model parameters
\item known: fixed model parameters
\item primary_model: a character describing the primary model
\item fit_results: an instance of modFit or modMCMC with the results of the fit
\item best_prediction: Instance of \link{GrowthPrediction} with the best growth fit
\item sec_models: a named vector with the secondary models assigned for each
environmental factor. \code{NULL} for \code{environment="constant"}
\item env_conditions: a tibble with the environmental conditions used for model
fitting. \code{NULL} for \code{environment="constant"}
\item niter: number of iterations of the Markov chain. \code{NULL} if \code{algorithm != "MCMC"}
\item logbase_mu: base of the logarithm for the definition of parameter mu
(check the relevant vignette)
\item logbase_logN: base of the logarithm for the definition of the population size
(check the relevant vignette)
}
}
\section{Methods (by generic)}{
\itemize{
\item \code{print(GrowthFit)}: print of the model

\item \code{coef(GrowthFit)}: vector of fitted model parameters.

\item \code{summary(GrowthFit)}: statistical summary of the fit.

\item \code{predict(GrowthFit)}: vector of model predictions.

\item \code{residuals(GrowthFit)}: vector of model residuals.

\item \code{vcov(GrowthFit)}: variance-covariance matrix of the model, estimated
as 1/(0.5*Hessian) for regression and as the variance-covariance of the draws
for MCMC

\item \code{deviance(GrowthFit)}: deviance of the model.

\item \code{fitted(GrowthFit)}: vector of fitted values.

\item \code{logLik(GrowthFit)}: loglikelihood of the model

\item \code{AIC(GrowthFit)}: Akaike Information Criterion

\item \code{plot(GrowthFit)}: compares the fitted model against the data.

\item \code{predictMCMC(GrowthFit)}: prediction including parameter uncertainty

}}

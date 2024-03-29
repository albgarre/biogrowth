% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_counts.R
\name{time_to_size}
\alias{time_to_size}
\title{Time for the population to reach a given size}
\usage{
time_to_size(model, size, type = "discrete", logbase_logN = NULL)
}
\arguments{
\item{model}{An instance of \link{GrowthPrediction}, \link{GrowthFit}, \link{GlobalGrowthFit},
\link{GrowthUncertainty} or \link{MCMCgrowth}.}

\item{size}{Target population size (in log scale)}

\item{type}{Tye of calculation, either "discrete" (default) or "distribution"}

\item{logbase_logN}{Base of the logarithm for the population size. By default,
10 (i.e. log10). See vignette about units for details.}
}
\value{
If  \code{type="discrete"}, a number. If \code{type="distribution"}, an instance of
\link{TimeDistribution}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Calculates the elapsed time required for the population to reach a given size
(in log scale)
}
\details{
The calculation method differs depending on the value of \code{type}. If \code{type="discrete"}
(default), the function calculates by linear interpolation a discrete time to
reach the target population size. If \code{type="distribution"}, this calculation
is repeated several times, generating a distribution of the time. Note that this
is only possible for instances of \link{GrowthUncertainty} or \link{MCMCgrowth}.
}
\examples{

## Example 1 - Growth predictions -------------------------------------------

## The model is defined as usual with predict_growth

my_model <- list(model = "modGompertz", logN0 = 0, C = 6, mu = .2, lambda = 20)

my_time <- seq(0, 100, length = 1000)  # Vector of time points for the calculations

my_prediction <- predict_growth(my_time, my_model, environment = "constant")

plot(my_prediction)

## We just have to pass the model and the size (in log10)

time_to_size(my_prediction, 3)

## If the size is not reached, it returns NA

time_to_size(my_prediction, 8)

## By default, it considers the population size is defined in the same log-base
## as the prediction. But that can be changed using logbase_logN

time_to_size(my_prediction, 3)
time_to_size(my_prediction, 3, logbase_logN = 10)
time_to_size(my_prediction, log(100), logbase_logN = exp(1))

## Example 2 - Model fit ----------------------------------------------------

my_data <- data.frame(time = c(0, 25, 50, 75, 100), 
                      logN = c(2, 2.5, 7, 8, 8))
                      
models <- list(primary = "Baranyi")

known <- c(mu = .2)

start <- c(logNmax = 8, lambda = 25, logN0 = 2)

primary_fit <- fit_growth(my_data, models, start, known,
                          environment = "constant",
                          )
                          
plot(primary_fit)

time_to_size(primary_fit, 4)

## Example 3 - Global fitting -----------------------------------------------

## We need a model first

data("multiple_counts")
data("multiple_conditions")

sec_models <- list(temperature = "CPM", pH = "CPM")

known_pars <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
                   temperature_n = 2, temperature_xmin = 20, 
                   temperature_xmax = 35,
                   temperature_xopt = 30,
                   pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5)
                   
my_start <- list(mu_opt = .8)

global_fit <- fit_growth(multiple_counts, 
                         sec_models, 
                         my_start, 
                         known_pars,
                         environment = "dynamic",
                         algorithm = "regression",
                         approach = "global",
                         env_conditions = multiple_conditions
                         ) 
                         
plot(global_fit)

## The function calculates the time for each experiment

time_to_size(global_fit, 3)

## It returns NA for the particular experiment if the size is not reached

time_to_size(global_fit, 4.5)

}

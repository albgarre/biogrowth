

#' Residuals of multiple dynamic predictions
#'
#' Function for calculating residuals of dynamic predictions under
#' different conditions for the same model parameters according
#' to the requirements of \code{\link{modFit}}.
#'
#' @param this_p named vector of model parameters
#' @param known_pars named vector of known model parameters
#' @param sec_model_names named character vector with names the
#' environmental conditions and values the secondary model (see secondary_model_data).
#' @param experiment_data a nested list with the experimental data. Each entry describes
#' one experiment as a list with two elements: data and conditions. \code{data} is a tibble
#' with two columns: time and logN. \code{conditions} is a tibble with one column named time
#' and as many additional columns as environmental factors.
#'
#' @return an instance of \code{modCost}.
#'
#' @importFrom FME modCost
#'
get_multi_dyna_residuals <- function(this_p, experiment_data,
                                     known_pars, sec_model_names) {

    old_cost <- NULL

    for (each_experiment in experiment_data) {

            my_cost <- get_dyna_residuals(unlist(this_p), each_experiment$data,
                                          each_experiment$conditions,
                                          unlist(known_pars), sec_model_names,
                                          cost = old_cost)

            old_cost <- my_cost

    }

    my_cost

}

#' Fitting growth models to multiple dynamic experiments
#'
#' This functions enables to fit a growth model using a dataset comprised of
#' several experiments with potentially different dynamic experimental conditions.
#' Note that the definition of secondary models must comply with the
#' `secondary_model_data` function.
#'
#' @inheritParams get_multi_dyna_residuals
#' @param starting_point a named vector of starting values for the model parameters.
#' @param ... additional arguments for \code{modFit}.
#'
#' @importFrom FME modFit
#'
#' @return A list of calss \code{FitMultipleDynamicGrowth} with:
#'
#'      \itemize{
#'          \item fit_results: the object returned by \code{modFit}.
#'          \item best_prediction: a list with the models predictions for each condition.
#'          \item data: a list with the data used for the fit.
#'          \item starting: starting values for model fitting
#'          \item known: parameter values set as known.
#'          \item sec_models: a named vector with the secondary model
#'          for each environmental factor.
#'          }
#'
#' @export
#'
#' @examples
#' ## We will use the multiple_experiments data set
#'
#' data("multiple_experiments")
#'
#' ## For each environmental factor, we need to defined a model
#'
#' sec_names <- c(temperature = "CPM", pH = "CPM")
#'
#' ## Any model parameter can be fixed
#'
#' known <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
#'     temperature_n = 2, temperature_xmin = 20, temperature_xmax = 35,
#'     pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5)
#'
#' ## The rest require starting values for model fitting
#'
#' start <- list(mu_opt = .8, temperature_xopt = 30)
#'
#' ## We can now call the fitting function
#'
#' global_fit <- fit_multiple_growth(start, multiple_experiments, known, sec_names)
#'
#' ## Parameter estimates can be retrieved with summary
#'
#' summary(global_fit)
#'
#' ## We can compare fitted model against observations
#'
#' plot(global_fit)
#'
#' ## Any single environmental factor can be added to the plot using add_factor
#'
#' plot(global_fit, add_factor = "temperature")
#'
#'
fit_multiple_growth <- function(starting_point, experiment_data,
                                known_pars, sec_model_names,
                                ...) {

    ## Fit the model

    my_fit <- modFit(get_multi_dyna_residuals, unlist(starting_point),
                     experiment_data = experiment_data,
                     known_pars = unlist(known_pars),
                     sec_model_names = sec_model_names,
                     ...)

    #- Output the results

    pars_fit <- my_fit$par

    primary_pars <- extract_primary_pars(pars_fit, known_pars)

    secondary_models <- extract_secondary_pars(pars_fit, known_pars,
                                               sec_model_names)

    best_predictions <- lapply(experiment_data, function(each_experiment) {

        times <- seq(0, max(each_experiment$data$time), length=100)

        best_prediction <- predict_dynamic_growth(times, each_experiment$conditions,
                                                  as.list(primary_pars),
                                                  secondary_models)

    })

    names(best_predictions) <- names(experiment_data)

    out <- list(fit_results = my_fit,
                best_prediction = best_predictions,
                data = experiment_data,
                starting = starting_point,
                known = known_pars,
                sec_models = sec_model_names
    )

    class(out) <- c("FitMultipleDynamicGrowth", class(out))
    return(out)

}


#' Fitting growth models to multiple dynamic experiments using MCMC
#'
#' This functions enables to fit a growth model using a dataset comprised of
#' several experiments with potentially different dynamic experimental conditions.
#'
#' @inheritParams get_multi_dyna_residuals
#' @param ... additional arguments for \code{modMCMC} (e.g. upper and lower bounds).
#' @param niter number of samples of the MCMC algorithm.
#'
#' @return A list of calss \code{FitMultipleDynamicGrowthMCMC} with:
#'
#'      \itemize{
#'          \item fit_results: the object returned by \code{modFit}.
#'          \item best_prediction: a list with the models predictions for each condition.
#'          \item data: a list with the data used for the fit.
#'          \item starting: starting values for model fitting
#'          \item known: parameter values set as known.
#'          \item sec_models: a named vector with the secondary model
#'          for each environmental factor.
#'          }
#'
#' @export
#'
#' @examples
#' \donttest{
#' ## We will use the multiple_experiments data set
#'
#' data("multiple_experiments")
#'
#' ## For each environmental factor, we need to defined a model
#'
#' sec_names <- c(temperature = "CPM", pH = "CPM")
#'
#' ## Any model parameter can be fixed
#'
#' known <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
#'     temperature_n = 2, temperature_xmin = 20, temperature_xmax = 35,
#'     pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5)
#'
#' ## The rest require starting values for model fitting
#'
#' start <- list(mu_opt = .8, temperature_xopt = 30)
#'
#' ## We can now call the fitting function
#'
#' set.seed(12412)
#' global_MCMC <- fit_multiple_growth_MCMC(start, multiple_experiments, known, sec_names, niter = 1000,
#'    lower = c(.2, 29),  # lower limits of the model parameters
#'    upper = c(.8, 34))  # upper limits of the model parameters
#'
#' ## Parameter estimates can be retrieved with summary
#'
#' summary(global_MCMC)
#'
#' ## We can compare fitted model against observations
#'
#' plot(global_MCMC)
#'
#' ## Any single environmental factor can be added to the plot using add_factor
#'
#' plot(global_MCMC, add_factor = "temperature")
#' }
#'
fit_multiple_growth_MCMC <- function(starting_point, experiment_data,
                                known_pars, sec_model_names, niter,
                                ...) {

    ## Fit the model

    my_fit <- modMCMC(get_multi_dyna_residuals, unlist(starting_point),
                     experiment_data = experiment_data,
                     known_pars = unlist(known_pars),
                     sec_model_names = sec_model_names,
                     niter = niter,
                     ...)

    #- Output the results

    pars_fit <- my_fit$bestpar

    primary_pars <- extract_primary_pars(pars_fit, known_pars)

    secondary_models <- extract_secondary_pars(pars_fit, known_pars,
                                               sec_model_names)

    best_predictions <- lapply(experiment_data, function(each_experiment) {

        times <- seq(0, max(each_experiment$data$time), length=100)

        best_prediction <- predict_dynamic_growth(times, each_experiment$conditions,
                                                  as.list(primary_pars),
                                                  secondary_models)

    })

    out <- list(fit_results = my_fit,
                best_prediction = best_predictions,
                data = experiment_data,
                starting = starting_point,
                known = known_pars,
                sec_models = sec_model_names
    )

    class(out) <- c("FitMultipleGrowthMCMC", class(out))
    return(out)

}












#' Residuals of dynamic prediction
#'
#' Function for calculating residuals of a dynamic prediction according
#' to the requirements of \code{\link{modFit}}.
#'
#' @param this_p named vector of model parameters
#' @param fit_data tibble with the data for the fit
#' @param env_conditions tibble with the environmental conditions
#' @param known_pars named vector of known model parameters
#' @param sec_model_names named character vector with names the
#' environmental conditions and values the secondary model (e.g. 'CPM').
#' @param cost an instance of modCost to be combined (to fit multiple models).
#'
#' @return An instance of \code{\link{modCost}}.
#'
#' @importFrom FME modCost
#'
get_dyna_residuals <- function(this_p, fit_data, env_conditions,
                               known_pars, sec_model_names,
                               cost = NULL
                               ) {

    ## Build the parameters of the primary model

    primary_pars <- extract_primary_pars(this_p, known_pars)

    ## Build the parameters of the secondary model

    secondary_models <- extract_secondary_pars(this_p, known_pars,
                                               sec_model_names)

    ## Calculate the prediction

    times <- seq(0, max(fit_data$time), length = 1000)

    prediction <- predict_dynamic_growth(times, env_conditions, as.list(primary_pars),
                                         secondary_models)

    ## Calculate residuals

    modCost(model = as.data.frame(prediction$simulation),
            obs = as.data.frame(fit_data),
            cost = cost
            )

}

#' Fit dynamic growth models
#'
#' Fits a growth model to a data obtained under dynamic conditions
#' using the one-step approach (non-linear regression).
#'
#' @param fit_data Tibble with the data to use for model fit. It must
#' contain a column named 'time' with the storage time and another named
#' 'logN' with the observed microbial count.
#' @param env_conditions Tibble with the (dynamic) environmental conditions
#' during the experiment. It must have one column named 'time' with the
#' storage time and as many columns as required with the environmental conditions.
#' Note that only those defined in 'sec_model_names' will be considered for
#' the model fit.
#' @param starting_point A named vector of starting values for the model parameters.
#' Parameters for the primary model must be named in the usual way. Parameters for the
#' secondary model are named as \code{env_factor}+'_'+\code{parameter}. For instance,
#' the maximum growth temperature shall be named 'temperature_xmax'.
#' @param known_pars A named vectors of known model parameters (i.e. not fitted). They
#' must be named using the same convention as for \code{starting_point}.
#' @param sec_model_names A named character vector defining the secondary model for each
#' environmental factor. The names define the factor and the value the type of model.
#' Names must match columns in \code{fit_data} and \code{env_conditions}.
#' @param ... Additional arguments passed to modFit.
#'
#' @return A list of class FitDynamicGrowth with the following items:
#'      \itemize{
#'          \item fit_results: the object returned by \code{modFit}.
#'          \item best_prediction: the model prediction for the fitted parameters.
#'          \item data: data used for the fit.
#'          \item starting: starting values for model fitting
#'          \item known: parameter values set as known.
#'          \item sec_models: a named vector with the secondary model
#'          for each environmental factor
#'          }
#'
#' @importFrom FME modFit
#'
#' @export
#'
#' @examples
#' \donttest{
#' ## We use the datasets included in the package
#'
#' data("example_dynamic_growth")
#' data("example_env_conditions")
#'
#' ## Define the secondary models
#'
#' sec_model_names <- c(temperature = "CPM", aw= "CPM")
#'
#' ## Any model parameter can be fixed
#'
#' known_pars <- list(Nmax = 1e4,  # Primary model
#'     N0 = 1e0, Q0 = 1e-3,  # Initial values of the primary model
#'     mu_opt = 4, # mu_opt of the gamma model
#'     temperature_n = 1,  # Secondary model for temperature
#'     aw_xmax = 1, aw_xmin = .9, aw_n = 1  # Secondary model for water activity
#'     )
#'
#' ## The remaining parameters need initial values
#'
#' my_start <- list(temperature_xmin = 25, temperature_xopt = 35,
#'     temperature_xmax = 40, aw_xopt = .95)
#'
#' ## We can now call the fitting function
#'
#' my_dyna_fit <- fit_dynamic_growth(example_dynamic_growth, example_env_conditions,
#'     my_start, known_pars, sec_model_names)
#'
#' summary(my_dyna_fit)
#'
#' ## We can compare the data and the fitted curve
#'
#' plot(my_dyna_fit)
#'
#' ## We can plot any environmental condition using add_factor
#'
#' plot(my_dyna_fit, add_factor = "aw",
#'     label_y1 = "Log count (log CFU/ml)",
#'     label_y2 = "Water activity")
#' }
#'
fit_dynamic_growth <- function(fit_data, env_conditions,
                               starting_point, known_pars,
                               sec_model_names, ...) {

    my_fit <- modFit(get_dyna_residuals, unlist(starting_point),
                     fit_data = fit_data,
                     env_conditions = env_conditions,
                     known_pars = unlist(known_pars),
                     sec_model_names = sec_model_names,
                     ...)

    #- Output the results

    pars_fit <- my_fit$par

    primary_pars <- extract_primary_pars(pars_fit, known_pars)

    secondary_models <- extract_secondary_pars(pars_fit, known_pars,
                                               sec_model_names)



    times <- seq(0, max(fit_data$time), length=100)

    best_prediction <- predict_dynamic_growth(times, env_conditions,
                                              as.list(primary_pars),
                                              secondary_models)

    out <- list(fit_results = my_fit,
                best_prediction = best_prediction,
                data = fit_data,
                starting = starting_point,
                known = known_pars,
                sec_models = sec_model_names
                )

    class(out) <- c("FitDynamicGrowth", class(out))
    return(out)

    }

#' Fit growth models using MCMC
#'
#' Fits a growth model to a data obtained under dynamic conditions
#' using the one-step approach (MCMC algorithm).
#'
#' @inheritParams fit_dynamic_growth
#'
#' @param niter number of iterations of the MCMC algorithm.
#'
#' @return A list of class 'FitDynamicGrowthMCMC' with the following items:
#'      \itemize{
#'          \item fit_results: the object returned by \code{modMCMC}.
#'          \item best_prediction: the model prediction for the fitted parameters.
#'          \item data: data used for the fit.
#'          \item starting: starting values for model fitting
#'          \item known: parameter values set as known.
#'          \item sec_models: a named vector with the secondary model
#'          for each environmental factor
#'          }
#'
#' @importFrom FME modMCMC
#'
#' @export
#'
#' @examples
#' \donttest{
#' ## We use the example data included in the package
#'
#' data("example_dynamic_growth")
#' data("example_env_conditions")
#'
#' ## Definition of the secondary models
#' sec_model_names <- c(temperature = "CPM", aw= "CPM")
#'
#' ## Any model parameter can be fixed
#' known_pars <- list(Nmax = 1e4,  # Primary model
#'     N0 = 1e0, Q0 = 1e-3,  # Initial values of the primary model
#'     mu_opt = 4, # mu_opt of the gamma model
#'     temperature_n = 1,  # Secondary model for temperature
#'     aw_xmax = 1, aw_xmin = .9, aw_n = 1  # Secondary model for water activity
#'     )
#'
#' ## We need starting values for the remaining parameters
#'
#' my_start <- list(temperature_xmin = 25, temperature_xopt = 35,
#'     temperature_xmax = 40,
#'     aw_xopt = .95)
#'
#' ## We can now call the fitting function
#'
#' set.seed(12124) # Setting seed for repeatability
#'
#' my_MCMC_fit <- fit_MCMC_growth(example_dynamic_growth, example_env_conditions,
#'     my_start, known_pars, sec_model_names, niter = 3000)
#'
#' ## Always check the MCMC chain!!
#'
#' plot(my_MCMC_fit$fit_results)
#'
#' ## We can compare data against fitted curve
#'
#' plot(my_MCMC_fit)
#'
#' ## Any environmental factor can be included using add_factor
#'
#' plot(my_MCMC_fit, add_factor = "temperature",
#'     label_y1 = "Count (log CFU/ml)", label_y2 = "Temperature (C)")
#'
#' }
#'
fit_MCMC_growth <- function(fit_data, env_conditions,
                            starting_point, known_pars,
                            sec_model_names, niter, ...) {

    my_fit <- modMCMC(get_dyna_residuals, unlist(starting_point),
                     fit_data = fit_data,
                     env_conditions = env_conditions,
                     known_pars = unlist(known_pars),
                     sec_model_names = sec_model_names,
                     niter = niter,
                     ...)

    #- Output the results

    pars_fit <- my_fit$bestpar

    primary_pars <- extract_primary_pars(pars_fit, known_pars)

    secondary_models <- extract_secondary_pars(pars_fit, known_pars,
                                               sec_model_names)



    times <- seq(0, max(fit_data$time), length=100)

    best_prediction <- predict_dynamic_growth(times, env_conditions,
                                              as.list(primary_pars),
                                              secondary_models)

    out <- list(fit_results = my_fit,
                best_prediction = best_prediction,
                data = fit_data,
                starting = starting_point,
                known = known_pars,
                sec_models = sec_model_names
    )

    class(out) <- c("FitDynamicGrowthMCMC", class(out))
    return(out)

}




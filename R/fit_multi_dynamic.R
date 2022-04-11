

#' Residuals of multiple dynamic predictions
#'
#' Function for calculating residuals of dynamic predictions under
#' different conditions for the same model parameters according
#' to the requirements of [modFit()].
#'
#' @param this_p named vector of model parameters
#' @param known_pars named vector of known model parameters
#' @param sec_model_names named character vector with names the
#' environmental conditions and values the secondary model (see secondary_model_data).
#' @param experiment_data a nested list with the experimental data. Each entry describes
#' one experiment as a list with two elements: data and conditions. `data` is a tibble
#' with two columns: time and logN. `conditions` is a tibble with one column named time
#' and as many additional columns as environmental factors.
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, 10 (i.e. log10). See vignette about units for details. 
#'
#' @return an instance of `modCost`.
#'
#' @importFrom FME modCost
#'
get_multi_dyna_residuals <- function(this_p, experiment_data,
                                     known_pars, sec_model_names,
                                     logbase_mu = logbase_logN,
                                     logbase_logN = 10) {

    old_cost <- NULL

    for (each_experiment in experiment_data) {

            my_cost <- get_dyna_residuals(unlist(this_p), each_experiment$data,
                                          each_experiment$conditions,
                                          unlist(known_pars), sec_model_names,
                                          cost = old_cost,
                                          logbase_mu = logbase_mu,
                                          logbase_logN = logbase_logN
                                          )

            old_cost <- my_cost

    }

    my_cost

}

#' Fitting growth models to multiple dynamic experiments
#' 
#' @description 
#' `r lifecycle::badge("superseded")`
#' 
#' The function [fit_multiple_growth()] has been superseded by the top-level
#' function [fit_growth()], which provides a unified approach for growth modelling.
#'
#' But, if you so wish, this functions still enables fitting a growth model using a dataset comprised of
#' several experiments with potentially different dynamic experimental conditions.
#' Note that the definition of secondary models must comply with the
#' `secondary_model_data` function.
#'
#' @inheritParams get_multi_dyna_residuals
#' @param starting_point a named vector of starting values for the model parameters.
#' @param ... additional arguments for [modFit()].
#' @param check Whether to check the validity of the models. `TRUE` by default.
#' @param experiment_data a nested list with the experimental data. Each entry describes
#' one experiment as a list with two elements: data and conditions. `data` is a tibble
#' with a column giving the elapsed time (named "time" by default) and another one
#' with the decimal logarithm of the population size (named "logN" by default).
#' `conditions` is a tibble with one column giving the elapsed time (using the
#' same name as `data`) and as many additional columns as environmental factors.
#' The default column names can be changed with the formula argument. 
#' @param formula an object of class "formula" describing the x and y variables.
#' `logN ~ time` as a default.
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, 10 (i.e. log10). See vignette about units for details. 
#'
#' @importFrom FME modFit
#' @importFrom formula.tools lhs rhs get.vars
#' @importFrom dplyr rename
#' @importFrom formula.tools lhs rhs get.vars
#' 
#' @return An instance of [FitMultipleDynamicGrowth()].
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
#'}
#'
fit_multiple_growth <- function(starting_point, experiment_data,
                                known_pars, sec_model_names,
                                ..., check = TRUE,
                                formula = logN ~ time,
                                logbase_mu = logbase_logN,
                                logbase_logN = 10
                                ) {
    
    ## Check the model parameters
    
    if (isTRUE(check)) {
        
        check_secondary_pars(starting_point, known_pars, sec_model_names,
                             primary_pars = c("mu_opt", "N0", "Nmax", "Q0"))
        
    }
    
    ## Apply the formula
    
    if (length(get.vars(formula)) > 2) {
        stop("Only formulas with 2 terms are supported.")
    }
    
    y_col <- lhs(formula)
    x_col <- rhs(formula)
    
    for (i in 1:length(experiment_data)) {
        
        experiment_data[[i]]$data <- experiment_data[[i]]$data %>%
            select(time = x_col, logN = y_col)
        
        experiment_data[[i]]$conditions <- experiment_data[[i]]$conditions %>%
            rename(time = x_col)
    }

    ## Fit the model

    my_fit <- modFit(get_multi_dyna_residuals, unlist(starting_point),
                     experiment_data = experiment_data,
                     known_pars = unlist(known_pars),
                     sec_model_names = sec_model_names,
                     logbase_mu = logbase_mu,
                     logbase_logN = logbase_logN,
                     ...)

    #- Output the results

    pars_fit <- my_fit$par

    primary_pars <- extract_primary_pars(pars_fit, known_pars)

    secondary_models <- extract_secondary_pars(pars_fit, known_pars,
                                               sec_model_names)

    best_predictions <- lapply(experiment_data, function(each_experiment) {

        times <- seq(0, max(each_experiment$data$time), length=100)
        
        best_prediction <- predict_growth(environment = "dynamic",
                                          times,
                                          as.list(primary_pars),
                                          secondary_models,
                                          each_experiment$conditions,
                                          logbase_mu = logbase_mu,
                                          logbase_logN = logbase_logN
        )

        # best_prediction <- predict_dynamic_growth(times, each_experiment$conditions,
        #                                           as.list(primary_pars),
        #                                           secondary_models
        #                                           )

    })

    names(best_predictions) <- names(experiment_data)

    out <- list(fit_results = my_fit,
                best_prediction = best_predictions,
                data = experiment_data,
                starting = starting_point,
                known = known_pars,
                sec_models = sec_model_names,
                logbase_mu = logbase_mu,
                logbase_logN = logbase_logN
    )

    class(out) <- c("FitMultipleDynamicGrowth", class(out))
    return(out)

}


#' Fitting growth models to multiple dynamic experiments using MCMC
#' 
#' @description 
#' `r lifecycle::badge("superseded")`
#' 
#' The function [fit_multiple_growth_MCMC()] has been superseded by the top-level
#' function [fit_growth()], which provides a unified approach for growth modelling.
#'
#' However, this functions can still be used to fit a growth model using a dataset comprised of
#' several experiments with potentially different dynamic experimental conditions.
#'
#' @inheritParams fit_multiple_growth
#' @param ... additional arguments for [modMCMC] (e.g. upper and lower bounds).
#' @param niter number of samples of the MCMC algorithm.
#'
#' @return An instance of [FitMultipleGrowthMCMC()].
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
                                ..., 
                                check = TRUE,
                                formula = logN ~ time,
                                logbase_mu = logbase_logN,
                                logbase_logN = 10
                                ) {

    ## Check the model parameters
    
    if (isTRUE(check)) {
        
        check_secondary_pars(starting_point, known_pars, sec_model_names,
                             primary_pars = c("mu_opt", "N0", "Nmax", "Q0"))
        
    }
    
    ## Apply the formula
    
    if (length(get.vars(formula)) > 2) {
        stop("Only formulas with 2 terms are supported.")
    }
    
    y_col <- lhs(formula)
    x_col <- rhs(formula)
    
    for (i in 1:length(experiment_data)) {
        
        experiment_data[[i]]$data <- experiment_data[[i]]$data %>%
            select(time = x_col, logN = y_col)
        
        experiment_data[[i]]$conditions <- experiment_data[[i]]$conditions %>%
            rename(time = x_col)
    }
    
    ## Fit the model

    my_fit <- modMCMC(get_multi_dyna_residuals, unlist(starting_point),
                     experiment_data = experiment_data,
                     known_pars = unlist(known_pars),
                     sec_model_names = sec_model_names,
                     niter = niter,
                     logbase_mu = logbase_mu,
                     logbase_logN = logbase_logN,
                     ...)

    #- Output the results

    pars_fit <- my_fit$bestpar

    primary_pars <- extract_primary_pars(pars_fit, known_pars)

    secondary_models <- extract_secondary_pars(pars_fit, known_pars,
                                               sec_model_names)

    best_predictions <- lapply(experiment_data, function(each_experiment) {

        times <- seq(0, max(each_experiment$data$time), length=100)
        
        best_prediction <- predict_growth(environment = "dynamic",
                                          times,
                                          as.list(primary_pars),
                                          secondary_models,
                                          each_experiment$conditions,
                                          logbase_mu = logbase_mu,
                                          logbase_logN = logbase_logN
        )

        # best_prediction <- predict_dynamic_growth(times, each_experiment$conditions,
        #                                           as.list(primary_pars),
        #                                           secondary_models)

    })

    names(best_predictions) <- names(experiment_data)

    out <- list(fit_results = my_fit,
                best_prediction = best_predictions,
                data = experiment_data,
                starting = starting_point,
                known = known_pars,
                sec_models = sec_model_names,
                logbase_mu = logbase_mu,
                logbase_logN = logbase_logN
    )

    class(out) <- c("FitMultipleGrowthMCMC", class(out))
    return(out)

}










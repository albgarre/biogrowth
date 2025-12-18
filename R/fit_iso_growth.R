
#' Residuals of isothermal prediction
#'
#' @param this_p named vector of model parameters to fit
#' @param fit_data tibble with the data for the fit
#' @param model_name character defining the primary growth model
#' @param known_pars named vector of fixed model parameters
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, the same as logbase_logN. See vignette about units for details. 
#' @param logbase_logN Base of the logarithm for the population size. By default,
#' 10 (i.e. log10). See vignette about units for details.
#'
#' @return An instance of `modCost`.
#'
#' @importFrom FME modCost
#'
get_iso_residuals <- function(this_p, fit_data, model_name, known_pars,
                              logbase_mu = logbase_logN,
                              logbase_logN = 10 
                              ) {

    times <- sort(unique(fit_data$time))

    pars <- c(this_p, known_pars)
    
    my_model <- as.list(pars)
    my_model$model <- model_name
    
    predictions <- predict_growth(times, my_model, check = FALSE,
                                  logbase_mu = logbase_mu,
                                  logbase_logN = logbase_logN)

    modCost(model = as.data.frame(predictions$simulation),
            obs = as.data.frame(fit_data))

}


#' Fit primary growth models
#' 
#' @description 
#' `r lifecycle::badge("superseded")`
#' 
#' The function [fit_isothermal_growth()] has been superseded by the top-level
#' function [fit_growth()], which provides a unified approach for growth modelling.
#' 
#' Nonetheless, it can still fit a primary growth model to data obtained under static 
#' environmental conditions.
#'
#' @importFrom FME modFit
#' @importFrom formula.tools lhs rhs get.vars
#' @importFrom dplyr rename
#'
#' @param fit_data Tibble of data for the fit. It must have two columns, one with
#' the elapsed time (`time` by default) and another one with the decimal logarithm
#' of the populatoin size (`logN` by default). Different column names can be
#' defined using the `formula` argument. 
#' @param model_name Character defining the primary growth model
#' @param starting_point Named vector of initial values for the model parameters.
#' @param known_pars Named vector of known model parameters (not fitted).
#' @param ... Additional arguments passed to [FME::modFit()].
#' @param check Whether to do some basic checks (TRUE by default).
#' @param formula an object of class "formula" describing the x and y variables.
#' `logN ~ time` as a default.
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, the same as logbase_logN. See vignette about units for details. 
#' @param logbase_logN Base of the logarithm for the population size. By default,
#' 10 (i.e. log10). See vignette about units for details.
#'
#' @return An instance of [FitIsoGrowth()].
#'
#' @export
#'
#' @examples
#'
#' ## Some dummy data
#'
#' library(tibble)
#'
#' my_data <- tibble(time = c(0, 25, 50, 75, 100),
#'     logN = c(2, 2.5, 7, 8, 8))
#'
#' ## Choose the model
#'
#' my_model <- "Baranyi"
#'
#' ## Initial values for the model parameters
#'
#' start = c(logNmax = 8, lambda = 25, logN0 = 2)
#'
#' ## Any model parameter can be fixed
#'
#' known <- c(mu = .2)
#'
#' ## Now, we can call the function
#'
#' static_fit <- fit_isothermal_growth(my_data, my_model, start, known)
#'
#' summary(static_fit)
#'
#' ## We can plot the fitted model against the observations
#'
#' plot(static_fit)
#'
fit_isothermal_growth <- function(fit_data, model_name, starting_point,
                                  known_pars,
                                  ..., 
                                  check = TRUE,
                                  formula = logN ~ time,
                                  logbase_mu = logbase_logN,
                                  logbase_logN = 10  # TODO
                                  ) {

    ## Check the model parameters

    if (isTRUE(check)) {

        check_primary_pars(model_name, c(starting_point, known_pars))

    }
    
    ## Apply the formula
    
    if (length(get.vars(formula)) > 2) {
        stop("Only formulas with 2 terms are supported.")
    }
    
    y_col <- lhs(formula)
    x_col <- rhs(formula)
    
    fit_data <- select(fit_data, 
                       time = x_col,
                       logN = y_col
                       )

    ## Fit the model
    
    # browser()

    my_fit <- modFit(get_iso_residuals, unlist(starting_point),
                     fit_data = fit_data, model_name = model_name,
                     known_pars = known_pars,
                     logbase_mu = logbase_mu,
                     logbase_logN = logbase_logN,
                     ...)

    ## Prepare the output
    
    # browser()

    times <- seq(0, max(fit_data$time), length = 1000)
    pars <- c(my_fit$par, known_pars)
    
    my_model <- as.list(pars)
    my_model$model <- model_name
    
    best_prediction <- predict_growth(times, my_model, check = FALSE,
                                      logbase_mu = logbase_mu,
                                      logbase_logN = logbase_logN)

    # best_prediction <- predict_isothermal_growth(model_name, times, as.list(pars), check=FALSE)

    out <- list(
        data = fit_data,
        model = model_name,
        starting_point = starting_point,
        known = known_pars,
        fit = my_fit,
        best_prediction = best_prediction,
        logbase_mu = logbase_mu,
        logbase_logN = logbase_logN
    )

    class(out) <- c("FitIsoGrowth", class(out))
    out

}










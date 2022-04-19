
#' Prediction of microbial growth
#' 
#' @description 
#' `r lifecycle::badge("stable")`
#' 
#' This function provides a top-level interface for predicting population growth. 
#' Predictions can be made either under constant or dynamic environmental conditions. 
#' See below for details on the calculations.
#' 
#' @details 
#' To ease data input, the functions can convert between parameters defined in different
#' scales. Namely, for predictions in constant environments (environment="constant"):
#' * "logN0" can be defined as "N0". The function automatically calculates the log-transformation.
#' * "logNmax" can be defined as "Nmax". The function automatically calculates the log-transformation.
#' * "mu" can be defined as "mu_opt". The function assumes the prediction is under optimal growth conditions.
#' * "lambda" can be defined by "Q0". The duration of the lag phase is calculated using [Q0_to_lambda()].
#' 
#' And, for predictions in dynamic environments (environment="dynamic"):
#' * "N0" can be defined as "N0". The function automatically calculates the antilog-transformation.
#' * "Nmax" can be defined as "logNmax". The function automatically calculates the antilog-transformation.
#' * "mu" can be defined as "mu_opt". The function assumes mu was calculated under optimal growth conditions.
#' * "Q0" can be defined by the value of "lambda" under dynamic conditions. Then, the 
#' value of Q0 is calculated using [lambda_to_Q0()].
#' 
#' @section Predictions in constant environments:
#' Predictions under constant environments are calculated using only primary models. 
#' Consequently, the arguments "secondary_models" and "env_conditions" are ignored.
#' If these were passed, the function would return a warning. In this case, 
#' predictions are calculated using the algebraic form of the primary model (see 
#' vignette for details).
#' 
#' The growth model is defined through the "primary_model" argument using a named list.
#' One of the list elements must be named "model" and must take take one of the valid 
#' keys returned by [primary_model_data()]. The remaining entries of the list define the
#' values of the parameters of the selected model. A list of valid keys can be retrieved
#' using [primary_model_data()] (see example below). Note that the functions can do
#' some operations to facilitate the compatibility between constant and dynamic environments
#' (see Details).
#' 
#' @section Predictions in dynamic environments:
#' Predictions under dynamic environments are calculated by solving numerically
#' the differential equation of the Baranyi growth model. The effect of
#' changes in the environmental conditions in the growth rate are calculated
#' according to the gamma approach. Therefore, one must define both primary 
#' and secondary models. 
#' 
#' The dynamic environmental conditions are defined using a tibble (or data.frame)
#' through the "env_conditions" argument. It must include one column named "time" stating the elapsed
#' time and as many additional columns as environmental conditions included in the prediction.
#' For values of time not included in the tibble, the values of the environmental conditions
#' are calculated by linear interpolation.
#' 
#' Primary models are defined as a named list through the "primary_model" argument. It
#' must include the following elements:
#' * N0: initial population size
#' * Nmax: maximum population size in the stationary growth phase
#' * mu_opt: growth rate under optimal growth conditions
#' * Q0: value defining the duration of the lag phase
#' Additional details on these parameters can be found in the package vignettes.
#' 
#' Secondary models are defined as a nested list through the "secondary_models" argument.
#' The list must have one entry per environmental condition, whose name must match 
#' those used in the "env_conditions" argument. Each of these entries must be a named list
#' defining the secondary model for each environmental condition. The model equation is defined
#' in an entry named "model" (valid keys can be retrieved from [secondary_model_data()]). Then,
#' additional entries defined the values of each model parameters (valid keys can be retrieved from [secondary_model_data()])
#' 
#' For additional details on how to define the secondary models, please see the package vignettes (and examples below).
#' 
#' 
#' @param times numeric vector of time points for making the predictions
#' @param primary_model  named list defining the values of the parameters of the primary growth model
#' @param environment type of environment. Either "constant" (default) or "dynamic" (see below for details 
#' on the calculations for each condition)
#' @param secondary_models a nested list describing the secondary models. See below for details
#' @param env_conditions Tibble describing the variation of the environmental
#' conditions for dynamic experiments. It must have with the elapsed time (named `time` 
#' by default; can be changed with the "formula" argument), 
#' and as many additional columns as environmental factors. Ignored for "constant" environments.
#' @param ... Additional arguments for [ode()].
#' @param check Whether to check the validity of the models. `TRUE` by default.
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, the same as logbase_logN. See vignette about units for details. 
#' @param logbase_logN Base of the logarithm for the population size. By default,
#' 10 (i.e. log10). See vignette about units for details.
#' @param formula An object of class "formula" describing the x variable for predictions 
#' under dynamic conditions. `. ~ time` as a default.
#' 
#' @return For environment="constant", an instance of [IsothermalGrowth()] . 
#' For environment="dynamic", an instance of [DynamicGrowth()].
#' 
#' 
#' @export
#' 
#' @examples 
#' 
#' ## Example 1 - Growth under constant conditions -----------------------------
#' 
#' ## Valid model keys can be retrieved calling primary_model_data()
#' 
#' primary_model_data()
#' 
#' my_model <- "modGompertz"  # we will use the modified-Gompertz
#' 
#' ## The keys of the model parameters can also be obtained from primary_model_data()
#' 
#' primary_model_data(my_model)$pars
#' 
#' ## We define the primary model as a list
#' 
#' my_model <- list(model = "modGompertz", logN0 = 0, C = 6, mu = .2, lambda = 20)
#' 
#' ## We can now make the predictions
#' 
#' my_time <- seq(0, 100, length = 1000)  # Vector of time points for the calculations
#' 
#' my_prediction <- predict_growth(my_time, my_model, environment = "constant")
#' 
#' ## The instance of IsothermalGrowth includes several S3 methods 
#' 
#' print(my_prediction)
#' plot(my_prediction)
#' coef(my_prediction)
#' 
#' ## Example 2 - Growth under dynamic conditions ------------------------------
#' 
#' ## We will consider the effect of two factors: temperature and pH
#' 
#' my_conditions <- data.frame(time = c(0, 5, 40),
#'                             temperature = c(20, 30, 35),
#'                             pH = c(7, 6.5, 5)
#'                             )
#'                             
#' ## The primary model is defined as a named list
#'                             
#' my_primary <- list(mu = 2, Nmax = 1e7, N0 = 1, Q0 = 1e-3)
#' 
#' ## The secondary model is defined independently for each factor
#' 
#' sec_temperature <- list(model = "Zwietering",
#'     xmin = 25, xopt = 35, n = 1)
#'
#' sec_pH = list(model = "CPM",
#'     xmin = 5.5, xopt = 6.5,
#'     xmax = 7.5, n = 2)
#'     
#' ## Then, they are assigned to each factor using a named list
#'
#' my_secondary <- list(
#'     temperature = sec_temperature,
#'     pH = sec_pH
#'     )
#'     
#' ## We can call the function now
#' 
#' my_times <- seq(0, 50, length = 1000)  # Where the output is calculated
#' 
#' dynamic_prediction <- predict_growth(environment = "dynamic", 
#'                                      my_times, my_primary, my_secondary,
#'                                      my_conditions
#'                                      )
#'                                      
#' ## The instance of DynamicGrowth includes several useful S3 methods
#' 
#' print(dynamic_prediction)
#' plot(dynamic_prediction)
#' plot(dynamic_prediction, add_factor = "pH")
#' coef(dynamic_prediction)
#' 
#' ## The time_to_size function can predict the time to reach a population size
#' 
#' time_to_size(my_prediction, 3)
#' 
#' 
predict_growth <- function(times,
                           primary_model,
                           environment = "constant",
                           secondary_models = NULL,
                           env_conditions = NULL,
                           ...,
                           check = TRUE,
                           logbase_mu = logbase_logN,
                           logbase_logN = 10,
                           formula = . ~ time
                           ) {
    
    ## This is just a top-level function. All the heavy lifting is still done in the superseded functions
    
    if (environment == "constant") {  # Predictions under constant environmental conditions

        
        ## Get the name of the primary model and remove it from the vector
        
        my_model <- primary_model$model
        my_pars <- primary_model
        my_pars$model <- NULL
        
        if (check) {
            
            if (is.null(my_model)) {
                stop("primary model must include a 'model' entry")
            }
            
        }
        
        ## Convert the parameters (to make both static and dynamic compatible)
        
        if( is.null(my_pars$logN0) ) {
            my_pars$logN0 <- log(my_pars$N0, base = logbase_logN)
            my_pars$N0 <- NULL
        }
        
        if( is.null(my_pars$logNmax) & is.null(my_pars$C) ) {
            my_pars$logNmax <- log(my_pars$Nmax, base = logbase_logN)
            my_pars$Nmax <- NULL
        }
        
        if( is.null(my_pars[["mu"]]) ) {
            my_pars$mu <- my_pars$mu_opt
            my_pars$mu_opt <- NULL
        }
        
        if( is.null(my_pars$lambda) ) {
            my_pars$lambda <- Q0_to_lambda(my_pars$Q0, my_pars$mu,
                                           logbase_mu = logbase_mu)
            my_pars$Q0 <- NULL
        }
        
        ## Give a warning if someone defined environmental conditions
        
        if (! is.null(env_conditions)) {
            warning("env_conditions are ignored for 'constant' predictions")
        }
        
        ## Give a warning if someone defined secondary models
        
        if (! is.null(secondary_models)) {
            warning("secondary_models are ignored for 'constant' predictions")
        }
        
        ## Call the superseded function
        
        out <- predict_isothermal_growth(my_model, times, my_pars, check = check,
                                         logbase_mu = logbase_mu,
                                         logbase_logN = logbase_logN)
        
        ## Overwrite the class
        
        class(out) <- c("GrowthPrediction", "list")
        
        ## Adapt the attributes for the new class
        
        out$primary_model <- primary_model
        out$environment <- "constant"
        out$env_conditions <- NULL  # only for dynamic
        out$sec_models <- NULL  # only for dynamic
        out$gammas <- NULL  # only for dynamic
        
        out$model <- NULL  # superseded by primary_model
        out$pars <- NULL  # superseded by primary_model
        
        ## Return
        
        out
        
    } else if(environment == "dynamic") {  # Prediction under dynamic environmental conditions
        
        ## Check arguments specific for dynamic conditions
        
        if (check) {
            
            if (is.null(env_conditions)) {
                stop("env_conditions must be defined for predictions in dynamic environments")
            }
            
            if (is.null(secondary_models)) {
                stop("secondary_models must be defined for predictions in dynamic environments")
            }
            
        }
        
        ## Check that times starts at 0. Give a warning otherwise
        
        if (min(times) != 0) {
            warning(paste("times does not start at t=0.",
                          "Be mindful that the calculation assumes that the first value of times indicates the initial time for the simulation (i.e., the time point where N = N0)",
                          "If this is not what you intend, just pass c(0, times)."
                          )
                    )
        }
        
        ## Convert the parameters of the primary model (to make both static and dynamic compatible)
        
        my_pars <- primary_model
        my_pars$model <- NULL
        
        if( is.null(my_pars$N0) ) {
            my_pars$N0 <- logbase_logN^my_pars$logN0
            my_pars$logN0 <- NULL
        }
        
        if( is.null(my_pars$Nmax)) {
            my_pars$Nmax <- logbase_logN^my_pars$logNmax
            my_pars$logNmax <- NULL
        }
        
        if( is.null(my_pars$mu_opt) ) {
            my_pars$mu_opt <- my_pars$mu
            my_pars$mu <- NULL
        }
        
        if( is.null(my_pars$Q0) ) {
            my_pars$Q0 <- lambda_to_Q0(my_pars$lambda, my_pars$mu_opt,
                                       logbase_mu = logbase_mu
                                       )  
            my_pars$lambda <- NULL
        }

        ## And call the superseded function
        
        out <- predict_dynamic_growth(times, 
                                      env_conditions, 
                                      my_pars,
                                      secondary_models, 
                                      check = check,
                                      formula = formula,
                                      logbase_mu = logbase_mu,
                                      logbase_logN = logbase_logN,
                                      ...)
        
        ## Overwrite the class
        
        class(out) <- c("GrowthPrediction", "list")
        
        ## Adjust the attributes for the new class
        
        out$primary_model <- primary_model
        out$environment <- "dynamic"
        
        out$primary_pars <- NULL  # superseded by primary_model
        
        ## Return
        
        out
        
        
    } else {  # Wrong input
        stop("environment must be 'constant' or 'dynamic', received: ", environment)
    }

}




















#' Time to reach a given microbial count
#' 
#' @description 
#' `r lifecycle::badge("superseded")`
#' 
#' The function [time_to_logcount()] has been superseded by 
#' function [time_to_size()], which provides a more general interface.
#'
#' But it still returns the storage time required for the microbial count to
#' reach `log_count` according to the predictions of `model`.
#' Calculations are done using linear interpolation of the model predictions.
#'
#' @param model An instance of `IsothermalGrowth` or `DynamicGrowth`.
#' @param log_count The target log microbial count.
#'
#' @importFrom stats approx
#'
#' @return The predicted time to reach `log_count`.
#'
#' @export
#'
#' @examples
#'
#' ## First of all, we will get an IsothermalGrowth object
#'
#' my_model <- "modGompertz"
#' my_pars <- list(logN0 = 2, C = 6, mu = .2, lambda = 25)
#' my_time <- seq(0, 100, length = 1000)
#'
#' static_prediction <- predict_isothermal_growth(my_model, my_time, my_pars)
#' plot(static_prediction)
#'
#' ## And now we calculate the time to reach a microbial count
#'
#' time_to_logcount(static_prediction, 2.5)
#'
#' ## If log_count is outside the range of the predicted values, NA is returned
#'
#' time_to_logcount(static_prediction, 20)
#'
#'
#'
time_to_logcount <- function(model, log_count) {
    
    

    if (is.IsothermalGrowth(model)) {
        
        approx(model$simulation$logN, model$simulation$time,
               log_count, 
               ties = function(x) min(x, na.rm = TRUE))$y
        
    } else if(is.DynamicGrowth(model)) {
        
        approx(model$simulation$logN, model$simulation$time,
               log_count, 
               ties = function(x) min(x, na.rm = TRUE))$y
        
    } else {
        
        stop("Model type not supported: ", class(model))
        
    }

}


#' Distribution of times to reach a certain microbial count
#' 
#' @description 
#' `r lifecycle::badge("superseded")`
#' 
#' The function [distribution_to_logcount()] has been superseded by 
#' function [time_to_size()], which provides more general interface.
#'
#' Returns the probability distribution of the storage time required for
#' the microbial count to reach `log_count` according to the predictions of
#' a stochastic `model`.
#' Calculations are done using linear interpolation of the individual
#'  model predictions.
#'
#' @param model An instance of `StochasticGrowth` or `MCMCgrowth`.
#' @param log_count The target microbial count.
#'
#' @return An instance of [TimeDistribution()].
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr %>% summarize
#' @importFrom rlang .data
#' @importFrom stats sd median quantile
#'
#' @export
#'
#' @examples
#' \donttest{
#' ## We need an instance of StochasticGrowth
#'
#' my_model <- "modGompertz"
#' my_times <- seq(0, 30, length = 100)
#' n_sims <- 3000
#' 
#' library(tibble)
#' 
#' pars <- tribble(
#'     ~par, ~mean, ~sd, ~scale,
#'     "logN0", 0, .2, "original",
#'     "mu", 2, .3, "sqrt",
#'     "lambda", 4, .4, "sqrt",
#'     "C", 6, .5, "original"
#' )
#' 
#' stoc_growth <- predict_stochastic_growth(my_model, my_times, n_sims, pars)
#'
#' ## We can now call the function
#'
#' time_distrib <- distribution_to_logcount(stoc_growth, 4)
#'
#' ## And plot the results
#'
#' plot(time_distrib)
#' }
#'
distribution_to_logcount <- function(model, log_count) {

    if (is.StochasticGrowth(model)) {

        time_dist <- split(model$simulations, model$simulations$iter) %>%
            # split(.$iter) %>%
            map_dfr(~ approx(.$logN, .$time, log_count, ties = "ordered")
            )

    } else if(is.MCMCgrowth(model)) {

        time_dist <- split(model$simulations, model$simulations$sim) %>%
            # split(.$sim) %>%
            map_dfr(~ approx(.$logN, .$time, log_count, ties = "ordered")
            )

    } else {
        stop("Model type not supported: ", class(model))
    }

    my_summary <- time_dist %>%
        summarize(m_time = mean(.data$y, na.rm = TRUE),
                  sd_time = sd(.data$y, na.rm=TRUE),
                  med_time = median(.data$y, na.rm = TRUE),
                  q10 = quantile(.data$y, .1, na.rm = TRUE),
                  q90 = quantile(.data$y, .9, na.rm = TRUE))

    out <- list(distribution = time_dist$y,
                summary = my_summary)

    class(out) <- c("TimeDistribution", class(out))

    out

}

#' Time for the population to reach a given size 
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' Calculates the elapsed time required for the population to reach a given size
#' (in log scale)
#' 
#' @details 
#' The calculation method differs depending on the type of object passed. If the
#' model contains a deterministic prediction ([IsothermalGrowth] or [DynamicGrowth]), 
#' the time to reach the given population size is estimated by linear interpolation and
#' the function a number. 
#' 
#' If, on the other hand, a stochastic prediction is passed ([StochasticGrowth],
#' [MCMCgrowth]), the time to reach "size" is estimated for each Monte Carlo iteration.
#' Then, the function returns an instance of [TimeDistribution], which contains
#' the distribution of times.
#' 
#' @param model An instance of [IsothermalGrowth], [DynamicGrowth], [StochasticGrowth]
#' or [MCMCgrowth]
#' @param size Target population size (in log scale)
#' 
#' @importFrom purrr map_dbl
#' 
#' @return If "model" is [IsothermalGrowth] or [DynamicGrowth], a number. If "model"
#' is [StochasticGrowth] or [MCMCgrowth], an instance of [TimeDistribution]
#' 
#' @export
#' 
#' @examples 
#' 
#' ## Example 1 - Growth predictions -------------------------------------------
#' 
#' ## The model is defined as usual with predict_growth
#' 
#' my_model <- list(model = "modGompertz", logN0 = 0, C = 6, mu = .2, lambda = 20)
#' 
#' my_time <- seq(0, 100, length = 1000)  # Vector of time points for the calculations
#' 
#' my_prediction <- predict_growth(my_time, my_model, environment = "constant")
#' 
#' plot(my_prediction)
#' 
#' ## We just have to pass the model and the size (in log10)
#' 
#' time_to_size(my_prediction, 3)
#' 
#' ## If the size is not reached, it returns NA
#' 
#' time_to_size(my_prediction, 8)
#' 
#' ## Example 2 - Model fit ----------------------------------------------------
#' 
#' my_data <- data.frame(time = c(0, 25, 50, 75, 100), 
#'                       logN = c(2, 2.5, 7, 8, 8))
#'                       
#' models <- list(primary = "Baranyi")
#' 
#' known <- c(mu = .2)
#' 
#' start <- c(logNmax = 8, lambda = 25, logN0 = 2)
#' 
#' primary_fit <- fit_growth(my_data, models, start, known,
#'                           environment = "constant",
#'                           )
#'                           
#' plot(primary_fit)
#' 
#' time_to_size(primary_fit, 4)
#' 
#' ## Example 3 - Global fitting -----------------------------------------------
#' 
#' ## We need a model first
#' 
#' data("multiple_counts")
#' data("multiple_conditions")
#' 
#' sec_models <- list(temperature = "CPM", pH = "CPM")
#' 
#' known_pars <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
#'                    temperature_n = 2, temperature_xmin = 20, 
#'                    temperature_xmax = 35,
#'                    pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5)
#'                    
#' my_start <- list(mu_opt = .8, temperature_xopt = 30)
#' 
#' global_fit <- fit_growth(multiple_counts, 
#'                          sec_models, 
#'                          my_start, 
#'                          known_pars,
#'                          environment = "dynamic",
#'                          algorithm = "regression",
#'                          approach = "global",
#'                          env_conditions = multiple_conditions
#'                          ) 
#'                          
#' plot(global_fit)
#' 
#' ## The function calculates the time for each experiment
#' 
#' time_to_size(global_fit, 3)
#' 
#' ## It returns NA for the particular experiment if the size is not reached
#' 
#' time_to_size(global_fit, 4.5)
#' 
time_to_size <- function(model, 
                         size, 
                         type = "discrete"
                         ) {
    
    if (type == "discrete") {  # Calculation of a discrete time ----------------
        
        if (is.GrowthPrediction(model)) {  # For model predictions
            
            d <- model$simulation
            
            approx(x = d$logN, y = d$time, size, 
                   ties = function(x) min(x, na.rm = TRUE))$y
            
        } else if (is.GrowthFit(model)) {  # For model fits using the single approach
            
            d <- model$best_prediction$simulation
            
            approx(x = d$logN, y = d$time, size, 
                   ties = function(x) min(x, na.rm = TRUE))$y
            
        } else if (is.GlobalGrowthFit(model)) {  # For global fits

            model$best_prediction %>%
                map_dbl(~ approx(x = .$simulation$logN, 
                                 y = .$simulation$time, 
                                 size, 
                                 ties = function(x) min(x, na.rm = TRUE))$y
                        )
            
        } else if (is.GrowthUncertainty(model)) {  # For growth with uncertainty
            
            d <- model$quantiles
            
            approx(x = d$q50, y = d$time, size,  # Interpolate the median
                   ties = function(x) min(x, na.rm = TRUE))$y
            
        } else if (is.MCMCgrowth(model)) { 
            
            d <- model$quantiles
            
            approx(x = d$q50, y = d$time, size,  # Interpolate the median
                   ties = function(x) min(x, na.rm = TRUE))$y
            
        } else {
            stop("Model type not supported for discrete calculations: ", class(model))
        }
        
    } else if (type == "distribution") {  # Calculation of the distribution ----
        
        if (is.GrowthUncertainty(model)) {  # Predictions with parameter uncertainty
            
            time_dist <- split(model$simulations, model$simulations$iter) %>%
                map_dfr(~ approx(.$logN, .$time, size, 
                                 ties = function(x) min(x, na.rm = TRUE))
                )
            
        } else if (is.MCMCgrowth(model)) {
            
            time_dist <- split(model$simulations, model$simulations$sim) %>%
                map_dfr(~ approx(.$logN, .$time, size, 
                                 ties = function(x) min(x, na.rm = TRUE))
                )
            
        } else {
            stop("Model type not supported for calculating distributions: ", class(model))
        }
        
        ## Put the calculations together and return
        
        my_summary <- time_dist %>%
            summarize(m_time = mean(.data$y, na.rm = TRUE),
                      sd_time = sd(.data$y, na.rm=TRUE),
                      med_time = median(.data$y, na.rm = TRUE),
                      q10 = quantile(.data$y, .1, na.rm = TRUE),
                      q90 = quantile(.data$y, .9, na.rm = TRUE))
        
        out <- list(distribution = time_dist$y,
                    summary = my_summary)
        
        class(out) <- c("TimeDistribution", class(out))
        
        out
        
    } else {
        
        stop("type must be 'discrete' or 'distribution', not ", type)
        
    }

    
}

    
    
    
    
    
    
    
    
    
    



































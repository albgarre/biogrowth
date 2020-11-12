
#' Time to reach a given microbial count
#'
#' Returns the storage time required for the microbial count to
#' reach \code{log_count} according to the predictions of \code{model}.
#' Calculations are done using linear interpolation of the model predictions.
#'
#' @param model An instance of \code{IsothermalGrowth} or \code{DynamicGrowth}.
#' @param log_count The target log microbial count.
#'
#' @importFrom stats approx
#'
#' @return The predicted time to reach \code{log_count}.
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
               log_count)$y
    } else if(is.DynamicGrowth(model)) {
        approx(model$simulation$logN, model$simulation$time,
               log_count)$y
    } else {
        stop("Model not supported")
    }

}


#' Distribution of times to reach a certain microbial count
#'
#' Returns the probability distribution of the storage time required for
#' the microbial count to reach \code{log_count} according to the predictions of
#' a stochastic \code{model}.
#' Calculations are done using linear interpolation of the individual
#'  model predictions.
#'
#' @param model An instance of \code{StochasticGrowth} or \code{MCMCgrowth}.
#' @param log_count The target microbial count.
#'
#' @return A list of class \code{TimeDistribution} with the items:
#' \itemize{
#' \item distribution Sample of the distribution of times to reach \code{log_count}.
#' \item summary Summary statistics of distribution (mean, sd, median, q10 and q90).
#' }
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
#' my_model <- "Trilinear"
#' my_times <- seq(0, 30, length = 100)
#' n_sims <- 3000
#'
#' stoc_growth <- predict_stochastic_growth(my_model, my_times, n_sims,
#'     mean_logN0 = 0, sd_logN0 = .2,
#'     mean_sqmu = 2,sd_sqmu = .3,
#'     mean_sqlambda = 4, sd_sqlambda = .4,
#'     mean_logNmax = 6, sd_logNmax = .5)
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
        stop("Model not supported")
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





#' Time to reach a given microbial count
#'
#' @export
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
#' @importFrom purrr map_dfr
#' @importFrom dplyr %>%
#'
#' @export
#'
distribution_to_logcount <- function(model, log_count) {

    if (is.StochasticGrowth(model)) {

        time_dist <- model$simulations %>%
            split(.$iter) %>%
            map_dfr(.,
                    ~ approx(.$logN, .$time, log_count)
            )

    } else if(is.MCMCgrowth(model)) {

        time_dist <- model$simulations %>%
            split(.$sim) %>%
            map_dfr(.,
                    ~ approx(.$logN, .$time, log_count)
            )

    } else {
        stop("Model not supported")
    }

    my_summary <- time_dist %>%
        summarize(m_time = mean(y, na.rm = TRUE),
                  sd_time = sd(y, na.rm=TRUE),
                  med_time = median(y, na.rm = TRUE),
                  q10 = quantile(y, .1, na.rm = TRUE),
                  q90 = quantile(y, .9, na.rm = TRUE))

    out <- list(distribution = time_dist$y,
                summary = my_summary)

    class(out) <- c("TimeDistribution", class(out))

    out

}




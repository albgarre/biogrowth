
#' Isothermal growth with variability
#'
#' Stochastic simulation of microbial growth based on probability
#' distributions of the parameters of the primary model.
#'
#' Simulations are limited to multivariate normal distributions of
#' the model parameters.
#'
#' @param model_name Character describing the primary growth model.
#' @param times Numeric vector of storage times for the simulations
#' @param n_sims Number of simulations
#' @param mean_logN0 Mean value of the initial log microbial count.
#' @param sd_logN0 Standard error of the initial log microbial count.
#' @param mean_sqmu Mean value of the square root of the maximum
#' specific growth rate.
#' @param sd_sqmu Standard error of the square root of the maximum
#' specific growth rate.
#' @param mean_sqlambda Mean value of the square root of the lag phase duration.
#' @param sd_sqlambda Standard error of the square root of the lag phase duration.
#' @param mean_logNmax Mean value of the maximum log microbial count.
#' @param sd_logNmax Standard error of the maximum log microbial count.
#' @param corr_matrix Correlation matrix of the model parameters. Defined in the
#' order (logN0, sqrt(mu), sqrt(lambda), logNmax). A diagonal matrix by default
#' (uncorrelated parameters).
#'
#' @return A list of class \code{StochasticGrowth} with the items:
#' \itemize{
#' \item sample: parameter sample used for the calculations.
#' \item simulations: growth curves predicted for each parameter.
#' \item quantiles: limits of the credible intervals (5%, 10%, 50%, 90%, 95%) for
#' each time point.
#' \item model: Model used for the calculations.
#' \item mus: Mean parameter values used for the simulations.
#' \item sigma: Variance-covariance matrix used for the simulations.
#' }
#'
#'
#' @importFrom MASS mvrnorm
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom rlang set_names .data
#' @importFrom purrr map
#' @importFrom purrr imap_dfr
#'
#' @export
#'
predict_stochastic_growth <- function(model_name, times, n_sims,
                                      mean_logN0, sd_logN0,
                                      mean_sqmu, sd_sqmu,
                                      mean_sqlambda, sd_sqlambda,
                                      mean_logNmax, sd_logNmax,
                                      corr_matrix = diag(4)
                                      ) {

    ## Build the parameter sample

    mus <- c(mean_logN0, mean_sqmu, mean_sqlambda, mean_logNmax)
    stdevs <- c(sd_logN0, sd_sqmu, sd_sqlambda, sd_logNmax)
    b <- stdevs %*% t(stdevs)
    cov_matrix <- b * corr_matrix

    par_sample <- as.data.frame(mvrnorm(n_sims,
                                        mus,
                                        cov_matrix
                                        )
                                ) %>%
        set_names(c("logN0", "sq_mu", "sq_lambda", "logNmax"))

    ## Variable transformations

    par_sample <- par_sample %>%
        mutate(mu = .data$sq_mu^2,
               lambda = .data$sq_lambda^2) %>%
        select(-.data$sq_mu, -.data$sq_lambda)

    if (model_name == "modGompertz") {
        par_sample <- par_sample %>%
            mutate(C = .data$logNmax - .data$logN0) %>%
            select(-.data$logNmax)
    }

    ## Do the simulations

    aa <- par_sample %>%
        mutate(iter = row_number())

    my_sims <- split(aa, aa$iter) %>%
        # split(.$iter) %>%
        map(as.list) %>%
        map(~ predict_isothermal_growth(model_name, times, .)) %>%
        map(~ .$simulation) %>%
        imap_dfr(~ mutate(.x, iter = .y))

    ## Extract quantiles

    q_values <- my_sims %>%
        group_by(.data$time) %>%
        summarize(q50 = quantile(.data$logN, probs = .5, na.rm=TRUE),
                  q10 = quantile(.data$logN, probs = .1, na.rm=TRUE),
                  q90 = quantile(.data$logN, probs = .9, na.rm=TRUE),
                  q05 = quantile(.data$logN, probs = .05, na.rm=TRUE),
                  q95 = quantile(.data$logN, probs = .95, na.rm=TRUE),
                  m_logN= mean(.data$logN, na.rm=TRUE)
        )


    ## Output

    out <- list(sample = par_sample,
                simulations = my_sims,
                quantiles = q_values,
                model = model_name,
                mus = mus,
                sigma = cov_matrix)

    class(out) <- c("StochasticGrowth", class(out))

    out

}














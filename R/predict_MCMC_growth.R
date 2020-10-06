
#' Stochastic growth of MCMC fit
#'
#' Makes a stochastic prediction of microbial growth based on a growth model
#' fitted using \code{\link{fit_MCMC_growth}}. This function predicts growth
#' curves for \code{niter} samples (with replacement) of the samples of the
#' MCMC algorithm. Then, credible intervals are calculated based on the
#' quantiles of the model predictions at each time point.
#'
#' @param MCMCfit An instance of \code{FitDynamicGrowthMCMC}.
#' @param times Numeric vector of storage times for the predictions.
#' @param env_conditions Tibble with the (dynamic) environmental conditions
#' during the experiment. It must have one column named 'time' with the
#' storage time and as many columns as required with the environmental conditions.
#' @param niter Number of iterations.
#'
#' @return A list of class \code{MCMCgrowth} with items:
#' \itemize{
#' \item sample: Parameter sample used for the calculations.
#' \item simulations: Individual growth curves calculated based on the parameter
#' sample.
#' \item quantiles: Tibble with the limits of the credible intervals
#'  (5%, 10%, 50%, 90% and 95%) for each time point.
#' \item model: Instance of \code{FitDynamicGrowthMCMC} used for predictions.
#' }
#'
#'
#' @importFrom dplyr sample_n
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr group_by
#' @importFrom tibble as_tibble
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr imap_dfr
#' @importFrom rlang .data
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' \donttest{
#' ## We need a FitDynamicGrowthMCMC object
#'
#' data("example_dynamic_growth")
#' data("example_env_conditions")
#'
#' sec_model_names <- c(temperature = "CPM", aw= "CPM")
#'
#' known_pars <- list(Nmax = 1e4,  # Primary model
#'     N0 = 1e0, Q0 = 1e-3,  # Initial values of the primary model
#'     mu_opt = 4, # mu_opt of the gamma model
#'     temperature_n = 1,  # Secondary model for temperature
#'     aw_xmax = 1, aw_xmin = .9, aw_n = 1  # Secondary model for water activity
#'     )
#'
#' my_start <- list(temperature_xmin = 25, temperature_xopt = 35,
#'     temperature_xmax = 40,
#'     aw_xopt = .95)
#'
#' set.seed(12124) # Setting seed for repeatability
#'
#' my_MCMC_fit <- fit_MCMC_growth(example_dynamic_growth, example_env_conditions,
#'     my_start, known_pars, sec_model_names, niter = 3000)
#'
#' ## Define the conditions for the simulation
#'
#' my_times <- seq(0, 15, length = 5)
#' niter <- 3000
#'
#' my_MCMC_prediction <- predict_MCMC_growth(my_MCMC_fit,
#'     my_times,
#'     example_env_conditions, # It could be different from the one used for fitting
#'     niter)
#'
#' plot(my_MCMC_prediction)
#' }
#'
predict_MCMC_growth <- function(MCMCfit, times, env_conditions, niter) {

    ## Extract the parameters

    par_sample <- MCMCfit$fit_results$pars %>%
        as.data.frame() %>%
        as_tibble() %>%
        sample_n(niter, replace = TRUE) %>%
        mutate(iter = row_number())

    known_pars <- MCMCfit$known

    sec_model_names <- MCMCfit$sec_models

    ## Build the models

    primary_pars <- split(par_sample, par_sample$iter) %>%
        # split(.$iter) %>%
        map(as.list) %>%
        map(~ extract_primary_pars(., known_pars)
        )

    secondary_models <- split(par_sample, par_sample$iter) %>%
        # split(.$iter) %>%
        map(as.list) %>%
        map(~ extract_secondary_pars(., known_pars, sec_model_names)
        )

    ## Do the simulations

    simulations <- map2(primary_pars, secondary_models,
                        ~ predict_dynamic_growth(times, env_conditions,
                                                 as.list(.x),
                                                 .y)
                        ) %>%
        map(~.$simulation) %>%
        imap_dfr(~ mutate(.x, sim = .y))

    q_values <- simulations %>%
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
                simulations = simulations,
                quantiles = q_values,
                model = MCMCfit)

    class(out) <- c("MCMCgrowth", class(out))

    out


}













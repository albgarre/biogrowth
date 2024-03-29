
#' Stochastic growth of MCMC fit
#' 
#' @description 
#' `r lifecycle::badge("superseded")`
#' 
#' The function [predict_MCMC_growth()] has been superseded by [predictMCMC()]
#' S3 methods of the relevant classes. 
#'
#' Nonetheless, it can still make a prediction of microbial growth including
#' parameter uncertainty based on a growth model
#' fitted using [fit_MCMC_growth()] or [fit_multiple_growth_MCMC()].
#' This function predicts growth curves for `niter` samples (with replacement)
#' of the samples of the MCMC algorithm. Then, credible intervals are calculated based on the
#' quantiles of the model predictions at each time point.
#'
#' @param MCMCfit An instance of `FitDynamicGrowthMCMC` or
#' `FitMultipleGrowthMCMC`.
#' @param times Numeric vector of storage times for the predictions.
#' @param env_conditions Tibble with the (dynamic) environmental conditions
#' during the experiment. It must have one column named 'time' with the
#' storage time and as many columns as required with the environmental conditions.
#' @param niter Number of iterations.
#' @param newpars A named list defining new values for the some model parameters. 
#' The name must be the identifier of a model already included in the model. 
#' These parameters do not include variation, so defining a new value for a fitted
#' parameters "fixes" it. `NULL` by default (no new parameters).
#' @param formula A formula stating the column named defining the elapsed time in 
#' `env_conditions`. By default, . ~ time.
#'
#' @return An instance of [MCMCgrowth()].
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
#' my_times <- seq(0, 15, length = 50)
#' niter <- 2000
#' 
#' newpars <- list(N0 = 1e-1,  # A parameter that was fixed
#'                 temperature_xmax = 120  # A parameter that was fitted
#'                 )
#' 
#' 
#' ## Make the simulations
#'
#' my_MCMC_prediction <- predict_MCMC_growth(my_MCMC_fit,
#'     my_times,
#'     example_env_conditions, # It could be different from the one used for fitting
#'     niter,
#'     newpars)
#'     
#' ## We can plot the prediction interval
#'
#' plot(my_MCMC_prediction)
#' 
#' ## We can also get the quantiles at each time point
#' 
#' print(my_MCMC_prediction$quantiles)
#' }
#'
predict_MCMC_growth <- function(MCMCfit, 
                                times, 
                                env_conditions, 
                                niter,
                                newpars = NULL,
                                formula = . ~ time) {

    ## Extract the parameters

    par_sample <- MCMCfit$fit_results$pars %>%
        as.data.frame() %>%
        as_tibble() %>%
        sample_n(niter, replace = TRUE) %>%
        mutate(iter = row_number())

    known_pars <- MCMCfit$known

    sec_model_names <- MCMCfit$sec_models
    
    ## Set up the new parameters
    
    if (!is.null(newpars)) {
        
        for (each_par in names(newpars)) {
            
            if (each_par %in% names(par_sample)) {
                
                par_sample[[each_par]] <- newpars[[each_par]]
                
            } else if (each_par %in% names(known_pars)) {
                
                known_pars[[each_par]] <- newpars[[each_par]]
                
            } else {
                stop("Parameter not defined in the model: ", each_par)
            }
            
        }
        
    }

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
                                                 .y,
                                                 formula = formula)
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
                model = MCMCfit,
                env_conditions = env_conditions)

    class(out) <- c("MCMCgrowth", class(out))

    out


}













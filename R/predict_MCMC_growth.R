
#' Stochastic growth of MCMC fit
#'
#' @importFrom dplyr sample_n
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom tibble as_tibble
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr imap_dfr
#'
#' @export
#'
predict_MCMC_growth <- function(MCMCfit, times, env_conditions, niter) {

    ## Extract the parameters

    par_sample <- MCMCfit$fit_results$pars %>%
        as_tibble() %>%
        sample_n(niter, replace = TRUE) %>%
        mutate(iter = row_number())

    known_pars <- MCMCfit$known

    sec_model_names <- MCMCfit$sec_models

    ## Build the models

    primary_pars <- par_sample %>%
        split(.$iter) %>%
        map(as.list) %>%
        map(.,
            ~ extract_primary_pars(., known_pars)
        )

    secondary_models <- par_sample %>%
        split(.$iter) %>%
        map(as.list) %>%
        map(.,
            ~ extract_secondary_pars(., known_pars, sec_model_names)
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
        group_by(time) %>%
        summarize(q50 = quantile(logN, probs = .5, na.rm=TRUE),
                  q10 = quantile(logN, probs = .1, na.rm=TRUE),
                  q90 = quantile(logN, probs = .9, na.rm=TRUE),
                  q05 = quantile(logN, probs = .05, na.rm=TRUE),
                  q95 = quantile(logN, probs = .95, na.rm=TRUE),
                  m_logN= mean(logN, na.rm=TRUE)
        )

    ## Output

    out <- list(sample = par_sample,
                simulations = simulations,
                quantiles = q_values,
                model = MCMCfit)

    class(out) <- c("MCMCgrowth", class(out))

    out


}













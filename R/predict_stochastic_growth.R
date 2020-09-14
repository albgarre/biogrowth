
#' Isothermal growth with variability
#'
#' @importFrom MASS mvrnorm
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom rlang set_names
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
        mutate(mu = sq_mu^2,
               lambda = sq_lambda^2) %>%
        select(-sq_mu, -sq_lambda)

    if (model_name == "modGompertz") {
        par_sample <- par_sample %>%
            mutate(C = logNmax - logN0) %>%
            select(-logNmax)
    }

    ## Do the simulations

    my_sims <- par_sample %>%
        mutate(iter = row_number()) %>%
        split(.$iter) %>%
        map(as.list) %>%
        map(.,
            ~predict_isothermal_growth(model_name, times,
                                       .)
        ) %>%
        map(., ~.$simulation) %>%
        imap_dfr(~ mutate(.x, iter = .y))

    ## Extract quantiles

    q_values <- my_sims %>%
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
                simulations = my_sims,
                quantiles = q_values,
                model = model_name,
                mus = mus,
                sigma = cov_matrix)

    class(out) <- c("StochasticGrowth", class(out))

    out

}















#' Growth under dynamic conditions
#'
#' Predicts microbial growth under dynamic conditions based on the
#' Baranyi model (Baranyi and Roberts, 1994) and secondary models
#' based on the gamma concept (Zwietering et al. 1992).
#'
#' Model predictions are done by linear interpolation of the environmental
#' conditions defined in \code{env_conditions}.
#'
#' @param times Numeric vector of storage times to make the predictions
#' @param env_conditions Tibble describing the variation of the environmental
#' conditions during storage. It must have a column named \code{time} with the
#' storage time and as many additional columns as environmental factors.
#' @param primary_pars A named list defining the parameters of the primary model
#' and the initial values of the model variables. That is, with names \code{mu_opt},
#' \code{Nmax}, \code{N0}, \code{Q0}.
#' @param secondary_models A nested list describing the secondary models.
#' @param ... Additional arguments for \code{\link{ode}}.
#'
#' @return A list of class \code{DynamicGrowth} with items:
#' \itemize{
#' \item simulation: A tibble with the model prediction
#' \item gammas: A tibble with the value of each gamma factor for each
#' value of \code{times}.
#' \item env_conditions: A list of functions interpolating the environmental
#' conditions.
#' \item primary_pars: A list with the model parameters of the primary model.
#' \item sec_models: A nested list defining the secondary models.
#' }
#'
#'
#' @importFrom deSolve ode
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_cols
#'
#' @export
#'
predict_dynamic_growth <- function(times, env_conditions, primary_pars,
                                   secondary_models, ...) {

    ## Prepare stuff

    my_env <- approx_env(env_conditions)

    yini <- c(Q = primary_pars$Q0,
              N = primary_pars$N0)

    ## Make the simulation

    my_sim <- ode(yini, times, dBaranyi, primary_pars,
                  env_func = my_env,
                  sec_models = secondary_models,
                  ...
    ) %>%
        as.data.frame() %>%
        as_tibble() %>%
        mutate(logN = log10(N))

    ## Calculate the gammas

    gammas <- lapply(times, function(x) {

        calculate_gammas(x, my_env, secondary_models)

    })

    gammas <- do.call(rbind.data.frame, gammas)
    names(gammas) <- names(secondary_models)
    gammas <- bind_cols(time = times, gammas)


    ## Prepare the output

    out <- list(simulation = my_sim,
                gammas = as_tibble(gammas),
                env_conditions = my_env,
                primary_pars = primary_pars,
                sec_models = secondary_models)

    class(out) <- c("DynamicGrowth", class(out))

    out

}




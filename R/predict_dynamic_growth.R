
#' Growth under dynamic conditions
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
                                   secondary_models) {

    ## Prepare stuff

    my_env <- approx_env(env_conditions)

    yini <- c(Q = primary_pars$Q0,
              N = primary_pars$N0)

    ## Make the simulation

    my_sim <- ode(yini, times, dBaranyi, primary_pars,
                  env_func = my_env,
                  sec_models = secondary_models
    ) %>%
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



